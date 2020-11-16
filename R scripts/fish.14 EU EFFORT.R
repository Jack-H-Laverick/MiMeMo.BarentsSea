
# Calculate absolute fishing effort by gear across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326)

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                               # Drop unnecessary columns
  st_union() %>%                                                              # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                                # Drop holes so we don't accidentally lose fishing near Svalbard
  st_make_valid() %>%                                                         # Still some residual holes so make valid
  nngeo::st_remove_holes()                                                    # And drop again

gears <- read.csv("./Data/MiMeMo gears.csv")                                  # Load fishing gear classifications

EU_effort <- rgdal::readOGR(dsn="./Data/EU fish/spatial_effort_2015-2018/") %>% # Import EU effort shapefile
  st_as_sf() %>%                                                              # Convert to SF
  dplyr::select(year, quarter, ger_typ, rctngl_, ttfshdy, trgt_ss) %>%        # Drop some columns, ttfshdy is "total fishing days"
  rename(Gear_code = ger_typ, FAO = trgt_ss)

#### Locate EU effort inside habitat types ####

EU_Arctic <- st_contains(Region_mask, EU_effort, sparse = F) %>%              # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_effort[.,] %>%                                                           # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>%                                  # Create a column to track each EU polygon
  st_intersection(Domains) %>%                                                # Split the EU polygons cross habitat types
  mutate(area_stat_rectangle = as.numeric(st_area(.))) %>%                    # Work out the area of all the pieces
  st_drop_geometry() %>%                                                      # Drop geometry column for simplicity
  drop_na(Shore) %>%                                                          # Drop shapes which fall outside of habitats
  group_by(EU_polygon) %>%                                                    # Per original EU polygon
  mutate(share = area_stat_rectangle / sum(area_stat_rectangle)) %>%          # Work out the proportion of the area in each piece split over habitats
  ungroup() %>% 
  mutate(effort_contributions = ttfshdy*share) %>%                            # Combine habitat labels
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped") %>% 
  group_by(Aggregated_gear, year) %>%                                         # By year and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  drop_na() %>%                                                               # Drop unassigned gears
  summarise(effort = mean(effort, na.rm = TRUE)) %>%                          # Average for each gear across years
  mutate(effort = effort*24)                                                  # Convert days to hours

#saveRDS(EU_Arctic, "./Objects/EU absolute effort by gear.rds")


## test

library(furrr)
plan(multiprocess)

test <- st_contains(Region_mask, EU_effort, sparse = F) %>%                   # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_effort[.,] %>%                                                           # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>%                                  # Create a column to track each EU polygon
  split(f = list(.$EU_polygon)) %>%                                           # Isolate each shape for fast paralel processing
  future_map( ~ {bind_rows(.x, rename(st_sf(domain), geometry = domain)) %>%  # Bind the domain polygon to the object 
    st_intersection() %>%                                                     # Split the polygons by areas of overlap
    mutate(area_stat_rectangle = as.numeric(st_area(.))) %>%                  # Calculate the area of each piece
    st_drop_geometry() %>%                                                    # Drop geometry column for simplicity
    drop_na() %>%                                                             # Drop the model domain area which isn't under the fishing polygon
    mutate(weight = area_stat_rectangle/sum(area_stat_rectangle, na.rm = T)) %>% # Calculate the proportion each piece of fishing area represents
    filter(n.overlaps == 2)                                                   # Limit to the piece where domain and fishing overlapped
    }, .progress = T) %>%
  data.table::rbindlist() %>%                                                 # Bind each fishing feature back into a DF
  mutate(effort_contributions = ttfshdy*weight) %>%                           # Scale fishing effort by area in the model domain
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped") %>% 
  group_by(Aggregated_gear, year) %>%                                         # By year and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  drop_na() %>%                                                               # Drop unassigned gears
  summarise(effort = mean(effort, na.rm = TRUE)) %>%                          # Average for each gear across years
  mutate(effort = effort*24)                                                  # Convert days to hours

target <- dplyr::select(gears, Aggregated_gear) %>%                           # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears

Goal <- test %>%                                                              # Total fishing effort
  right_join(target) %>%                                                      # Reintroduce unobserved gears
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(Goal, "./Objects/EU absolute fishing effort")                         # Save

