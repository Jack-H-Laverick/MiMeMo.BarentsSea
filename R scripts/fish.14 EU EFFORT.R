
# Calculate absolute fishing effort by gear across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

GFW_mobile <- brick("./Objects/GFW.nc", varname = "REST_mobile_gear") %>%     # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)

GFW_static <- brick("./Objects/GFW.nc", varname = "REST_static_gear") %>%     # For each class of gear
  calc(mean, na.rm = T)

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                               # Drop unnecessary columns
  st_union() %>%                                                              # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                                # Drop holes so we don't accidentally lose fishing near Svalbard
  st_make_valid() %>%                                                         # Still some residual holes so make valid
  nngeo::st_remove_holes()                                                    # And drop again

gears <- read.csv("./Data/MiMeMo gears.csv")                                  # Load fishing gear classifications

EU <- rgdal::readOGR(dsn="./Data/EU fish/spatial_effort_2015-2018/") %>%      # Import EU effort shapefile
  st_as_sf() %>%                                                              # Convert to SF
  dplyr::select(year, quarter, ger_typ, rctngl_, ttfshdy) %>%                 # Drop some columns, ttfshdy is "total fishing days"
  rename(Gear_code = ger_typ)

EU_Arctic <- st_contains(Region_mask, EU, sparse = F) %>%                     # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU[.,] %>%                                                                  # Subset the Eu data spatially
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped")  

#### Locate EU effort inside habitat types ####

target <- dplyr::select(gears, Aggregated_gear) %>%                           # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears

tictoc::tic()
corrected_effort <- rownames_to_column(EU_Arctic, var = "EU_polygon") %>%      # Create a column to track each IMR region and gear combination
  split(f = list(.$EU_polygon)) %>%                                           # Isolate each shape for fast paralel processing
  future_map( ~ {bind_rows(.x, rename(st_sf(domain), geometry = domain)) %>%  # Bind the domain polygon to the object 
    st_intersection() %>%                                                     # Split the polygons by areas of overlap
    mutate(GFW = ifelse(Gear_type == "Mobile", exact_extract(GFW_mobile, ., fun = "sum"),          # Get the GFW fishing effort in each shape
                                               exact_extract(GFW_static, ., fun = "sum"))) %>% 
    st_drop_geometry() %>%                                                    # Drop geometry column for simplicity
    drop_na() %>%                                                             # Drop the model domain area which isn't under the fishing polygon
    mutate(weight = GFW/sum(GFW, na.rm = T)) %>%                              # Calculate the proportion each piece of fishing area represents
    filter(n.overlaps == 2)                                                   # Limit to the piece where domain and fishing overlapped
    }, .progress = T) %>%
  data.table::rbindlist() %>%                                                 # Bind each fishing feature back into a DF
  mutate(effort_contributions = ttfshdy*weight*24)                            # Scale fishing effort by area in the model domain
                                                                              # Convert days to hours
saveRDS(corrected_effort, "./Objects/EU corrected pixel fishing effort")      # Save

#### Summarise ####
  
summary <- group_by(corrected_effort, Aggregated_gear, year) %>%              # By year and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  drop_na() %>%                                                               # Drop unassigned gears
  summarise(effort = mean(effort, na.rm = TRUE)) %>%                          # Average for each gear across years
  right_join(target) %>%                                                      # Reintroduce unobserved gears
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects
tictoc::toc()

saveRDS(summary, "./Objects/EU absolute fishing effort")                      # Save

#### Visualise ####

library(ggnewscale)
library(stars)

star <- st_as_stars(GFW_mobile)    # convert GFW to stars objects for use in sf plots
star2 <- st_as_stars(GFW_static)

ggplot() +
  geom_sf(data = domain, fill = "yellow", colour = "black") +
  geom_sf(data = EU_Arctic, aes(fill = ttfshdy), alpha = 0.3, colour = NA) +
  new_scale("fill") +
  geom_stars(data=star) +
  scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt")
