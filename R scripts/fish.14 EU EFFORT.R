
# Calculate absolute fishing effort by gear across EU flags by gear and guild

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

domain <- readRDS("./Objects/Domains.rds") %>% 
  st_transform(crs = 4326)

gears <- read.csv("./Data/MiMeMo gears.csv")                                  # Load fishing gear classifications

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                        # Load fishing gear classifications
  dplyr::select(Guild, FAO)
  
EU_effort <- rgdal::readOGR(dsn="./Data/EU fish/spatial_effort_2015-2018/") %>% # Import EU effort shapefile
  st_as_sf() %>%                                                              # Convert to SF
  dplyr::select(year, quarter, ger_typ, rctngl_, ttfshdy, trgt_ss) %>%                 # Drop some columns, ttfshdy is "total fishing days"
  rename(Gear_code = ger_typ, FAO = trgt_ss)

#### Locate EU effort inside habitat types ####

EU_Arctic <- st_contains(Region_mask, EU_effort, sparse = F) %>%              # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_effort[.,] %>%                                                           # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>%                                  # Create a column to track each EU polygon
  st_intersection(domain) %>%                                                 # Split the EU polygons cross habitat types
  mutate(area_stat_rectangle = as.numeric(st_area(.))) %>%                    # Work out the area of all the pieces
  st_drop_geometry() %>%                                                      # Drop geometry column for simplicity
  drop_na(Shore) %>%                                                          # Drop shapes which fall outside of habitats
  group_by(EU_polygon) %>%                                                    # Per original EU polygon
  mutate(share = area_stat_rectangle / sum(area_stat_rectangle)) %>%          # Work out the proportion of the area in each piece split over habitats
  ungroup() %>% 
  mutate(effort_contributions = ttfshdy*share) %>%                           # Combine habitat labels
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped") %>% 
  left_join(guild)
  
  group_by(year, Gear_type, Aggregated_gear, Guild) %>%                       # By year, habitat and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  ungroup() %>% 
  drop_na() %>%                                                               # Drop unassigned gears
  mutate(effort = effort*24)                                                  # Convert days to hours

saveRDS(EU_Arctic, "./Objects/EU effort by gear and target guild.rds")
