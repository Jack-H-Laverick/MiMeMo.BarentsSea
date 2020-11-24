
#**# Plot a time series of IMR landings by guild and gear to check for stability

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%     # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

gear <- read.csv("./Data/MiMeMo gears.csv")                                   # Import gear names

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                        # Import guild names
  dplyr::select(Guild, IMR.code) %>%                                          # Limit to IMR system
  drop_na() %>%                                                               # Drop those without an IMR code
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(IMR.code) %>%                                                      # 1 duplicated IMR code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

Regions <- rgdal::readOGR(dsn="./Data/IMR/Regions/") %>%                      # Import IMR regions shapefile
  st_as_sf() %>%                                                              # Convert to SF
  st_join(Domains) %>%                                                        # Which regions are in the model domain?
  drop_na() %>%                                                               # Drop those outside
  dplyr::select(Region = havomr) %>%                                          # Select and rename region column
  distinct(Region)                                                            # Keep unique shapes

GFW_mobile <- brick("./Objects/GFW.nc", varname = "NOR_mobile_gear") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)  

GFW_static <- brick("./Objects/GFW.nc", varname = "NOR_static_gear") %>%      # For each class of gear
  calc(mean, na.rm = T)

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(RE = "character", HO = "character")) %>% # Overwriting default column types  
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Region", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>% # Set column names
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region, IMR.code, "Weight") %>% # Ditch unnecessary columns
  left_join(gear) %>%                                                         # Attach gear labels
  left_join(guild) %>%                                                        # Attach guild labels
  filter(Aggregated_gear != "Dropped", Region %in% Regions$Region,            # Limited to gears and regions of interest
         between(Year, 2011, 2019)) %>%                                       # To when the electronic reporting system started to the last complete year
  group_by(Aggregated_gear, Gear_type, Guild, Region, Year) %>%                         
  summarise(Effort = sum(Fishing_time, na.rm = T),                            # Total up effort within years
            Weight = sum(Weight, na.rm = T)/1000) %>%                         # Total up landings within years
  summarise(Effort = mean(Effort, na.rm = T),                                 # Then average across years
            Weight = mean(Weight, na.rm = T)) %>%                             
  ungroup() %>% 
  merge(Regions, .)                                                           # Add in sf geometries by IMR region

#### Correct IMR by overlap with model domain ####

Regions_GFW <- Regions %>% 
  mutate(mobile_total = exact_extract(GFW_mobile, ., fun = "sum"),            # Get all mobile fishing effort from GFW in an IMR region
         static_total = exact_extract(GFW_static, ., fun = "sum")) %>%        # This is the total effort to scale features to within a polygon
  st_drop_geometry()                                                          # Drop geometry for a non-spatial join

corrected_IMR <- rownames_to_column(IMR, var = "Feature") %>%                 # Create a column to track each polygon
  st_intersection(Domains) %>%                                                # Crop the IMR polygons to the model domain
  mutate(mobile_feature = exact_extract(GFW_mobile, ., fun = "sum"),          # Get the GFW fishing effort in each shape
         static_feature = exact_extract(GFW_static, ., fun = "sum")) %>%      # Depending on gear type
  left_join(Regions_GFW) %>%                                                  # Attach total GFW effort by IMR region
  mutate(GFW_Scale = case_when(Gear_type == "Mobile" ~ (mobile_feature)/mobile_total, # Depending on gear type
                               Gear_type == "Static" ~ (static_feature)/static_total)) %>%  # Get proportion of GFW effort from a region within a feature
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the region replace NA with 1 to not use this for scaling
  mutate(corrected_effort = Effort*GFW_Scale,                                 # Scale whole region effort per gear by the proportion of GFW activity by gear type in the model domain 
         corrected_weight = Weight*GFW_Scale)

saveRDS(corrected_IMR, "./Objects/IMR regional absolute effort and landings.rds")

#### Convert IMR landings to a matrix by guild and gear ####

landings_target <- expand.grid(Guild = unique(guild$Guild), 
                      Aggregated_gear = unique(gear$Aggregated_gear) )      # Get combinations of gear and guild

landings <- st_drop_geometry(corrected_IMR) %>% 
  group_by(Aggregated_gear, Guild) %>% 
  summarise(corrected_weight = sum(corrected_weight, na.rm = TRUE)) %>%     # Total fishing effort across regions
  ungroup() %>% 
  right_join(landings_target) %>%                                           # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the unneeded gear class
  replace_na(replace = list(corrected_weight = 0)) %>%                      # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = corrected_weight) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

saveRDS(landings, "./Objects/IMR landings by gear and guild.rds")

heatmap(landings)

#### Summarise IMR Effort by gear ####

effort_target <- dplyr::select(gear, Aggregated_gear) %>%                     # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears
  
effort <- st_drop_geometry(corrected_IMR) %>%                                 # Remove geometries
  group_by(Aggregated_gear) %>%                                               # By gear
  summarise(Hours = sum(corrected_effort, na.rm = T)) %>%                     # Total fishing effort
  right_join(effort_target) %>%                                               # Reintroduce unobserved gears
  replace_na(replace = list(Hours = 0)) %>%                                   # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(effort, "./Objects/IMR absolute fishing effort")                      # Save

#### visual checks ####

# library(ggnewscale)
# library(stars)
# 
# star <- st_as_stars(GFW_mobile)    # convert GFW to stars objects for use in sf plots
# star2 <- st_as_stars(GFW_static)
# 
# # Overlay domain, GFW, and IMR
# 
# ggplot() +
#   geom_sf(data = Regions, fill = "yellow", colour = "black") +
#   geom_sf(data = IMR_effort, aes(fill = GFW_Scale)) +
#   new_scale("fill") +
#   geom_stars(data=star) +
#   scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt") +
#   labs(title = str_glue("{round(sum(IMR_effort$corrected_effort, na.rm = T)/
#                         sum(IMR$Effort, na.rm = T), 2) *100}% of fishing effort allocated"))
