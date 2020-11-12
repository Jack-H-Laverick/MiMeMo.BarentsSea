
#**# Plot a time series of IMR landings by guild and gear to check for stability

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%     # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                               # Drop unnecessary columns
  st_union()                                                                  # Create whole domain shape 

gear <- read.csv("./Data/MiMeMo gears.csv") 

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
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region) %>%        # Ditch unnecessary columns
  left_join(gear) %>%                                                         # Attach gear labels
  filter(Aggregated_gear != "Dropped", Region %in% Regions$Region) %>%        # Limited to gears and regions of interest
  group_by(Aggregated_gear, Gear_type, Region, Year) %>%                      # Total up effort within years          
  summarise(Effort = sum(Fishing_time, na.rm = T)) %>%
  summarise(Effort = mean(Effort, na.rm = T)) %>%                             # Then average across years
  ungroup() %>% 
  merge(Regions, .)                                                           # Add in sf geometries by IMR region

#### Proportion IMR effort across gears and guilds ####

Regions_GFW <- Regions %>% 
  mutate(mobile_total = exact_extract(GFW_mobile, ., fun = "sum"),            # Get all mobile fishing effort from GFW in an IMR region
         static_total = exact_extract(GFW_static, ., fun = "sum")) %>%        # This is the total effort to scale features to within a polygon
  st_drop_geometry()                                                          # Drop geometry for a non-spatial join

IMR_effort <- rownames_to_column(IMR, var = "Feature") %>%                    # Create a column to track each polygon
  st_intersection(Domains) %>%                                                # Crop the IMR polygons to the model domain
  mutate(mobile_feature = exact_extract(GFW_mobile, ., fun = "sum"),          # Get the GFW fishing effort in each shape
         static_feature = exact_extract(GFW_static, ., fun = "sum")) %>%      # Depending on gear type
  left_join(Regions_GFW) %>%                                                  # Attach total GFW effort by IMR region
  mutate(GFW_Scale = case_when(Gear_type == "Mobile" ~ (mobile_feature)/mobile_total, # Depending on gear type
                               Gear_type == "Static" ~ (static_feature)/static_total)) %>%  # Get proportion of GFW effort from a region within a feature
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the region replace NA with 1 to not use this for scaling
  mutate(corrected_effort = Effort*GFW_Scale)                                 # Scale whole region effort per gear by the proportion of GFW activity by gear type in the model domain 

#### Summarise ####

target <- dplyr::select(gear, Aggregated_gear) %>%                            # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears
  
Goal <- st_drop_geometry(IMR_effort) %>%                                      # Remove geometries
  group_by(Aggregated_gear) %>%                                               # By gear
  summarise(Hours = sum(corrected_effort, na.rm = T)) %>%                     # Total fishing effort
  right_join(target) %>%                                                      # Reintroduce unobserved gears
  replace_na(replace = list(Hours = 0)) %>%                                   # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(Goal, "./Objects/IMR absolute fishing effort")                        # Save

#### visual checks ####

library(ggnewscale)
library(stars)

star <- st_as_stars(GFW_mobile)    # convert GFW to stars objects for use in sf plots
star2 <- st_as_stars(GFW_static)

# Overlay domain, GFW, and IMR

ggplot() +
  geom_sf(data = Regions, fill = "yellow", colour = "black") +
  geom_sf(data = IMR_effort, aes(fill = GFW_Scale)) +
  new_scale("fill") +
  geom_stars(data=star) +
  scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt") +
  labs(title = str_glue("{round(sum(IMR_effort$corrected_effort, na.rm = T)/
                        sum(IMR$Effort, na.rm = T), 2) *100}% of fishing effort allocated"))

# Spatial distribution according to GFW

plot_Regions_GFW <- Regions %>% 
  mutate(mobile_total = exact_extract(GFW_mobile, ., fun = "sum"),
         static_total = exact_extract(GFW_static, ., fun = "sum")) %>%
  mutate(total = (mobile_total+static_total)/(sum(mobile_total)+sum(static_total))*100) %>% 
  arrange(Region)

ggplot(plot_Regions_GFW) +
  geom_sf(aes(fill = total)) +
  geom_sf_text(aes(label = round(total,1)), colour = "white") +
  labs(title = "GFW distribution", fill = "Effort (%)")

# Spatial distribution according to IMR

plot_Regions_IMR <- IMR %>% 
  group_by(Region) %>% 
  summarise(total = sum(Effort, na.rm = T)) %>% 
  mutate(total = total/sum(total)*100)%>% 
  arrange(Region)    

ggplot(plot_Regions_IMR) +
  geom_sf(aes(fill = total)) +
  geom_sf_text(aes(label = round(total,1)), colour = "white") +
  labs(title = "IMR distribution", fill = "Effort (%)")

# Discrepancy 

deviation <- plot_Regions_GFW %>% 
  mutate(total = total - plot_Regions_IMR$total)

ggplot(deviation) +
  geom_sf(aes(fill = total)) +
  geom_sf_text(aes(label = round(total,1)), colour = "white") +
  labs(title = "GFW% - IMR%", fill = "Effort (%)",
       subtitle = str_glue("total absolute deviation = {round(sum(abs(deviation$total)), 1)}%")) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  theme_void()
