
#**# Plot a time series of IMR landings by guild and gear to check for stability

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326)         # reproject to match EU data

habitats <- readRDS("./Objects/Habitats.rds")                                 # Load habitat polygons

gear <- read.csv("./Data/MiMeMo gears.csv") 

Regions <- rgdal::readOGR(dsn="./Data/IMR/Regions/") %>%                      # Import IMR regions shapefile
  st_as_sf() %>%                                                              # Convert to SF
  st_join(Domains) %>%                                                        # Which regions are in the model domain?
  drop_na() %>%                                                               # Drop those outside
  dplyr::select(Region = havomr) %>%                                          # Select and rename region column
  distinct(Region)                                                            # Keep unique shapes

GFW_mobile <- brick("./Objects/GFW.nc", varname = "NOR_mobile_gear") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean)  

GFW_static <- brick("./Objects/GFW.nc", varname = "NOR_static_gear") %>%      # For each class of gear
  calc(mean)

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(RE = "character", HO = "character")) %>% # Overwriting default column types  
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Region", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>%   # Set column names
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region) %>% 
  left_join(gear) %>%                                                                       # Attach labels
  filter(Aggregated_gear != "Dropped", Region %in% Regions$Region) %>% 
  group_by(Aggregated_gear, Gear_type, Region, Year) %>% 
  summarise(Effort = sum(Fishing_time, na.rm = T)) %>%
  summarise(Effort = mean(Effort, na.rm = T)) %>% 
  ungroup() %>% 
  merge(Regions, .)

#### Proportion IMR effort across gears and guilds ####

IMR_habitats <- rownames_to_column(IMR, var = "Feature") %>%               # Create a column to track each EU polygon
  st_intersection(habitats) %>%                                            # Split the EU polygons cross habitat types
  mutate(mobile = exact_extract(GFW_mobile, ., fun = "sum"),
         static = exact_extract(GFW_static, ., fun = "sum"),
         Habitat = paste0(Shore, " ", Habitat)) %>%                        # Combine habitat labels
  mutate(GFW = case_when(Gear_type == "Mobile" ~ mobile,
                         Gear_type == "Static" ~ static)) %>% 
  group_by(Region) %>%                                                     # Per original EU polygon
  mutate(share = GFW / sum(GFW, na.rm = T)) %>%                            # Work out the proportion of the area in each piece split over habitats
  ungroup() #%>% 

Empties <- group_by(IMR_habitats, Region) %>% 
          summarise(GFW = sum(share),
                    Effort = sum(Effort)) %>% 
  mutate(GFW = ifelse(is.na(GFW), "Missing", "Data"),
         Effort = Effort/sum(Effort)*100)

## NAs are introduced for GFW effort when no fishing is detected across any gear 
## and any habitat, so the denominator is 0. This means the effort in that region can't be allocated.

ggplot() +
  geom_sf(data = Empties, aes(colour = GFW, fill = Effort)) +
  scale_colour_manual(values=c("skyblue2", "firebrick")) +
  #facet_grid(cols = vars(Gear_type)) +
  theme_minimal() +
  labs(title = str_glue("{round(sum(filter(Empties, GFW == 'Missing')$Effort),2)}% of effort missed at regional level"))
    
# st_drop_geometry() %>%                                                      # Drop geometry column for simplicity
#       mutate(effort_contributions = Effort*share) %>%                                  # Adjust fishing days to account for breaking up EU polygons
#     group_by(Gear_type, Aggregated_gear, Habitat) %>%                           # By habitat and gear
#     summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
#     ungroup() %>% 
#     drop_na()
# 
# saveRDS(IMR_habitats, "./Objects/Habitat effort IMR.rds")
