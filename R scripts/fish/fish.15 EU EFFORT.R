
# Calculate absolute fishing effort by gear across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multisession)

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
  filter(Aggregated_gear != "Dropped") %>%  
  rownames_to_column(var = "EU_polygon") 

#### Locate EU effort inside the model domain ####

tictoc::tic()
corrected_effort <- dplyr::select(EU_Arctic, EU_polygon, Gear_type) %>%       # Limit to information needed to calculate the proportion of fishing effort in the model domain
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%                                                     # Isolate each shape for fast paralel processing
  future_map( ~{                                                              # In parallel
    mutate(.x, total = if_else(Gear_type == "Mobile",                         # If this is a mobile gear
                               exact_extract(GFW_mobile, .x, fun = "sum"),    # Get all mobile fishing effort from GFW, else static effort
                               exact_extract(GFW_static, .x, fun = "sum"))) %>% # This is the total effort to scale features to within a polygon
      st_intersection(domain) %>%                                             # Crop the polygons to the model domain
      mutate(feature = if_else(Gear_type == "Mobile",                         # Now count fishing effort again
                               as.numeric(exact_extract(GFW_mobile, ., fun = "sum")),       
                               as.numeric(exact_extract(GFW_static, ., fun = "sum")))) %>% 
               st_drop_geometry()}, .progress = T) %>%            
  data.table::rbindlist() %>%                                                 # Bind each fishing feature back into a DF
  mutate(GFW_Scale = feature/total) %>%                                       # Get the proportion of effort per polygon in the domain
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the polygon replace NA with 1 to not use this for scaling
  dplyr::select(GFW_Scale, EU_polygon) %>% 
  left_join(EU_Arctic) %>% 
  mutate(effort_contributions = ttfshdy*GFW_Scale*24)                         # Scale fishing effort by area in the model domain
tictoc::toc()    

saveRDS(corrected_effort, "./Objects/EU corrected pixel fishing effort.rds")  # Save

#### Summarise ####

target <- expand.grid(Aggregated_gear = unique(gears$Aggregated_gear),        # Select gear names
                      year = 2015:2018) %>%                                   # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears

summary <- group_by(corrected_effort, Aggregated_gear, year) %>%              # By year and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  ungroup() %>% 
  drop_na() %>%                                                               # Drop unassigned gears
  right_join(target) %>%                                                      # Reintroduce unobserved gears and years otherwise you miss 0s when averaging
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually effort of 0
  group_by(Aggregated_gear) %>% 
  summarise(effort = mean(effort, na.rm = TRUE)) %>%                          # Average for each gear across years
  remove_rownames() %>% 
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(summary, "./Objects/EU absolute fishing effort.rds")                  # Save

#### Visualise ####

# library(ggnewscale)
# library(stars)
# 
# star <- st_as_stars(GFW_mobile)    # convert GFW to stars objects for use in sf plots
# star2 <- st_as_stars(GFW_static)
# 
# ggplot() +
#   geom_sf(data = domain, fill = "yellow", colour = "black") +
#   geom_sf(data = EU_Arctic, aes(fill = ttfshdy), alpha = 0.3, colour = NA) +
#   new_scale("fill") +
#   geom_stars(data=star) +
#   scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt")
