
# Create Simple Feature polygons of the FAO areas of interest to the project

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "sf", "rgdal")  # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

#### Set FAO fishing regions to extract data from ####

readOGR(dsn="./Data/FAO_map", layer = "FAO_AREAS") %>%                        # Import FAO shapefile
  st_as_sf() %>%                                                              # Convert to sf
  filter(F_CODE %in% c("27.1.a", "27.1.b", "27.2.a.1", "27.2.a.2",            # Limit to areas of interest
                       "27.2.b.1", "27.2.b.2", "27.14.a")) %>%                                  
  mutate(Region = c("27.1", "27.1", "27.2.a", "27.2.a", "27.2.b", "27.2.b", "27.14.a")) %>% # Specify which should be combined
  select(Region, SURFACE) %>%                                               # Drop excess columns 
  group_by(Region) %>%                                                      # Cheat way to union polygons by a group
  summarise(Area = sum(SURFACE)) %>% 
  saveRDS(file = "./Objects/FAO.rds")                                         # Save object

