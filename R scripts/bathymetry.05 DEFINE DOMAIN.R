
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

Distance <- readRDS("./Objects/Bathymetry and distance from shore.rds")     # Read in bathymetry points with distances

Bathymetry <- readRDS("./Objects/Bathymetry_points.rds") %>%                # Get bathymetry
  .[seq(1, nrow(.), 16),] %>%                                               # Reduce resolution for plotting speed
  filter(between(Elevation, -1000, 0)) %>%                                  # Clip area
  st_as_stars()                                                             # Convert to stars to get cells instead of points
st_crs(Bathymetry) <- st_crs(4326)                                          # set lat-lon crs

Bathymetry <- st_as_sf(Bathymetry, as_points = F, merge = F)  %>%           # Convert the stars grid into SF polygons which can be merged
  st_transform(crs = crs) %>% 
  st_join(dplyr::select(Distance, -Elevation)) %>%                          # Attach distance from shore for simultaneous filtering
  drop_na()

#### Final area choices ####

Domains <- mutate(Bathymetry, Shore = ifelse(between(Elevation, -400, -60) & Shore_dist > 20000, "Offshore",
                                    ifelse(Elevation > -60 | Shore_dist < 20000, "Inshore", NA))) %>% 
  drop_na() %>% 
  st_join(Region_mask) %>%                                                  # Limit the area of interest 
  drop_na() %>% 
  mutate(Area = as.numeric(st_area(.))) %>%                                 # Measure the size of each cell
  group_by(Shore) %>% 
  summarise(Elevation = mean(Elevation),                                    # nb, Inshore mean depth is deeper than 60 m because of deep areas close to shore.
            area = sum(Area))                                               # Cheat way to union cells by group and get measurements 
saveRDS(Domains, "./Objects/Domains.rds")

#### Plot ####

colours <- c(Inshore = "yellow", Offshore = "yellow3")

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
  geom_sf(data = Region_mask, colour = "red", fill = NA) + 
  geom_sf(data = world, size = 0.1, fill = "black") +
  scale_fill_manual(values = colours, name = "Zone") +
  zoom +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(caption = "Final model area") +
  NULL
ggsave_map("./Figures/bathymetry/Domains.png", map)
