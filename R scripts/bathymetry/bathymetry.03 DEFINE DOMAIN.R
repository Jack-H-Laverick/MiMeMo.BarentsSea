
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Distance from shore ####

shrink <- filter(world, subregion %in% c("Northern America", "Northern Europe", "Eastern Europe")) # Measuring distances goes faster if we don't check every country

Bathymetry <- readRDS("./Objects/Bathymetry_points.rds") %>%                # Get bathymetry
  .[seq(1, nrow(.), 16),] %>%                                               # Reduce resolution for plotting speed
  filter(between(Elevation, -1000, 0))  

close <- st_as_sf(Bathymetry, coords = c("Longitude", "Latitude"), crs = 4326) %>% # Set dataframe to SF format
  st_transform(crs) 

dist <- st_distance(close, shrink) %>% pbapply::pbapply(1, min)             # Calculate the distances between points and polygons, grab the closest
close$Shore_dist <- dist                                                    # Send distances to a column

ggplot() +
  geom_sf(data = close, aes(geometry = geometry, colour = Shore_dist), size = 0.1) +
  geom_sf(data = world, size = 0.1) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  viridis::scale_colour_viridis(name = 'Distance (m)') +
  guides(colour = guide_colourbar(barwidth = 0.5, barheight = 15)) +
  labs(caption = "Distance from shore") +
  zoom +
  NULL

ggsave_map("./Figures/bathymetry/Distnace.png", last_plot())

#### Final area choices ####

Bathymetry <- st_as_stars(Bathymetry)                                       # Convert to stars to get cells instead of points
st_crs(Bathymetry) <- st_crs(4326)                                          # set lat-lon crs

Bathymetry <- st_as_sf(Bathymetry, as_points = F, merge = F)  %>%           # Convert the stars grid into SF polygons which can be merged
  st_transform(crs = crs) %>% 
  st_join(dplyr::select(close, -Elevation)) %>%                             # Attach distance from shore for simultaneous filtering
  drop_na()

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
