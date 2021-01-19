
# Make a choice map of model domains based on upper and lower depths, and calculate distance from shore

#### Set up ####

rm(list=ls())                                                               # Wipe brain                                             

Tidy_packages <- c("tidyverse", "data.table", "pbapply", "viridis", "furrr")# List handy data packages
Geo_packages <- c("sf", "rnaturalearth",  "stars")                          # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 
plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

Bathymetry <- readRDS("./Objects/Bathymetry_points.rds") %>%                # Get bathymetry
  .[seq(1, nrow(.), 16),] %>%                                               # Reduce resolution for plotting speed
  filter(between(Elevation, -1000, 0))  

cells <- st_as_stars(Bathymetry)                                            # Convert to stars to get cells instead of points, (so no gaps) 
st_crs(cells) <- st_crs(4326)                                               # set lat-lon crs

cells <- st_as_sf(cells, as_points = F, merge = F) %>%                      # Convert the stars grid into SF polygons
  drop_na() %>% 
  st_transform(crs = crs)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Calculate distance from shore for points ####

shrink <- filter(world, subregion %in% c("Northern America", "Northern Europe", "Eastern Europe")) # Measuring distances goes faster if we don't check every country

close <- st_as_sf(Bathymetry, coords = c("Longitude", "Latitude"), crs = 4326) %>% # Set dataframe to SF format
  st_transform(crs) 

dist <- st_distance(close, shrink) %>% pbapply(1, min)                      # Calculate the distances between points and polygons, grab the closest
close$Shore_dist <- dist                                                    # Send distances to a column

saveRDS(close, "./Objects/Bathymetry and distance from shore.rds")

distance_plot <- ggplot() +
  geom_sf(data = close, aes(geometry = geometry, colour = Shore_dist), size = 0.1) +
  geom_sf(data = world, size = 0.1) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  scale_colour_viridis(name = 'Distance (m)') +
  guides(colour = guide_colourbar(barwidth = 0.5, barheight = 15)) +
  labs(caption = "Distance from shore") +
  zoom +
  NULL

ggsave_map("./Figures/bathymetry/Distance from shore.png", distance_plot)

#### Highlight areas between bathymetry limits ####

combos <- expand.grid(Shallow = c(-10,-20,-30,-40), 
                      Deep = c(-200,-300,-400,-500))                        # Create combinations of max and min depths

limit <- function(Shallow, Deep) {
  
labels <- data.frame("Deep" = abs(Deep), "Shallow" = abs(Shallow))
  
Shape <- filter(cells, between(Elevation, Deep, Shallow)) %>%
  st_union() %>%
  st_sf(labels)
  }                                      # Filter the bathymetry by a max and min depth combo and turn to a polygon

Choices <- future_map2(combos$Shallow, combos$Deep, .f=limit) %>% 
  rbindlist() %>%
  st_as_sf() %>% 
  st_transform(crs = 3035)

#### Plot choice maps ####

choice_plot <- ggplot() +
  geom_sf(data = Choices, aes(geometry = .), fill = "yellow", size = 0.1) +
  geom_sf(data = world, fill = "Black", colour = "Black", size = 0.1) +
  theme_minimal() +
  zoom +
  facet_grid(rows = vars(Shallow), cols = vars(Deep)) +
  labs(caption = "Bathymetry choice maps", x = NULL, y = NULL) +
  theme(axis.text = element_blank()) +
  NULL

ggsave_map("./Figures/bathymetry/Depth choices.png", choice_plot)

    