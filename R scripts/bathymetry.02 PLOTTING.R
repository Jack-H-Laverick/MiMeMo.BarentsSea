
## Visualise the GEBCO bathymetry 

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "viridis")                                  # List handy data packages
Geo_packages <- c("sf", "stars", "rnaturalearth")                           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

contours <- readRDS("./Objects/Bathymetry_lines.rds")                       # Get the contours

points <- readRDS("./Objects/Bathymetry_points.rds") %>%                    # Get bathymetry
  .[seq(1, nrow(.), 16),]                                                   # Reduce resolution for plotting speed

cells <- filter(points, Elevation <=0) %>% 
  st_as_stars()                                                             # Convert to stars to get cells instead of points, (so no gaps) 
st_crs(cells) <- st_crs(4326)                                               # set lat-lon crs

cells <- st_as_sf(cells, as_points = F, merge = F) %>%                      # Convert the stars grid into SF polygons
  drop_na() %>% 
  st_transform(crs = crs)

world <- ne_countries(scale = "medium", returnclass = "sf")                 # Get a world map

#### Reproject ####

world <- st_transform(world, crs = crs)                                     # Assign polar projection

points_proj <- st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326) %>% # Specify original projection (crs) 
  st_transform(crs = crs)                                                   # Transform to new projection
saveRDS(points_proj, file = "./Objects/Bathymetry_points_proj.rds")

contours_proj <- st_as_sf(contours, coords = c("x", "y"), crs = 4326) %>%   # Specify original projection (crs) 
  st_transform(crs = crs) %>%                                               # Transform to new projection
  mutate(level = as.factor(level))                                          # Convert to factor for sensible legend
saveRDS(contours_proj, file = "./Objects/Bathymetry_lines_proj.rds")

#### Plot grid map ####

grid_map <- ggplot() +
  geom_sf(data = cells, aes(fill = Elevation), size = 0) +
  scale_fill_viridis(name = 'Depth (m)') +
  geom_sf(data = world) +
  zoom +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15)) +
  labs(caption = "GEBCO bathymetry grid") +
  NULL
ggsave_map("./Figures/bathymetry/GEBCO grid.png", grid_map)

#### Plot contours #### 

line_map <- ggplot() +
  geom_sf(data = contours_proj, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
  scale_colour_viridis(name = 'Depth (m)', discrete = TRUE) +
  geom_sf(data = world) +
  zoom +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(caption = "GEBCO bathymetry contours") +
  NULL
ggsave_map("./Figures/bathymetry/GEBCO contours.png", line_map)
