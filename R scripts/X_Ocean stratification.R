
#### Set up ####

setwd("~/Analyses/Depth stratification")
rm(list=ls(all.names = TRUE))     
library(tidyverse)
library(viridis)
library(ggnewscale)                                                         # Lets you specify multiple fills and colours 
library(data.table)
library(mapproj)
library(rnaturalearth)
library(sf)
library(rgdal)

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}                        # Function to pull the geometry column of an SF object into XY

output <- readRDS("~/Data/NEMO - MEDUSA/GEBCO_grid.rds")  %>%               # Load in Bathymetry pulled from GEBCO instead
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Specify original projection (crs) 
  st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
  sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
  st_set_geometry(NULL) %>%                                                 # chuck out geometry column
  mutate(Bathymetry = abs(Bathymetry))                                      # Remove the - sign

Tidy_data <- readRDS("~/Data/NEMO - MEDUSA/Tidy_data weighted.rds") %>% .[40:45] # Read in mixed layer and turbocline depths

Turbocline <- Tidy_data[1:3] %>% rbindlist %>% select(-depth) %>% left_join(output) # Combine and add bathymetry
Mixed <- Tidy_data[4:6] %>% rbindlist %>% select(-depth) %>% left_join(output)

#### Plots ####

ggplot(filter(Turbocline, Bathymetry > 0)) +
  geom_hex(aes(y = Turbocline, x = Bathymetry, fill = stat(log(count))), bins = 25) +
  scale_fill_viridis(name = 'Scaled\ndensity\n(log)') +
  geom_line(aes(y = Bathymetry, x = Bathymetry), colour = "red") +
  theme_minimal() +
  labs(x = "Sea floor depth (m)", y = "Min turbocline depth per month (m)",
       title = "Turbocline depths by month", subtitle = "All depths" ) +
  coord_cartesian(ylim = c(0, 2000)) +
  facet_wrap(~Month) +
  NULL
#ggsave('Turbocline.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
  
ggplot(filter(Turbocline, between(Bathymetry, 1, 100))) +
  geom_hex(aes(y = Turbocline, x = Bathymetry, fill = stat(log(count))), bins = 25) +
  scale_fill_viridis(name = 'Scaled\ndensity\n(log)') +
  geom_line(aes(y = Bathymetry, x = Bathymetry), colour = "red") +
  theme_minimal() +
  labs(x = "Sea floor depth (m)", y = "Min turbocline depth per month (m)",
       title = "Turbocline depths by month", subtitle = "Shallower than 100 m" ) +
  facet_wrap(~Month) +
  NULL
#ggsave('Turbocline_shallow.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

ggplot(filter(Mixed, Bathymetry > 0)) +
  geom_hex(aes(y = Mixed, x = Bathymetry, fill = stat(log(count))), bins = 25) +
  scale_fill_viridis(name = 'Scaled\ndensity\n(log)') +
  geom_line(aes(y = Bathymetry, x = Bathymetry), colour = "red") +
  theme_minimal() +
  labs(x = "Sea floor depth (m)", y = "Min mixed layer depth per month (m)",
       title = "Mixed layer depths by month", subtitle = "All depths" ) +
  coord_cartesian(ylim = c(0, 2000)) +
  facet_wrap(~Month) +
  NULL
#ggsave('Mixed.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
  
ggplot(filter(Mixed, between(Bathymetry, 1, 100))) +
  geom_hex(aes(y = Mixed, x = Bathymetry, fill = stat(log(count))), bins = 25) +
  scale_fill_viridis(name = 'Scaled\ndensity\n(log)') +
  geom_line(aes(y = Bathymetry, x = Bathymetry), colour = "red") +
  theme_minimal() +
  labs(x = "Sea floor depth (m)", y = "Min mixed layer depth per month (m)",
       title = "Mixed layer depths by month", subtitle = "Shallower than 100 m" ) +
  facet_wrap(~Month) +
  NULL
#ggsave('Mixed_shallow.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
