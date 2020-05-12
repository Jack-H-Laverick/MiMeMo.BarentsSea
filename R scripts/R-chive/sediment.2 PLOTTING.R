
#### Set up ####

rm(list=ls())                                                               # Wipe the brain
library(tidyverse)                                                          # Enter the tidyverse
library(rnaturalearth)
library(sf)
library(ggnewscale)                                                         # Lets you specify multiple fills and colours 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

NGU <- readRDS("./Objects/Gridded_NGU.rds")
Greenland <- readRDS("./Objects/Greenland.rds")
Svalbard <- readRDS("./Objects/Svalbard.rds")

lines <- readrds("./Objectes/Bathymetry_lines.Rdata") %>%                   # Load all extracted depth contours
   filter(level %in% c("-30", "-200", "-1000"))                             # Take contours of interest

#### Plot the Sediment grid ####

ggplot() +
  geom_point(data = NGU, aes(x = x, y = y, colour = SEDKORNSTR),            # NGU data
             size = 0.65, stroke = 0, shape = 16) + 
  new_scale_colour() +   
  geom_point(data = Greenland, aes(x = x, y = y), colour = "green", size = 0.5) + # Pangaea data
  geom_point(data = Svalbard, aes(x = x, y = y), colour = "red", size = 0.5) +    # Pangaea data
  # Bathymetry
  geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.1, show.legend = "line") +
  scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "red", "-200" = "pink" , "-30" = "black")) +
  # World
  geom_sf(data = world, fill = "grey", size = 0.2) +
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
  theme_minimal() +
  theme(legend.position = "None") +
  labs(x = NULL, y = NULL) +
  NULL

 #ggsave("./Figures/sediment/Gridded Sediments.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
