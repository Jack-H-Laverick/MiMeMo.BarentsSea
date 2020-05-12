
#### Set up ####

rm(list=ls())                                                               # Wipe the brain
library(tidyverse)                                                          # Enter the tidyverse
library(rgdal)                                                              # Handle GIS data imports
library(ggfortify)                                                          # Coerce shapefiles to dataframes
library(maps)                                                               # GIS in R
library(rgeos)
library(rnaturalearth)
library(spatialEco)
library(sf)

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}                        # Function to pull the geometry column of an SF object into XY

Sediment <- readOGR(dsn="NGU_MarineBunnsedimenter_kornstr_oversikt", layer = "KornstorrelseFlate_oversikt")                                        # Import FAO shapefile

world <- fortify(map("world", fill = TRUE, col = "grey"))                                 # Import map of landmasses

# #### Inspect NGU shapefile ####
# 
#  test <- fortify(Sediment)
#  
#  ggplot(data = test, aes(x = long, y = lat, group = group, fill = id)) +
#    geom_polygon(colour = "white", size = 0.1) +                                # Plots polygons with holes
#    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black") +
#    theme_minimal() +
#    cosord_fixed(1.3, xlim = c(min(-50), max(75)), ylim = c(min(50), max(90))) +
#    labs(x = 'Longitude (W)', y = 'Latitude (N)', title = "Barents Sea (mostly) Sediment") +
#    theme(legend.position = "None") +
#    NULL
# ggsave('FIG_NGU Sediment types.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")
# 
#### Match Nemo - Medusa grid to NGU shapefile polygons ####

points <- readRDS("./Objects/Fixed_grid.rds") %>%                       # Read in Lat Lon positions from NEMO-MEDUSA outputs
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)             # Specify original projection (crs) 

NGU <- point.in.poly(points, Sediment) %>%                              # Check if a point is in a polygon and pull the associated meta-data
  fortify() %>%                                                         # Convert to DF
  drop_na() %>%                                                         # Drop NM coords outside the seidment data
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%                   # Convert to Simple Features object 
  st_transform(crs = 3035) %>%                                          # Transform to new polar centric projection
  sfc_as_cols() %>%                                                     # Extract geometry column to allow usual ggplot syntax
  st_set_geometry(NULL) %>%                                             # Chuck duplicated geometry column
  mutate(SEDKORNSTR = as.factor(SEDKORNSTR)) %>%                         # change data type
  filter(between(x, 2600000, 6300000) & between(y, 4000000, 7700000))   # Remove points you won't see to (limits colour scale range)
#saveRDS(NGU, file = "./Objects/Gridded_NGU.rds")

#NGU <- mutate(NGU, SEDKORNSTR = as.factor(SEDKORNSTR)

#### Pull in additional sediment data sources ####

data <- read.csv("norw_surf_grain_size.tab", skip = 98, header = TRUE, sep = "") # Read in the chunk of data from Pangaea

colnames(data) <- c("Event", "Latitude", "Longitude", "Depth (m)",          # Correct column names because of white space
                    "<63 µm (%)", "Sand (%)", ">2 mm (%)")

Greenland_NS <- data[,1:7] %>% 
  select(-"Depth (m)") %>%                                                  # Trim
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Specify original projection (crs) 
  st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
  sfc_as_cols() %>%             
  st_set_geometry(NULL)                                                     # chuck out geometry column

data <- read.csv("surface_sedimentology.tab", skip = 65, header = TRUE, sep = "") # Read in second sediment study from Pangaea

colnames(data) <- c("Event", "Latitude", "Longitude", "Elevation (m)", "Depth (m)", # Correct column names
                    "<2 µm, >9 phi (%)", "Silt (%)", "Sand (%)", ">2 mm (%)", "Kln/Ill", 
                    "bSiO2 (%)", "CaCO3 (%)", "TOC (%)", "C/N", "δ13C Corg [‰ PDB]")

Greenland <- data[,1:15] %>% 
  select(-"Depth (m)") %>%                                                  # Trim
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Specify original projection (crs) 
  st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
  sfc_as_cols() %>%
  st_set_geometry(NULL)                                                     # chuck out geometry column

data <- read.csv("Surface_clay_mineral.tab", skip = 104, header = TRUE, sep = "") # Read in the chunk of data from Pangaea

colnames(data) <- c("Event", "Latitude", "Longitude", "Elevation (m)", "Depth (m)", # Correct column names because of white space
                    "Sand (%)", "Silt (%)", "<2 µm, >9 phi [%]", "Sme [%]", "Ill [%]", "Chl [%]", "Kln [%]")

Svalbard <- data[,1:12] %>% 
  select(-"Depth (m)") %>%                                                  # Trim
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Specify original projection (crs) 
  st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
  sfc_as_cols() %>%             
  st_set_geometry(NULL)                                                     # chuck out geometry column

#saveRDS(Greenland_NS, file = "./Objects/Greenland.rds")
#saveRDS(Svalbard, file = "./Objects/Svalbard.rds")


