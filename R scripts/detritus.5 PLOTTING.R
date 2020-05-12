
# readRDS("./Objects/TS_detritus.rds")       # Marker so network script can see where the data is coming from
# readRDS("./Objects/SPATIAL_detritus.rds")  # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

Tidy_packages <- c("MiMeMo.tools", "tidyverse", "furrr", "tictoc", "viridis", "ggnewscale") # List handy data packages
Geo_packages <- c("mapproj", "rnaturalearth", "sf", "rgdal")                # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Set parallel processing
  
lines <- readRDS("./Objects/Bathymetry_lines_proj.rds") %>%                 # Read in all the contours
  filter(level %in% c("-30", "-200", "-1000"))                              # Take contours of interest

SP <- readRDS("./Objects/SPATIAL_detritus.rds")                             # Read in spatial data
TS <- readRDS("./Objects/TS_detritus.rds")                                  # Read in time series

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Plotting ####
    
ts_plot("Detritus_avg")                                                     # Save a time series figure

tic("Plotting spatial figures")                                             
 future_map(SP, point_plot, var = "Detritus", zoom = zoom, .progress = TRUE) # Plot spatial maps in parallel
toc()

