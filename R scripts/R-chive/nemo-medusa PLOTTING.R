
#### Set up ####

setwd("~/Data/NEMO - MEDUSA")
rm(list=ls(all.names = TRUE))     
library(tidyverse)
library(mapproj)
library(rnaturalearth)
library(sf)
library(viridis)
library(rgdal)
library(ggnewscale)                                                         # Lets you specify multiple fills and colours 
library(parallel)
library(tictoc)                                                             # For timing processes
source("file:///home/alb19154/R scripts/nemo-medusa FUNCTIONS.R")           # Bring in the functions I've written (we want the plotting ones)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

#load("~/Data/Fish Effort GFW/fishing_effort/GEBCO contours.Rdata")         # Load 200m and 1000m depth contour
load("~/Data/Bathymetry GEBCO/GEBCO many contours.Rdata")                   # Load all extracted depth contours
lines <- filter(many_lines, level %in% c("-30", "-200", "-1000"))           # Take contours of interest

Tidy_data <- readRDS("Tidy_data weighted.rds")
Currents <- readRDS("weighted.Currents.rds")

thin <- seq(1,nrow(Currents[[1]]), by = 6)                                  # Thin each dataframe to a sixth  
direction <- lapply(Currents, slice, thin) %>% lapply(drop_na)              # Take a subset of arrows to plot for legibility, dropping arrows with NAs
  
#### Decadal facet ####

#lapply(Tidy_data[37:45], point_plot)
#point_plot(Tidy_data[[30]])
#point_plot(Tidy_data$Turbocline.1980)

tic("Plotting point figures")
#mclapply(Tidy_data, point_plot, mc.cores = getOption("mc.cores", 7L))# Parallelised plotting over 7 of the 8 cores ** Not all cores are completing
lapply(Tidy_data, point_plot)
toc()

#### Stick plot for currents ####

#mapply(stick_plot, Currents, direction)
#(Currents[[36]], direction[[36]])

tic ("Plotting current plots")
mcmapply(stick_plot, Currents, direction, mc.cores = getOption("mc.cores", 3L)) # Plot with 3 cores
toc()

#### Incomplete attempt at rasterising plotting ####

 library(sp)
 library(raster)
 library(ggfortify)
 
# trial <- Tidy_data[["Turbocline.1980"]] %>% filter(Month == "January")
# 
# r <- raster(ncols=500, nrows=500)                                         # Set resolution of a raster layer
# 
# data <- st_as_sf(trial, coords = c("x", "y"), crs = 3035) %>% # Set dataframe to SF format
#   st_transform(crs = 4326) %>%                                                # Transform to new polar centric projection
#   rasterize(r, field = "Turbocline", fun=sum)# %>%                             # Get a raster of point presence

#  projectRaster(crs = crs)
# test <- fortify(data)
# plot(data)
