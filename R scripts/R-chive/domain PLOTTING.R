
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
source("file:///home/alb19154/R scripts/domain FUNCTIONS.R")                # Bring in the functions I've written (we want the plotting ones)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

load("~/Data/Bathymetry GEBCO/GEBCO many contours.Rdata")                   # Load all extracted depth contours
lines <- filter(many_lines, level %in% c("-30", "-200", "-1000"))           # Take contours of interest

Tidy_data <- readRDS("domain.SP.rds")                                       # Read in Spatial data summaries

Currents <- readRDS("domain.SP.currents.rds")                               # Read in spatial data summaries for currents
thin <- seq(1,nrow(Currents[[1]]), by = 6)                                  # Thin each dataframe to a sixth  
direction <- lapply(Currents, slice, thin) %>% lapply(drop_na)              # Take a subset of arrows to plot for legibility, dropping arrows with NAs

TS <- readRDS("TS")                                                         # Read in time series

#### Plotting ####
    
vars <- c("Ice", "Turbocline", "Mixed", "Salinity", 
          "Temperature", "DIN", "Chlorophyll", "Current")                   # List of variables to plot
sapply(vars, ts_plot)                                                       # Save a time series figure for each variable.

tic ("Plotting current figures")
mcmapply(stick_plot, Currents, direction, mc.cores = getOption("mc.cores", 3L)) # Plot with 3 cores
toc()

tic("Plotting spatial figures")                                             # Start timing
lapply(Tidy_data, point_plot)                                               # Slow but reliable plotting of spatial figures
toc()

