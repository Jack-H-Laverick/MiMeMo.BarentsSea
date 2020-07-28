
# readRDS("./Objects/TS.rds")       # Marker so network script can see where the data is coming from
# readRDS("./Objects/SPATIAL.rds")  # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "furrr", "tictoc", "rnaturalearth")
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Set parallel processing
  
lines <- readRDS("./Objects/Bathymetry_lines_proj.rds") %>%                 # Read in all the contours
  filter(level %in% c("-30", "-200", "-1000"))                              # Take contours of interest

SP <- readRDS("./Objects/SPATIAL.rds")                                      # Read in spatial data
TS <- readRDS("./Objects/TS.rds")                                           # Read in time series

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Plotting ####
    
vars_ts <- c("Ice_pres", "Ice_conc_avg", "Ice_Thickness_avg", "Snow_Thickness_avg", 
          "Vertical_diffusivity_avg", "Vertical_velocity_avg", "Salinity_avg",
          "Temperature_avg", "DIN_avg", "Detritus_avg", "Chlorophyll_avg")  # List of variables to plot   
vars_sp <- c("Ice_conc", "Ice_Thickness", "Snow_Thickness", "Salinity", 
             "Temperature", "DIN", "Detritus", "Chlorophyll")               # Tweak the variable names for spatial plots

sapply(vars_ts, ts_plot)                                                    # Save a time series figure for each variable.

tic("Plotting spatial figures")                                             
 future_map2(rep(SP[1:12], each = length(vars_sp)), 
             rep(vars_sp, times = length(SP)/2), point_plot,
             zoom = zoom, .progress = TRUE)                                 # Plot spatial maps in parallel
toc()
  
tic("Plotting spatial figures")                                             
future_map2(rep(SP[13:24], each = length(vars_sp)),                         # I can't run all in one go because I run out of RAM
            rep(vars_sp, times = length(SP)/2), point_plot, 
            zoom = zoom, .progress = TRUE)                                  # Plot spatial maps in parallel
toc()

tic ("Plotting current figures")
 future_map(SP, stick_plot, zoom = zoom, pre = pre, .progress = TRUE)       # Plot currents in parallel
toc()


#### speed voronoi? ####

# library(profvis)
# 
# tic("Plotting spatial figures")                                             
# profvis({
# future_map2(rep(SP[1], each = 4), 
#             vars_sp[4:7], point_plot_voronoi,
#             zoom = zoom, .progress = TRUE)                                 # Plot spatial maps in parallel
# })
# toc()
