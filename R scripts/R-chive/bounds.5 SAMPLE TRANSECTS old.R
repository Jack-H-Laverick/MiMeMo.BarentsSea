
# readRDS("./Objects/Currents/")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "viridis", "data.table", "furrr", "tictoc") # List handy data packages
Geo_packages <- c("stars", "sf", "rnaturalearth")                           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 
source("./R scripts/bounds.z FUNCTIONS.R")                                  # Pull custom functions

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

Shallow_transects <- readRDS("./Objects/Boundary_transects.rds") %>%        # Import transects to sample at
   filter(Shore == "Offshore" | Neighbour != "Offshore") %>%                # Drop Inshore to Offshore transects(avoid double accounting with Offshore to inshore transects)
   split(list(.$current))                                                   # Split by the current which needs to be sampled

Deep_transects <- readRDS("./Objects/Boundary_transects.rds") %>%           # Import transects to sample at
   filter(Shore == "Offshore" & Neighbour == "Ocean") %>%                   # The deep layer of the model only has exchange between offshore and the sea (and a vertical component dealt with separately)
   split(list(.$current))                                                   # Split by the current which needs to be sampled

grid <- readRDS("./Objects/Fixed_grid.rds")                                 # Joining data to the grid reorders the datapoints
lat <- matrix(grid$Latitude, nrow=235, ncol=190)                            # Reshape to a matrix for stars
lon <- matrix(grid$Longitude, nrow=235, ncol=190)                                  

#### Extract water exchanges between horizontal compartments ####

tic()
test <- Sample("./Objects/Currents/c.1.1981.rds")
toc()

tic()
Flows <- list.files("./Objects/Currents/", full.names = T) %>%             # Get the names of all data files 
  future_map(Sample, .progress = T) %>%                                    # Sample the currents in each file and aggregate
  rbindlist() %>%                                                          # Bind into a dataframe
  saveRDS("./Objects/H-Flows.rds")                                         # Save
toc()
