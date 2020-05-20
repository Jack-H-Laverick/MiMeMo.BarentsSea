
# Average the data pulled from NEMO - MEDUSA for creating decadal maps
# readRDS("./Objects/Detritus.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Instructions for parallel processing

#### Average by decade spatially ####

tic("Creating decadal spatial averages")                                    # Time the operation
  
SP <- list.files("./Objects/Detritus/", full.names = T) %>%                 # Get list of NM files
  future_map(decadal, .progress = TRUE) %>%                                 # Read in data, remove unnecessary columns and create a decade column
  rbindlist() %>%                                                           # Combine dataframes
  mutate(Decade = as.factor(Decade)) %>%                                    # Change decade to factor
  split(., f = list(.$Decade, .$Depth))                                     # Split into a large dataframe per decade (and depth to help plotting)

lapply(SP, setDT)                                                           # Break pipe to set all dataframes to datatables
    
  lapply(SP, summarise_sp, dt = T) %>%                                      # Average the variables per decade, dt method is faster
  future_map(reproj, crs = crs, .progress = TRUE) %>%
  saveRDS("./Objects/SPATIAL_detritus.rds")                                 # Save out spatial file
toc()                                                                       # Stop timing