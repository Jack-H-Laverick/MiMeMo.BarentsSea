
# Average the data pulled from NEMO - MEDUSA for creating decadal maps
# readRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)             
library(furrr)
plan(multisession)                                                          # Instructions for parallel processing

#### Average by decade spatially ####

tic()                                                                       # Time the operation
  
SP <- list.files("./Objects/Months/", full.names = T) %>%                   # Get list of NM files
  future_map(decadal, .progress = TRUE) %>%                                 # Read in data, remove unnecessary columns and create a decade column
  data.table::rbindlist() %>%                                               # Combine dataframes
  mutate(Decade = as.factor(Decade),                                        # Change decade to factor
         Speed = vectors_2_direction(Zonal, Meridional)[,"uvSpeed"]) %>%    # Convert currents to speed     
  split(., f = list(.$Decade, .$slab_layer)) %>%                            # Split into a large dataframe per decade (and depth to help plotting)
  lapply(strip_ice, dt = T) %>%                                             # Remove snow and ice variables from deep data before averaging
  lapply(NM_decadal_summary, dt = T) %>%                                    # Average the variables per decade, dt method is faster
  saveRDS("./Objects/SPATIAL.rds")                                          # Save out spatial file in the folder above WD
toc()                                                                       # Stop timing
