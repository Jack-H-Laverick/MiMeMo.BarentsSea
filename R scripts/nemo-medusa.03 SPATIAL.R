
# Average the data pulled from NEMO - MEDUSA for creating decadal maps
# readRDS("./Objects/Months.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tictoc", "furrr")                            # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Instructions for parallel processing

#### Average by decade spatially ####

tic("Creating decadal spatial averages")                                    # Time the operation
  
SP <- list.files("./Objects/Months/", full.names = T) %>%                   # Get list of NM files
  future_map(decadal, .progress = TRUE) %>%                                 # Read in data, remove unnecessary columns and create a decade column
  data.table::rbindlist() %>%                                               # Combine dataframes
  mutate(Decade = as.factor(Decade)) %>%                                    # Change decade to factor
  split(., f = list(.$Decade, .$Depth)) %>%                                 # Split into a large dataframe per decade (and depth to help plotting)
  lapply(strip_ice, dt = T) %>%                                             # Remove snow and ice variables from deep data before averaging
  lapply(summarise_sp, dt = T) %>%                                          # Average the variables per decade, dt method is faster
  saveRDS("./Objects/SPATIAL.rds")                                          # Save out spatial file in the folder above WD
toc()                                                                       # Stop timing
  
# Check NAs are preserved in maps