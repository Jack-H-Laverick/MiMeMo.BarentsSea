
# Pull a time series from the monthly data files extracted from NEMO - MEDUSA on the idrive
# readRDS("./Objects/Months.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "tictoc")                              # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Region file.R")                                         # Define project region 

plan(multiprocess)                                                            # Instructions for parallel processing

#### Extract time series ####

tic("Creating time series by compartment")                                    # Time the data extraction

TS <- list.files("./Objects/Months/", full.names = T) %>%                     # Get list of NEMO-MEDUSA files
  future_map(summarise_ts, .progress = T) %>%                                 # Read in the months and calaculate mean and sd by compartments
  data.table::rbindlist() %>%                                                 # Combine timesteps into series
  mutate(Planar_current = Zonal_avg + Meridional_avg,                         # Get the resultant current force without direction
         date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"), # Create a single date column for plotting
         Compartment = paste(Shore, Depth, sep = " ")) %>%                    # Build a single compartment column for plotting by
  saveRDS("./Objects/TS.rds")                                                 # Save out time series in the folder above
toc()                                                                         # Stop timing
