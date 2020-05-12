
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Currents/")  # Marker so network script can see where the data is saving to (it's buried in a function)

#### Set up ####
  
rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "data.table", "ncdf4",  "furrr", "tictoc", "radiant.data") # List handy data packages
Geo_packages <- c("sf")                                                     # List GIS package           
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/NM.z FUNCTIONS.R")                                      # Bring in the functions I've written

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domains <- readRDS("./Objects/Domains.rds")                                 # Load SF polygons of the MiMeMo model domains

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("Path", "File"), sep = 61) %>%                   # Extract the year and month from the file name
  separate(File, into = c("Type", "Date"), 
           remove = FALSE, sep = -11) %>%                                   # Extract the year and month from the file name
  mutate(Date = str_sub(Date, end = -4))                                    # Drop file extension to get number

all_files[which(all_files$Date == "_2047105"),]$Type <- "grid_T_"           # One file has been incorrectly labelled, I've corrected the name. Now all timesteps have 6 files
all_files[which(all_files$Date == "_2047105"),]$Date <- "20471005"          

all_files  <- all_files %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = c(4, 6)) %>%       # Extract the year and month from the file name 
  mutate(Year = as.integer(Year),                                           # Set time as integers 
         Month = as.integer(Month), 
         Day = as.integer(Day))  

examples <- group_by(all_files, Type) %>% slice(1)                          # Get one example for each file type
trial <- group_by(all_files, Type) %>% slice(1:3)                           # Get one example for each file type

#### Set the spatial environment ####

Space <- get_spatial(paste0(examples[1,"Path"], examples[1,"File"]))        # Pull space from the first file

W <- nc_open(paste0(examples[4,"Path"], examples[4,"File"]))                # Get the different depth vector for W files
DepthsW <- W$dim$depthw$vals ; nc_close(W) ; rm(W) 

output <- readRDS("./Objects/Fixed_grid.rds")                               # Load in Bathymetry pulled from GEBCO
mask_bathy <- matrix(abs(output$Bathymetry), 235, 190)                      # Create a bathymetry matrix

Shallow_mark <- between(Space$nc_depth[1:38], 0, 60)                        # Find the positions in the depth vector for shallow slice
Deep_mark <- Space$nc_depth[1:38] > 60                                      # Find the elements in the array to extract for deep slice
Shallow_mark_W <- between(DepthsW[1:39], 0, 60)                             # And for W files
Deep_mark_W <- DepthsW[1:39] > 60                            

sw <- get_weights(0, 60)                                                    # Work out water column proportions for weighted means                                                      
dw <- get_weights(60, 400)
sww <- get_weights.W(0, 60)                                                 # And for W files                                                      
dww <- get_weights.W(60, 400)

start3D <- c(1,1,1,1) ; count3D <- c(-1,-1,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
start3DW <- c(1,1,1,1) ; count3DW <- c(-1,-1,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)

#### Build the monthly sumaries ####

tic("Creating monthly current objects from netcdf files")                   # Time the data extraction

overnight <- filter(all_files, Type %in% c("grid_V_", "grid_U_")) %>%       # Reduce to 2D current data
#  slice(c(1:144)) %>%                                                      # A subset which runs in 60 seconds to test
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(Big_Currents, .progress = T)                                   # Perform the extraction and save an object for each month (in parallel)
toc()                                                                       # Stop timing 
