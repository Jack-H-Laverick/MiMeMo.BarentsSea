
# Pull the contents of netcdf files and wrangle them into a time series in model compartments

#### Set up ####
setwd("/import/fish/southampton/combined/timestep/")  
rm(list=ls())                                                               # Wipe the brain
library(tidyverse)
library(ncdf4)  
library(data.table)
library(tictoc)                                                             # Time performance
library(parallel)
library(sf)
source("file:///home/alb19154/R scripts/domain FUNCTIONS.R")                # Bring in the functions I've written (we want the plotting ones)

tic("total")                                                                # Start timing the whole process
all_files <- dir() %>%                                                      # List all file names in a folder and below
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("Year", "Month", "Day", "type", "end"), 
           remove = FALSE, sep = c(4, 6, 8, 9)) %>%                         # Extract the year and month from the file name
  dplyr::select(-end) %>%
  mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) # Set time as integers

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")                   # Load Domain

#### Set the spatial environment ####

start.ref <- c(1,182) ; count.ref <- c(-1,264)                              # Spatial cropping at import for spatial (variables without depths or time)
start2D <- c(1,182,1) ; count2D <- c(-1,264,-1)                             # Spatial cropping at import for variables without depths
start3D <- c(1,182,1,1) ; count3D <- c(-1,264,27,-1)                        # Spatial cropping at import for variables with depths shallower than 400 m
start3DP <- c(1,182,1,1) ; count3DP <- c(-1,264,38,-1)                      # Spatial cropping at import for variables with depths shallower than 400 m (prediction file indexing)

Space <- get_spatial_clipped(all_files[1,]$value)                           # Pull space from the first historical file
Space.pred <- get_spatial_clipped(all_files[7777,]$value)                   # Pull space from the first prediction file

# output <- Space$nc_lat %>%                                                # Create a spatial dataframe, grab latitudes
#   as.numeric() %>%                                                        # Make numeric
#   as_tibble() %>%                                                         # Convert to tibble/dataframe
#   rename(Latitude = value) %>%                                            # Rename single column
#   mutate(Longitude = as.numeric(Space$nc_lon))                            # Add in Longitudes
# saveRDS(object = output, file = "~/Data/NEMO - MEDUSA/NMgrid2.rds")

output <- readRDS("~/Data/NEMO - MEDUSA/GEBCO_grid2.rds")                   # Load in Bathymetry pulled from GEBCO instead
mask_bathy <- matrix(abs(output$Bathymetry), 490, 264)                      # Create a bathymetry matrix

Shallow_mark <- between(Space$nc_depth[1:27], 0, 60)                        # Find the positions in the depth vector for shallow slice
Deep_mark <- Space$nc_depth[1:27] > 60                                      # Find the elements in the array to extract for deep slice
Shallow_mark_pred <- between(Space.pred$nc_depth[1:38], 0, 60)              # And for predictions
Deep_mark_pred <- Space.pred$nc_depth[1:38] > 60                            

sw <- get_weights(0, 60)                                                    # Work out water column proportions for weighted means                                                     s 
dw <- get_weights(60, 400)
swp <- get_weights.pred(0, 60)                                              # And for predictions                                                     s 
dwp <- get_weights.pred(60, 400)

#### Build the timeseries ####

Variables <- c("Salinity", "Temperature", "Ice", "DIN", "Chlorophyll", 
               "Turbocline", "Mixed", "Zonal", "Meridional")

depth_levels <- list(Shallow = c(paste("S", Variables, sep = "_"), "S"),    # Rename depth factor levels
                     Deep = c(paste("D", Variables, sep = "_"), "D")) 

predictions <- all_files %>%                                                
  filter(type %in% c("U", "V", "T","S") & Year > "2005") %>%                # Isolate files containing useful predictions
  mutate(type = paste0(type, ".pred"))                                      # Mark as predictions

tic("Extracting time series data from NetCDF files")                        # Time the data extraction

overnight <- all_files %>%  
  filter(!type %in% c("T", "W") & Year < "2006") %>%                        # 2006 projections have differently assigned variables
  rbind(., predictions) %>%                                                 # Add in prediction files with different format
  split(., f = list(.$type)) %>%                                            # Get a DF of file names for each type
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  lapply(time_series)                                                       # Perform the extraction and save objects
toc()                                                                       # Stop timing the data extraction

#### Collect the time series into a single object ####

#### Collect the time series into a single object ####

Predictions <- list.files(pattern = ".rds") %>%                             # Get list of .rds files
  .[grepl("domain.ts", .)] %>% .[grepl(".pred", .)] %>%                     # Get time series predictions
  purrr::map(readRDS) %>%                                                   # Read in data
  reduce(full_join)                                                         # Join together all the data packets

Historical <- list.files(pattern = ".rds") %>%                              # Get list of .rds files
  .[grepl("domain.ts", .)] %>%  .[!grepl(".pred", .)] %>%                   # Grab historic time series files
  purrr::map(readRDS) %>%                                                   # Read in data
  reduce(full_join)                                                         # Join together all the data packets

TS <- bind_rows(Historical, Predictions) %>%                                # Bind together datasets
  mutate(Current = Zonal + Meridional,                                      # Get the resultant current force without direction
         date = paste("15", Month, Year, sep = "/")) %>%                    # Create a single date column for plotting
  mutate(date = as.Date(date, format = "%d/%m/%Y"),                         # Reformat date column for ggplot                   
         Compartment = paste(Shore, Depth, sep = " ")) %>%                  # Build a single compartment column for plotting by
  saveRDS("TS")