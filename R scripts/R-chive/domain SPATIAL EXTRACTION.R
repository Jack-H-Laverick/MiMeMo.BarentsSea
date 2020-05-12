
# Pull the contents of netcdf files

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

#### Build the spatial dataframes ####

predictions <- all_files %>%                                                
  filter(type %in% c("U", "V", "T","S") & Year > "2005") %>%                # Isolate files containing useful predictions
  mutate(type = paste0(type, ".pred"))                                      # Mark as predictions

tic("NetCDF file extractions")                                              # Time the data extraction

overnight <- all_files %>%  
  filter(!type %in% c("T","W") & Year < "2006") %>%                         # 2006 projections have differently assigned variables
#  slice(1:40) %>%
  rbind(., predictions) %>%                                                 # Add in prediction files with different format
  split(., f = list(.$type)) %>%                                            # Get a DF of file names for each type
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  lapply(combine)                                                           # Perform the extraction and save objects
toc()                                                                       # Stop timing the data extraction

#### Reshape for plotting - set up ####

setwd("~/Data/NEMO - MEDUSA")
rm(list=ls())     
library(mapproj)
library(rnaturalearth)
library(sf)
library(rgdal)
source("file:///home/alb19154/R scripts/domain FUNCTIONS.R")                # Bring in the functions I've written
source("file:///home/alb19154/R scripts/domain LISTS.R")                    # Bring in verbose lists and vectors for data wrangling

#### Perform the reshaping for point plots ####

tic("Reshaping")                                                            # Begin timing going from wide to long format
Tidy_data <- list(chained("domain.sp.S.rds", strip.s),                      # Put all the data in one place for manipulating without exceeding row limit
                  chained("domain.sp.P.rds", strip.p)) %>%         
  unlist(recursive = FALSE)                                                 # Remove the top layer of the nested list

final <- Tidy_data[c(1,2,6,7)] %>%  
  lapply(split_depth) %>% unlist(recursive = FALSE) %>%                     # Split list elements by depth, remove the top layer of the nested list
  list(., Tidy_data[c(3:5)]) %>% unlist(recursive = FALSE) %>%              # Add back in unstratified DFs
  lapply(split_decade) %>% unlist(recursive = FALSE) %>%                    # Split by decade
  saveRDS(., "domain.SP.rds")
toc()                                                                       # Finish timing the reshaping
toc()                                                                       # Finish timing whole script

#### Perform the reshaping for current plots ####

Merid_pred <- readRDS(file = "domain.sp.V.pred.rds")                        # Read in predictions for Zonal currents
  
Meridional <- readRDS(file = "domain.sp.V.rds") %>%                         # Read in Zonal currents
  rbind(., Merid_pred) %>%                                                  # Add in predictions
  gather(key = "depth", value = "Meridional", S_Meridional:D_Meridional) %>%# Reshape for ease in a plotting function 
  mutate(depth = as.factor(depth))
levels(Meridional$depth) <- depth_levels 

Zonal_pred <- readRDS(file = "domain.sp.U.pred.rds")                        # Read in predictions for Meridional currents

Zonal <- readRDS(file = "domain.sp.U.rds") %>%                              # Read in Meridional currents
  rbind(., Zonal_pred) %>%                                                  # Add in predictions
  gather(key = "depth", value = "Zonal", S_Zonal:D_Zonal) %>%               # Reshape for ease in a plotting function
  mutate(depth = as.factor(depth))
levels(Zonal$depth) <- depth_levels 

Currents <- left_join(Zonal, Meridional) %>%                                # Join to Meridional currents
  reproj() %>%                                                              # reproject
  sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
  st_set_geometry(NULL)                                                     # chuck out geometry column

levels(Currents$Month) <- Month_list   

Currents <- group_by(Currents, decade, depth, Shore, Region, Bathymetry, Shore_dist, x, y) %>%
  summarise(Zonal = mean(Zonal, na.rm = TRUE), 
            Meridional = mean(Meridional, na.rm = TRUE)) %>%                # We only need annual current maps
  ungroup() %>%                                                             # Remember to do this! It's a killer everytime...
  split(., f = list(.$depth, .$decade)) %>%                                 # Get a DF of file names for subset
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  saveRDS("domain.SP.currents.rds")

