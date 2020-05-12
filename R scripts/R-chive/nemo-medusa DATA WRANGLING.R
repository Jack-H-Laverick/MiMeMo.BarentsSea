
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
source("file:///home/alb19154/R scripts/nemo-medusa FUNCTIONS.R")           # Bring in the functions I've written (we want the plotting ones)

tic("total")                                                                # Start timing the whole process
all_files <- dir() %>%                                                      # List all file names in a folder and below
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("Year", "Month", "Day", "type", "end"), 
    remove = FALSE, sep = c(4, 6, 8, 9)) %>%                                # Extract the year and month from the file name
  dplyr::select(-end) %>%
  mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) # Set time as integers

#### Depth layer checks ####

## The mbathy data isn't actually depths, it's another index system of ~75 values (available in the prediction files). 
## If I want to calculate distance to the sea bed I'm going to have to use GEBCO data sampled to the N-M lat-lon coords to 
## make my own mask.
#
#mesh_mask <- nc_open("/import/fish/southampton/projection/grid/crop_mesh_mask.nc")                                     
#mask_bathy <-ncvar_get(mesh_mask, "mbathy")
#nc_close(mesh_mask)

#rm(mesh_mask)             

## Projections and historical have different depth projections, could well have different lat lons too, worth checking in the future
#test_files <- filter(all_files, !type %in% c("U","V","W"))                  # These files don't contain depths
#test_files <- filter(all_files, type == "S")                                # These files don't contain depths

#test <- sapply(test_files$value, get_depth) %>%                             # Pull the vector of depths from each netcdf file
#  .[!duplicated(lapply(., sort))]                                           # Check for unique depth vectors,

#### Check how much of the spatial environment we actually need to extract ####

# Space <- get_spatial(all_files[1,]$value)                                   # Pull space from the first file
# 
# #output <- Space$nc_lat %>%                                                 # Create a spatial dataframe, grab latitudes
# #  as.numeric() %>%                                                         # Make numeric
# #  as_tibble() %>%                                                          # Convert to tibble/dataframe
# #  rename(Latitude = value) %>%                                             # Rename single column
# #  mutate(Longitude = as.numeric(Space$nc_lon),
# #         Bathymetry = as.numeric(mask_bathy))                              # Add in Longitudes and bathymetry
# #saveRDS(object = output, file = "~/Data/NEMO - MEDUSA/NMgrid.rds")
# output <- readRDS("~/Data/NEMO - MEDUSA/GEBCO_grid.rds")                    # Load in Bathymetry pulled from GEBCO instead
# mask_bathy <- matrix(abs(output$Bathymetry), 490, 445)                      # Create a bathymetry matrix
# 
# extent <- st_as_sf(output, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%           # Specify original projection (crs) 
#   st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
#   sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
#   st_set_geometry(NULL) %>%                                                 # chuck out geometry column
#   filter(between(x, 2600000, 6300000) & between(y, 4000000, 7700000))       # Remove arrows you're not going to see anyway
#   
# max_lon <- max(extent$Longitude) ; min_lon <- min(extent$Longitude) 
# max_lat <- max(extent$Latitude) ; min_lat <- min(extent$Latitude) 
# 
# ## Check to see how much of the net_cdf file needs to be read in to get the spatial window of interest (I checked, it's the same for predictions files)
# check1 <- which(Space$nc_lat < max_lat, arr.ind=T)                          # Take everything
# check2 <- which(Space$nc_lat > min_lat, arr.ind=T)                          # Start at column 182 until the last column 
# check3 <- which(Space$nc_lon < max_lon, arr.ind=T)                          # Take everything
# check4 <- which(Space$nc_lon > min_lon, arr.ind=T)                          # Take everything

#### Set the spatial environment ####

start.ref <- c(1,182) ; count.ref <- c(-1,264)                              # Spatial cropping at import for spatial (variables without depths or time)
start2D <- c(1,182,1) ; count2D <- c(-1,264,-1)                             # Spatial cropping at import for variables without depths
start3D <- c(1,182,1,1) ; count3D <- c(-1,264,-1,-1)                        # Spatial cropping at import for variables with depths

Space <- get_spatial_clipped(all_files[1,]$value)                                   # Pull space from the first file

# output <- Space$nc_lat %>%                                                 # Create a spatial dataframe, grab latitudes
#   as.numeric() %>%                                                         # Make numeric
#   as_tibble() %>%                                                          # Convert to tibble/dataframe
#   rename(Latitude = value) %>%                                             # Rename single column
#   mutate(Longitude = as.numeric(Space$nc_lon))                             # Add in Longitudes
# saveRDS(object = output, file = "~/Data/NEMO - MEDUSA/NMgrid2.rds")

output <- readRDS("~/Data/NEMO - MEDUSA/GEBCO_grid2.rds")                  # Load in Bathymetry pulled from GEBCO instead
mask_bathy <- matrix(abs(output$Bathymetry), 490, 264)                     # Create a bathymetry matrix

output <- st_as_sf(output, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Specify original projection (crs) 
  st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
  sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
  st_set_geometry(NULL)                                                     # chuck out geometry column

Shallow_mark <- Space$nc_depth > 0 & Space$nc_depth < 30                    # Find the positions in the depth vector for shallow slice
Middle_mark <-  Space$nc_depth > 30 & Space$nc_depth <200                   # Find the positions for mid-water slice
Deep_mark <-    Space$nc_depth > 200 & Space$nc_depth < 1000                # Find the elements in the array to extract for deep slice

Space.pred <- get_spatial(all_files[7777,]$value)                           # Pull space from the first file

Shallow_mark_pred <- Space.pred$nc_depth > 0 & Space.pred$nc_depth < 30     # Find the positions in the depth vector for shallow slice
Middle_mark_pred <-  Space.pred$nc_depth > 30 & Space.pred$nc_depth <200    # Find the positions for mid-water slice
Deep_mark_pred <-    Space.pred$nc_depth > 200 & Space.pred$nc_depth < 1000 # Find the elements in the array to extract for deep slice

sw <- get_weights(0, 30)                                                    # Work out water column proportions for weighted means                                                     s 
mw <- get_weights(30, 200)                                                  # Get the weights averaging different slice
dw <- get_weights(200, 1000)

swp <- get_weights.pred(0, 30)                                              # Work out water column proportions for weighted means                                                     s 
mwp <- get_weights.pred(30, 200)                                            # Get the weights averaging different slice
dwp <- get_weights.pred(200, 1000)

## New output file to bind to, can't just filter to x y values, as this causes mismatch between NCvar lengths and the main file

#limited_Lons <- matrix(output$Longitude, 490, 445) %>% .[,182:ncol(.)]      # Create a clipped set of longitudes
#limited_Lats <- matrix(output$Latitude, 490, 445) %>% .[,182:ncol(.)]       
#mask_bathy <- matrix(abs(output$Bathymetry), 490, 445) %>% .[,182:ncol(.)]  

# output <- data.frame(Longitude = as.numeric(limited_Lons),
#                      Latitude = as.numeric(limited_Lats),
#                      Bathymetry = as.numeric(mask_bathy)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Specify original projection (crs) 
#   st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
#   sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
#   st_set_geometry(NULL)                                                     # chuck out geometry column
  
#### Build the dataframe ####

predictions <- all_files %>%                                                
  filter(type %in% c("U", "V", "T","S") & Year > "2005") %>%                # Isolate files containing useful predictions
  mutate(type = paste0(type, ".pred"))                                      # Mark as predictions

tic("NetCDF file extractions")                                              # Time the data extraction

overnight <- all_files %>%  
  filter(!type %in% c("T","W") & Year < "2006") %>%                         # 2006 projections have differently assigned variables
  rbind(., predictions) %>%                                                 # Add in prediction files with different format
  split(., f = list(.$type)) %>%                                            # Get a DF of file names for each type
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  lapply(combine)                                                           # Perform the extraction and save objects
#  mclapply(combine, mc.cores = getOption("mc.cores", 7L))                  # Parallelised over 7 of the 8 cores
toc()                                                                       # Stop timing the data extraction

#### Reshape for plotting - set up ####

setwd("~/Data/NEMO - MEDUSA")
rm(list=ls())     
library(mapproj)
library(rnaturalearth)
library(sf)
library(rgdal)
source("file:///home/alb19154/R scripts/nemo-medusa FUNCTIONS.R")           # Bring in the functions I've written (we want the plotting ones)

Month_list <- list("January"="1","February"="2","March" = "3",              # Name months for plotting
                   "April"="4","May"="5","June"="6","July"="7","August"="8",
                   "September"="9","October"="10","November"="11","December"="12")

Variables <- c("Salinity", "Temperature", "Ice", "DIN", "Chlorophyll", "Turbocline", "Mixed", "Zonal", "Meridional")
depth_levels <- list(Shallow = c(paste("S", Variables, sep = "_")),         # Rename depth factor levels
                     Mid = c(paste("M", Variables, sep = "_")), 
                     Deep = c(paste("D", Variables, sep = "_"))) 

strip.s <- list(Salinity = c(paste(c("S","M","D"), "Salinity", sep = "_")), # Groups of columns to convert into long dataframe
                Temperature = c(paste(c("S","M","D"), "Temperature", sep = "_")),
                Ice = "S_Ice", Turbocline = "S_Turbocline", Mixed = "S_Mixed") 

strip.p <- list(DIN = c(paste(c("S","M","D"), "DIN", sep = "_")),           # Groups of columns to convert into long dataframe
                Chlorophyll = c(paste(c("S","M","D"), "Chlorophyll", sep = "_")))

#### Perform the reshaping for point plots ####
tic("Reshaping")                                                            # Begin timing going from wide to long format
Tidy_data <- list(chained("weighted.S.rds", strip.s),            # Put all the data in one place for manipulating without exceeding row limit
                  chained("weighted.P.rds", strip.p)) %>%         
  unlist(recursive = FALSE)                                                 # Remove the top layer of the nested list

final <- Tidy_data[c(1,2,6,7)] %>%  
  lapply(split_depth) %>% unlist(recursive = FALSE) %>%                     # Split list elements by depth, remove the top layer of the nested list
  list(., Tidy_data[c(3:5)]) %>% unlist(recursive = FALSE) %>%              # Add back in unstratified DFs
  lapply(split_decade) %>% unlist(recursive = FALSE) %>%                    # Split by decade
  saveRDS(., "Tidy_data weighted.rds")
toc()                                                                       # Finish timing the reshaping
toc()                                                                       # Finish timing whole script

##** think how best to add in prediction data

#### Perform the reshaping for current plots ####

Merid_pred <- readRDS(file = "weighted.V.pred.rds")                         # Read in predictions for Zonal currents
  
Meridional <- readRDS(file = "weighted.V.rds") %>%                          # Read in Zonal currents
  rbind(., Merid_pred) %>%                                                  # Add in predictions
  gather(key = "depth", value = "Meridional", S_Meridional:D_Meridional) %>%# Reshape for ease in a plotting function 
  mutate(depth = as.factor(depth))
levels(Meridional$depth) <- depth_levels 

Zonal_pred <- readRDS(file = "weighted.U.pred.rds")                         # Read in predictions for Meridional currents

Zonal <- readRDS(file = "weighted.U.rds") %>%                               # Read in Meridional currents
  rbind(., Zonal_pred) %>%                                                  # Add in predictions
  gather(key = "depth", value = "Zonal", S_Zonal:D_Zonal) %>%               # Reshape for ease in a plotting function
  mutate(depth = as.factor(depth))
levels(Zonal$depth) <- depth_levels 

Currents <- left_join(Zonal, Meridional) %>%                                # Join to Meridional currents
  reproj() %>%                                                              # reproject
  sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
  st_set_geometry(NULL)                                                     # chuck out geometry column

levels(Currents$Month) <- Month_list   

Currents <- group_by(Currents, decade, depth, x, y) %>%
  summarise(Zonal = mean(Zonal, na.rm = TRUE), 
            Meridional = mean(Meridional, na.rm = TRUE)) %>%                # We only need annual current maps
  ungroup() %>%                                                             # Remember to do this! It's a killer everytime...
  filter(between(x, 2600000, 6300000) & between(y, 4000000, 7700000)) %>%   # Remove arrows you're not going to see anyway
  split(., f = list(.$depth, .$decade)) %>%                                 # Get a DF of file names for subset
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  saveRDS("weighted.Currents.rds")

Currents <- readRDS("weighted.Currents.rds")

#### Dirty code for chasing down NAs ####

#  nc_raw <- nc_open(all_files[2,]$value)                                   # Open up a netcdf file to see it's raw contents (var names)
#  nc_saline <- ncvar_get(nc_raw, "vosaline", start3D, count3D)                               # Extract a matrix of salinities
#  nc_ice <- ncvar_get(nc_raw, "soicecov", start2D, count2D)                    # Extract a matrix of ice fractions
#  nc_close(nc_raw)                                                         # You must close an open netcdf file when finished to avoid data loss

#check <- get_sea(all_files[2,]$value)  
#ggplot(output, aes(x = Longitude, y = Latitude)) + geom_point()