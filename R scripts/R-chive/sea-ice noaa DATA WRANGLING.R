
# Pull the contents of netcdf files

#### Set up ####
setwd("~/Data/Sea-Ice NOAA/Daily_Arctic_Sea_Ice")
rm(list=ls())                                                               # Wipe the brain
library(tidyverse)
library(ncdf4)                
library(data.table)
library(furrr)
plan(multiprocess)

## Interrogate the folder of Netcdf files downloaded from NOAA (Sea ice concentration)
all_files <- dir(recursive = TRUE) %>%                                      # List all file names in a folder (and sub-folders)
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
   separate(value, into = c("File", "Year", "Month", "End"), remove = FALSE,# Extract the year and month from the file name
   sep = c(-18, -14, -12)) %>%                                              # Seperate the string at a character position
    dplyr::select(-File, - End) %>%                                         # Drop clipped string ends
     mutate(Year = as.integer(Year), Month = as.integer(Month))             # Set times to integers

#### Pull the contents of the first Netcdf file ####

ff <- all_files[1,]$value                                                   # Take first file name

nc_raw <- nc_open(ff)                                                       # Open up a netcdf file to see it's raw contents (var names)
nc_lat <- ncvar_get(nc_raw, "latitude")                                     # Extract a matrix of all the latitudes
nc_lon <- ncvar_get(nc_raw, "longitude")                                    # Extract a matrix of all the longitudes
nc_conc <- ncvar_get(nc_raw, "goddard_nt_seaice_conc")                      # Extract a matrix of all the concentration estimates
nc_close(nc_raw)                                                            # You must close an open netcdf file when finished to avoid data loss

tidy_ll <- nc_lat %>%                                                       # Tidy lat-longs, grab latitudes
  as.numeric() %>%                                                          # Make numeric
  as_tibble() %>%                                                           # Convert to tibble/dataframe
  rename(Latitude = value) %>%                                              # Rename single column
  mutate(Longitude = as.numeric(nc_lon))                                    # Add in Longitudes

rm(nc_conc, nc_lat, nc_lon, nc_raw)                                         # Clean environment

#### Perform for all files ####

## R runs out of memory if you try to extract data from all the files at once while piping. So I've written a function
# Which performs the process piecemeal. Extract a months worth of data, and summarise before moving on to keep memory free.

read_ice <- function(ff){
  print(str_glue("getting sea ice concentration data from {ff}"))           # Print the name of the file being extracted
  nc_raw <- nc_open(ff)                                                     # Open file
  nc_conc <- ncvar_get(nc_raw, "goddard_nt_seaice_conc")                    # Extract concentration estimates
  nc_close(nc_raw)                                                          # Close file
  tidy_ll %>%                                                               # Grab the tidied dataframe of lat-longs
    mutate(Concentration = as.numeric(nc_conc))                                 # Append new variable to coordinates
  
}                               # Extract a variable for a file and append to lat-longs

Average_ice <- function(month) {
  
  
  Monthly <- month %>%                                          # Take the month
    mutate(data = purrr::map(value, read_ice)) %>%              # Extract data from each file
    unnest(data) %>%                                            # Extract all encoded data
    drop_na() %>%                                               # Drop NAs
    group_by(Longitude, Latitude, Year, Month) %>%              # Group by pixel and time step
    summarise(Sea_ice_avg = mean(Concentration)) %>%            # Calculate annual average sea ice at each pixel
    ungroup()                                                   # Ungroup for speed
  return(Monthly)  
  
}                        # Extract the data for a month and average

Average_year <- function (data) {
  
  Annual <- data %>%                                      # An empty slot in the list gets this years data
    group_by(Longitude, Latitude, Year) %>%                                 # Group by pixel and year
    summarise(Sea_ice_avg = mean(Sea_ice_avg)) %>%                         # Calculate annual average sea ice at each pixel
    ungroup()                                                             # Ungroup for speed
  
  return(Annual)
}                       # Average across the months once bound into a dataframe

Average_month <- function (data) {
  
  Annual <- data %>%                                        # An empty slot in the list gets this years data
    group_by(Longitude, Latitude, Month) %>%                # Group by pixel and month to get a seaonal cycle
    summarise(Sea_ice_avg = mean(Sea_ice_avg)) %>%                         # Calculate annual average sea ice at each pixel
    ungroup()                                                             # Ungroup for speed
  
  return(Annual)
}                       # Average across the months once bound into a dataframe

Ice <- all_files %>% 
       split(., f = list(.$Year, .$Month))                  # Create a list of dataframes for each separate month 

Ice <- Ice[sapply(Ice, function(x) dim(x)[1]) > 0] %>%      # Drop empty dataframes (Months which weren't observed but split introduces)
       future_map(Average_ice) %>%                          # Perform the extraction
       rbindlist()                                          # Collapse the list into a dataframe
 #     filter(Sea_ice_avg > 0) %>%                          # Drop pixels with average sea_ice cover of 0
  saveRDS(Ice, file = "Satellite_Ice_M.rds")              # Save Monthly summary
 #     Average_year() %>%                                   # Calculate annual averages
 #     filter(Sea_ice_avg > 0) %>%                          # Drop pixels with average sea_ice cover of 0
 # saveRDS(Ice, file = "Satellite_Ice_A.Rdata")             # Save Annual summary
                 
#### For GFW GIF ####
  
  Ice <- all_files %>%
  filter(between(Year, 2012, 2016)) %>% 
  split(., f = list(.$Year, .$Month))                  # Create a list of dataframes for each separate month 

Ice <- Ice[sapply(Ice, function(x) dim(x)[1]) > 0] %>% # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(Average_ice) %>%                          # Perform the extraction
  rbindlist() %>%                                      # Collapse the list into a dataframe
  Average_month() %>%                                  # Calculate seasonal average
  filter(Sea_ice_avg > 0) %>%                          # Drop pixels with average sea_ice cover of 0
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs = 3035)                             # Reproject
  
saveRDS(Ice, file = "Satellite_Ice_GFW.rds")           # Save Monthly summary
  
  

