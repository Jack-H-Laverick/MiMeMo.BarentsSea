
# Global Fishing Watch outputs daily estimates of fishing activity using a neural network based on VMS movement patterns.
# Data is annonymised and aggregated in 100th of a degree cells.

#### Set up                                           ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "furrr", "tictoc")   # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

plan(multiprocess)                                                            # Choose the method to parallelise by with furrr

Files <- data.frame(File = list.files(path = "./Data/GFW daily_csvs",         # Get a list of files to import 
                                      pattern ="*.csv", full.names = T)) %>% 
  separate(File, into = c(NA,NA,NA, "Date"), sep = "/", remove = F) %>%       # Pull the date from the file name
  separate(Date, into = c("Year", "Month",NA), sep = "-")                     # Get a month and year column for grouping time steps

FAO <- readRDS("./Objects/FAO.rds")                                           # Read in polygons for FAO regions of interest

#### Limit data to Arctic FAO regions                 ####
tic()
Arctic_fishing <- Files %>%                                                         
  split(list(Files$Year, Files$Month)) %>%                                    # Split files into chunks of each month
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                                 # Drop empty dataframes (Months which weren't observed but split introduces)
  map(~ {                                                                                 # For each data packet ->
    print(.x$File)                                                                        # Print the files being worked on
     future_map(as.character(.x$File), get_cropped_fishing, area = FAO, .progress= T) %>% # Apply clipping function to each daily csv file
       .[sapply(., function(x) dim(x)[1]) > 0] %>%                                        # Drop empty dataframes (Months with no fishing in the area)
       rbindlist() %>%                                                                    # Bind the month of data together
       separate(date, into = c("Year", "Month", "Day"), sep = "-") %>%                    # Split date column
       group_by(lon_bin, lat_bin, Region, geartype, Year, Month) %>%                     
       summarise(fishing = sum(fishing_hours)) %>%                                        # Count fishing hours in grids squares for the month 
       ungroup()                                                                   
  }) %>%                                                            # Import each month of fishing data
  rbindlist()                                                                 # Bind all the months together
toc()
#### Create seasonal grids                            ####
tic()
Arctic_fishing %>% 
  rename(long = lon_bin, lat = lat_bin) %>% 
  mutate(long = long / 100, lat = lat / 100) %>% 
  group_by(long, lat, Month, geartype) %>%                                    # Group by pixel, month, and gear
  summarise(fishing = mean(fishing)) %>%                                      # Average per month fishing hours in grid cells 
  ungroup() %>%            
  saveRDS(file = "./Objects/Seasonal.rds")
toc()

#### Create time series for each FAO area of interest ####

Arctic_fishing %>% 
  group_by(Region, Year, Month, geartype) %>%                                 # Group by area, time step, and gear
  summarise(Regional_fishing = sum(fishing)) %>%                              # Count fishing hours in an area
  saveRDS(file = "./Objects/Regional_ts.rds")

#### data.table version ####

## Seasonal
tic("DT way")
Arctic_fishing2 <- Arctic_fishing  
setDT(Arctic_fishing2, key = c("lon_bin", "lat_bin"))
test <- Arctic_fishing2[, by = .(lon_bin, lat_bin, Month, geartype), 
                        .(fishing = mean(fishing))][,
                                                    ':='(lon_bin = lon_bin / 100, lat_bin = lat_bin / 100)] %>% 
  saveRDS(file = "./Objects/Seasonal_dt.rds")
toc()

tic()
Arctic_fishing %>% 
  group_by(Region, Year, Month, geartype) %>%                                 # Group by area, time step, and gear
  summarise(Regional_fishing = sum(fishing)) %>%                              # Count fishing hours in an area
  saveRDS(file = "./Objects/Regional_ts_dt.rds")
toc()
