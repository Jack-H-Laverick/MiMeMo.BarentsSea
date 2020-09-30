
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "tictoc", "furrr", "ncdf4")                # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("./Data/SPM/", full.names = TRUE, pattern = ".nc") %>%
  #example <- as.data.frame("./Data/ L3b_20190501-20190531__GLOB_4_AVW-MODVIR_SPM-OC5_MO_00.nc") %>% 
  #  rename(value = 1) %>% 
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c(NA, "Date"), 
           remove = FALSE, sep = "_") %>%                                   # Extract the Date from the file name
  mutate(Year = str_sub(Date, end = 4),                                     # Extract the Year
         Month = str_sub(Date, start = 5, end = 6)) %>%                     # Extract the Month
  select(-Date) %>% 
  rename(File = "value")

#### Functions ####

coordinates <- function(bin)            {
  # bin <- 690  
  index <- row[bin] - row[1]+1 
  lat_bin = center_lat[index]
  lon_bin = center_lon[index] + col[bin] * lon_step[index]
  
  Coords <- data.frame(Bin = bin, latitude = lat_bin, longitude = lon_bin)
  return(Coords) }                               # Convert bin indices to lat lon coordinates. Pain in the arse!

get_SPM <- function(File, Year, Month)  {
  
  #File <- all_files$File[1] ; Year <- all_files$Year[1] ; Month <- all_files$Month[1] # test
  
  SPM <- ncvar_get(nc_open(File), "SPM-OC5_mean")           # Import all SPM data, can't crop at import sadly
  
  Summary <- mutate(domains_mask, SPM = SPM[Bin],           # Select SPM values by index, gets around the annoying aspects of the data format!
                    Year = Year,
                    Month = Month) %>%           
  group_by(Month, Year, Shore) %>%        
    summarise(SPM = log(mean(SPM))) %>%                     # Average by time step. log for StrathE2E
    ungroup()  
  
  return(Summary)
}                               # Pull SPM and create monthly time series per zone

#### Spatial ####

raw <- nc_open(all_files$File[1])                                           # Open an example file
row <- raw$dim$row$vals                                                     # Extract spatial information to build a grid
col <- ncvar_get(raw, "col")                                                # Each row has a diferent number of columns
center_lat <- ncvar_get(raw, "center_lat")                                  # Center of each row
center_lon <- ncvar_get(raw, "center_lon")                                  # Center of the first column
lon_step <- ncvar_get(raw, "lon_step")                                      # The steps for increasing longitude per column

space <- future_map_dfr(1:length(col), coordinates, .progress = T)          # Get lat/Lons

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                  # Project to Lat Lons

domains_mask <- st_as_sf(space, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert grid to SF
  st_join(domains) %>%                                                      # Link to model domain 
  drop_na(Shore) %>%                                                        # Crop
  select(-c(Elevation, area)) %>%                                           # Drop uneeded information
  st_drop_geometry() %>%                                                    # Drop SF formatting
  mutate(Shore = as.factor(Shore))

grid <- st_as_sf(space, coords = c("longitude", "latitude"), crs = 4326, remove = F) 
#ggplot() + geom_sf(data = grid) + geom_sf(data = domains, fill = NA)
#plot(grid)

#ggplot(domains_mask) + geom_raster(aes(x=latitude, y = longitude, colour = Shore))

#### Extract SPM ####

tic()
SPM <- future_pmap_dfr(all_files, get_SPM, .progress = T) %>%               # Data extraction with parameters stored rowise in dataframe, executed in par
  mutate(Date = as.Date(paste("01", Month, Year, sep = "/"), format = "%d/%m/%Y")) # Get date column for plotting
saveRDS(SPM, "./Objects/Suspended particulate matter.rds")
toc()

#### Plot ####

ggplot(data = SPM) + 
  geom_line(aes(x = Date, y = SPM, colour = Shore), size = 0.25) +
  theme_minimal() +
  labs(y = expression("Suspended particulate matter log.g m"^{-3}*"Month"^{-1}), 
       caption = "GlobColour suspended particulate matter estimates from satellite") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/NEMO-MEDUSA/Suspended particulate matter.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")
