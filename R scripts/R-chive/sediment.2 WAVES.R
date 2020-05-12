
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "stars", "tictoc", "ncdf4")                # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

all_files <- list.files("./Data/ECMWF Waves/", full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%  
  rename(File = "value") %>% 
  mutate(Year = str_sub(File, start = -7, end = -4))

#### Functions ####

Window <- function(file, w, e, s, n)       {
  
  # file <- all_files[1,]$File ; w = 0 ; e = 180 ; s = 0 ; n = 90
  
  raw <- nc_open(file)
  lon <- raw$dim$longitude$vals %>% between(w, e)
  W <- min(which(lon == TRUE))
  E <- max(which(lon == TRUE))
  
  lat <- raw$dim$latitude$vals %>% between(s, n)
  S <- min(which(lat == TRUE))
  N <- max(which(lat == TRUE))
  
  lons <- raw$dim$lon$vals[W:E]
  lats <- raw$dim$lat$vals[S:N]
  
  Limits <- data.frame("Lon_start" = W, "Lon_count" = E - W + 1, "Lat_start" = S, "Lat_count" = N - S + 1)
  
  Limits <- list(Lats = lats, Lons = lons, Limits = Limits)
  return(Limits)
}    # Extract the positions to clip the netcdf file to, and the values for the smaller grid

mync_get <- function(File)                 {
  
  ncvar_get(nc_open(File), "swh", c(Space$Limits$Lon_start, Space$Limits$Lat_start, 1, 1),  # Extract the variable of interest
            c(Space$Limits$Lon_count, Space$Limits$Lat_count, -1, -1))                    # cropped to window, with all time steps and ensemble members
}    # Import a variable clipped to Window

get_waves <- function(File, Year)          {
  
  #File <- all_files$File[1] ; Year <- all_files$Year[1]                     # test
  
  Data <- mync_get(File)                                                     # Extract the variables of interest
  
  Summary <- as.data.frame.table(Data, responseName = "SWH") %>%   # Reshape array as dataframe
    rename(Longitude = Var1, Latitude = Var2, Number = Var3, Time = Var4) %>% # Name the columns
    mutate(Longitude = rep(rep(rep(Space$Lons,                               # Replace the factor levels with dimension values
                                   times = length(Space$Lats)), times = length(unique(Number))), times = length(unique(Time))),
           Latitude = rep(rep(rep(Space$Lats,  
                                  each = length(Space$Lons)), times = length(unique(Number))), times = length(unique(Time))),
           Number = rep(rep(1:length(unique(Number)), each = length(Space$Lats) * length(Space$Lons)), times = length(unique(Time))),
           Time = rep(1:length(unique(Time)), each = length(Space$Lats) * length(Space$Lons) * length(unique(Number)))) %>% 
    left_join(domains_mask) %>%                                              # Crop to domain
    drop_na() %>%         
    left_join(months) %>%                                                    # Crop to domain
    mutate(Year = Year) %>%                                                  # Attach Year
    group_by(Month, Year, Shore) %>%        
    summarise(SWH = weighted.mean(SWH, weights)) %>%                                           # Average by month
    ungroup()  
  
  return(Summary)
}    # Pull significant wave height monthly time series per zone in a year file

#### Spatial ####

Space <- Window(all_files[1,]$File, w = 0, e = 76, s = 65, n = 84)          # Get values to crop a netcdf file spatially at import. 

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

areas <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>%     # Get the data grid
  mutate(Dummy = 10) %>%                                                    # Add dummy data to convert to a stars grid
  st_as_stars()
st_crs(areas) <- st_crs(4326)                                               # set lat-lon crs
areas <- st_as_sf(areas, as_points = F, merge = F) %>%                      # Convert the stars grid into SF polygons 
  st_join(domains) %>%                                                      # Link to model domain 
  drop_na() %>%                                                             # Crop
  select(-c(Elevation, area, Dummy)) %>%                                    # Drop uneeded information
  mutate(weights = as.numeric(st_area(.)))                                  # Calculate the area of each cell for weighted averages

domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>% # Get the data grid
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>% # Convert to SF
  st_join(areas) %>%                                                        # Add the areas for weighting
  drop_na() %>%                                                             # Drop points which we didn't calculate weights for
  st_drop_geometry()                                                        # Drop SF formatting

ggplot(domains_mask) + geom_raster(aes(x = Longitude, y = Latitude, fill = Shore))

#### Extract significant wave height ####

months <- data.frame(Time = 1:96, Month = rep(1:12, each = 8))  

tic()
Waves <- pmap_dfr(all_files, get_waves) %>%                                        # Data extraction with parameters stored rowise in dataframe, executed in par
  mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y")) # Get date column for plotting
saveRDS(Waves, "./Objects/Significant wave height.rds")
toc()

#### Plot ####

ggplot(data = Waves) + 
  geom_line(aes(x = Date, y = SWH, colour = Shore), size = 0.25) +
  theme_minimal() +
  labs(y = "Height (m)", caption = "Monthly mean significant wave height from ECMWF") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/Significant wave height.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")
