
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "tictoc", "ncdf4")         # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

wave_files <- list.files("./Data/ECMWF Waves/", full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%  
  rename(File = "value") %>% 
  mutate(Year = as.numeric(str_sub(File, start = -7, end = -4)))

tide_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("path", "file"), sep = "_") %>%                  # Extract the year and month from the file name
  separate(file, into = c("year", "month"),  
           remove = FALSE, sep = 4) %>%                                     # Extract the year and month from the file name
  mutate(path = paste0(path, "_"),                                          # Replace the dropped separator
         month = str_sub(month, end = -4),                                  # Drop file extension to get number
         year = as.integer(year)) %>%                                       # Set time as integers 
  mutate(month = as.integer(month)) %>% 
  filter(!file %in% c("200301.nc", "ncdump") &                              # First time step isn't accessible, and don't want the dump file
           year < 2011)                                                     # We only need data which overlaps with waves heights from ECMWF
  
domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                  # Transform to Lat/Lon to match other objects

ECMWF_mask <- readRDS("./Objects/ECMWF Targets.rds")                        # Import locations of traget pixels in ECMWF grid

SINMOD_mask <- readRDS("./Objects/SINMOD Targets.rds")                      # Import locations of traget pixels in SINMOD grid

#### Align grids ####

## Which ECMWF and SINMOD cells fall on the sediment/Global Fishing Watch grid?

GFW_grid <- st_make_grid(ECMWF_mask, what = "centers", cellsize = 0.01)      # Make a 0.01 degree grid across the model domain
plot(GFW_grid, axes = TRUE)

look <- st_coordinates(GFW_grid)

test2 <- st_make_grid(SINMOD_mask, what = "centers", cellsize = 0.01)
look2 <- st_coordinates(test2)

identical(look, look2)

test3 <- st_make_grid(domains, what = "centers", cellsize = 0.01)
look3 <- st_coordinates(test2)

identical(look3, look2)

#### Waves ####

mync_get_pixel <- function(File, var, pixel_x, pixel_y)                 {
  
  ncvar_get(nc_open(File), var, c(pixel_x, pixel_y, 1, 1),  # Extract the variable of interest
            c(1, 1, -1, -1)) %>%                            # cropped to a target pixel, with all time steps and ensemble members
    colSums(na.rm = TRUE)                                   # Average across ensemble quickly
                                                            # Setting na.rm = TRUE replaces iced NAs with 0s
}    # Import a variable clipped to Window

get_waves_ts <- function(File, vars, Year, pixel_lon, pixel_lat, pixel_x, pixel_y)          {
  
  #File <- all_files$File[1] ; Year <- all_files$Year[1]                           # test
  #pixel_lon <- ECMWF_mask$Longitude[1] ; pixel_lat <- ECMWF_mask$Latitude[1]  # test
  #pixel_x <- ECMWF_mask$x_index[1] ; pixel_y <- ECMWF_mask$y_index[1]         # test
  #vars <- c("swh", "mwd")                                                         # test
  
  Data <- map(vars, ~{mync_get_pixel(File, var = .x, pixel_x, pixel_y)})                   # Extract the variables of interest
  Data <- cbind(Data[[1]], Data[[2]])
colnames(Data) <- vars  

  TS <- cbind(Data,
              Longitude = rep(pixel_lon, 96),                                            # Replace the factor levels with dimension values
              Latitude = rep(pixel_lat),
              Year = rep(Year),
              Month = rep(1:12, each = 8),
              Hour = rep(seq(0, 21, by = 3), times = 12))
    #       Time_step = ISOdate(year = Year, month = Month, day=1, hour = Hour, min = 1))                                                      # Attach Year
    
  return(TS)
}    # Pull significant wave height monthly time series per zone in a year file

sample_waves <- function(pixel) {
  
  months <- map2(all_files$File, all_files$Year, get_waves_ts, vars = c("swh", "mwd"), # For each year file.
                 pixel_lon = ECMWF_mask$Longitude[[pixel]], pixel_lat = ECMWF_mask$Latitude[pixel], # Extract the time series for a pixel  
                 pixel_x = ECMWF_mask$x_index[pixel], pixel_y = ECMWF_mask$y_index[pixel]) %>% 
    do.call(rbind, .)                                                                  # Bind the years together quickly
  
  days <- matrix(rep(t(months), 28), ncol = ncol(months), byrow = TRUE) %>%            # Duplicate the months to get a daily tace (quickly)
    cbind(Day = rep(1:28, each = nrow(.)/28))
  
  colnames(days) <- c(colnames(months), "Day")  
  
  Time_step <- ISOdate(days[,"Year"], days[,"Month"], days[,"Day"], days[,"Hour"])     # Calculate time step
  
  days <- data.frame(days[, !colnames(days) %in% c("Year", "Month", "Day", "Hour")], Time_step)  # Replace time columns with time step
} 

tic()
test <- sample_waves(500)
toc()

ggplot(test) + geom_line(aes(x = as.POSIXct(Time_step), y = swh))

#### Tides ####


#### Extract significant wave height ####

#timesteps <- seq(ISOdate(year = year, month = 1, day=1, hour = 0, min = 1), by = "2 hours", length.out = 336) # Get values for timesteps
# Trying to align with tides, which uses a 28 day month

# tic()
# Waves <- pmap_dfr(all_files, get_waves) %>%                                        # Data extraction with parameters stored rowise in dataframe, executed in par
#   mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y")) # Get date column for plotting
# saveRDS(Waves, "./Objects/Significant wave height.rds")
# toc()

#### Plot ####

# ggplot(data = Waves) + 
#   geom_line(aes(x = Date, y = SWH, colour = Shore), size = 0.25) +
#   theme_minimal() +
#   labs(y = "Height (m)", caption = "Monthly mean significant wave height from ECMWF") +
#   theme(legend.position = "top") +
#   NULL
# 
# ggsave("./Figures/Significant wave height.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")

#### testing ####


# library(zoo)
# 
# interpolated_s <- zoo(full_ts$SWH, full_ts$Timestep) %>% 
#   zoo::na.spline()
# 
# plot(interpolated_s)
# 
# interpolated_l <- zoo(full_ts$SWH, full_ts$Timestep) %>% 
#   zoo::na.approx()
# 
# plot(interpolated_l)

