
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "tictoc", "ncdf4", "furrr")# List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

wave_files <- list.files("./Data/ECMWF_Waves/", full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%  
  rename(File = "value") %>% 
  mutate(Year = as.numeric(str_sub(File, start = -7, end = -4)))

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                  # Transform to Lat/Lon to match other objects

ECMWF_mask <- readRDS("./Objects/ECMWF Targets.rds") %>%                    # Import locations of traget pixels in ECMWF grid
  mutate(wave_x = x_index - min(x_index) + 1,                              # Adjust indices to sample at now we cropped the files
         wave_y = y_index - min(y_index) + 1) %>% 
  select(-c(x_index, y_index))

SINMOD_mask <- readRDS("./Objects/SINMOD Targets.rds") %>%                  # Import locations of traget pixels in SINMOD grid
  mutate(tide_x = V1 - min(V1) + 1,                                         # Adjust indices to sample at now we cropped the files
         tide_y = V2- min(V2) + 1) %>% 
  select(-c(V1, V2))

#### Align grids ####

## Which ECMWF and SINMOD cells fall on the sediment/Global Fishing Watch grid?
# Need to increase resolution to 0.01 to match GFW once working

GFW_grid <- st_make_grid(domains, what = "centers", cellsize = 0.1) %>%     # Make a 0.1 degree grid across the model domain
  st_sf(dummy = 1)                                                          # Convert tp sf for joining

targets1 <- st_join(GFW_grid, select(ECMWF_mask, wave_x, wave_y, Shore))
targets2 <- st_join(GFW_grid, select(SINMOD_mask, tide_x, tide_y, Shore))

aligned <- cbind(targets1, targets2[,c("tide_x", "tide_y")]) %>% 
  select(-c(dummy, ..1)) %>% 
  sfc_as_cols(names = c("Longitude", "Latitude")) %>% 
  st_drop_geometry()

aligned_minimal <- select(aligned, wave_x, wave_y, tide_x, tide_y) %>%      # Which are the unique grid cells we need to calculate for?
  distinct()                                                                # We can save time by just copying duplicate cells

#ggplot(targets2) + geom_sf()

#### Waves ####

mync_get_pixel <- function(File, var, pixel_x, pixel_y)                 {
  
  raw <- nc_open(File)
  ncvar_get(raw, var, c(pixel_x, pixel_y, 1, 1),  # Extract the variable of interest
            c(1, 1, -1, -1)) %>%                            # cropped to a target pixel, with all time steps and ensemble members
    colSums(na.rm = TRUE)                                   # Average across ensemble quickly
  nc_close(raw)                                              # Setting na.rm = TRUE replaces iced NAs with 0s
}    # Import a variable clipped to Window

get_waves_ts <- function(File, vars, Year, pixel_x, pixel_y)          {
  
  #  File <- all_files$File[1] ; Year <- all_files$Year[1]                           # test
  #  pixel_lon <- ECMWF_mask$Longitude[1] ; pixel_lat <- ECMWF_mask$Latitude[1]  # test
  #  pixel_x <- ECMWF_mask$x_index[1] ; pixel_y <- ECMWF_mask$y_index[1]         # test
  #  vars <- c("swh", "mwd")                                                         # test
  
  Data <- map(vars, ~{mync_get_pixel(File, var = .x, pixel_x, pixel_y)})                   # Extract the variables of interest
  Data <- cbind(Data[[1]], Data[[2]], Data[[3]])
  colnames(Data) <- vars  
  
  TS <- cbind(Data,
              Year = rep(Year, nrow(Data)),
              Month = rep(1:12, each = 8),
              Hour = rep(seq(0, 21, by = 3), times = 12))
  
  return(TS)
}    # Pull significant wave height monthly time series per zone in a year file

wave_ts <- function(wave_x, wave_y) {
  
  months <- map2(wave_files$File, wave_files$Year, get_waves_ts, vars = c("swh", "mwd", "mwp"), # For each year file.
                 pixel_x = wave_x, pixel_y = wave_y) %>%                       # Extract the time series for a pixel  
    do.call(rbind, .)                                                          # Bind the years together quickly
  
  days <- matrix(rep(t(months), 28), ncol = ncol(months), byrow = TRUE) %>%    # Duplicate the months to get a daily tace (quickly)
    cbind(Day = rep(1:28, each = nrow(.)/28))
  
  colnames(days) <- c(colnames(months), "Day")  
  
  Wave_step <- ISOdate(days[,"Year"], days[,"Month"], days[,"Day"], days[,"Hour"])     # Calculate time step
  
  wave_ts <- zoo::zoo(days[, c("swh", "mwd", "mwp")],      # Create time series of wave variales
                      Wave_step)                           # Specify time steps for the zoo object
} 

tic()
test <- wave_ts(aligned$wave_x[20000], aligned$wave_y[20000])
toc()

#### Waves 2 ####

raw <- nc_open("./Data/ECMWF_Waves/waves.nc")   # Open file containing all the data
swh <-  ncvar_get(raw, "swh")                   # Import significant wave height
mwp <-  ncvar_get(raw, "mwp")                   # Import mean wave period
mwd <-  ncvar_get(raw, "mwd")                   # Import mean wave direction
nc_close(raw)                                   # Close file

Wave_step <- seq(ISOdate(2003, 01, 1, 0), by = "3 hours", length.out = length(swh[40,40,])*28) # Calculate time steps for the dataset

day_2_month <- function(vec, chunk_length) {
  
  split(vec, ceiling(seq_along(vec)/chunk_length)) %>% 
    map(rep, times = 28) %>% 
    unlist() }

wave_ts2 <- function(swh, mwp, mwd, Wave_step) {
  
  month <- cbind(day_2_month(swh, 8), 
                 day_2_month(mwp, 8), 
                 day_2_month(mwd, 8))

  wave_ts <- matrix(t(month), ncol = ncol(month), byrow = TRUE) # Duplicate the months to get a daily tace (quickly)
  
  colnames(wave_ts) <- c("swh", "mwp", "mwd")          # reinstate coumn names

  wave_ts <- zoo::zoo(wave_ts, Wave_step)       # Add time steps and convert to zoo object
}

profvis::profvis({
test <- wave_ts2(swh[200,70,], mwp[200,70,], mwd[200,70,], Wave_step)
})

profvis::profvis({
  Wave_list <- map2(aligned_minimal$wave_x, aligned_minimal$wave_y,
                           ~ wave_ts2(swh[.x,.y,], mwp[.x,.y,], mwd[.x,.y,], Wave_step))
})

# ** incorrect periodicity, repearing the whole ts back to back 28 times, not each month 28 times.
#### Tides ####

raw <- nc_open("./Data/SINMOD/tides.nc")   # Open file containing all the data
u <-  ncvar_get(raw, "u_east")             # Import u tidal currents
v <-  ncvar_get(raw, "u_east")             # Import v tidal currents
nc_close(raw)                              # Close file

Tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = length(u[40,40,])) # Calculate time steps for the dataset

tide_ts <- function(u, v) {
  
  tide_ts <- zoo::zoo(vectors_2_direction(u, v),     # Convert UV to speed and direction
                      Tide_step)                     # Convert to zoo object to align time series
}            # Convert UV vectors to a time series of speed and direction

test <- tide_ts(u[40, 40,], v[40, 40,])    # Convert the time series for a single spatial pixel
plot(test)

# Some pixels are all NAs in SINMOD

#### align ####

align_stress <- function (tide_ts, wave_ts, depth = 40) {
  
  #  align <- merge(tide_ts, wave_ts)                                # Match up time series (identify missing steps)
  
  align <- merge(tide_ts, wave_ts, all = c(T,T))                   # Match up time series (identify missing steps)
  
  align$mwd <- zoo::na.approx(align$mwd, rule = 2)                 # Interpolate wave direction onto tide time steps
  align$swh <- zoo::na.approx(align$mwd, rule = 2)                 # Interpolate wave height onto tide time steps
  align$mwp <- zoo::na.approx(align$mwd, rule = 2)                 # Interpolate wave period onto tide time steps
  
  align <- align[which(zoo::index(align) %in% zoo::index(tide_ts)),] # Keep only tide time steps
  
  ## CALCULATE BED SHEAR STRESS ##
  
  stress <- bedshear::shear_stress(
    bathymetry = depth,                                            # Depth to sea floor
    D50 = 0.02,                                                    # Nominal median grain size
    tidal_velocity = align[,"uvSpeed"],                            # Water movements
    tidal_direction = align[,"uvDirection"], 
    wave_height = align[,"swh"],
    wave_period = align[,"mwp"],
    wave_direction = align[,"mwd"]/10,
    switch = 0) %>% 
    dplyr::mutate(Time_step = zoo::index(align))                   # Add time 
  
  return(stress)  
}

profvis({
  tides <- tide_ts(u[aligned$tide_x[20000], aligned$tide_y[20000],], v[aligned$tide_x[20000], aligned$tide_y[20000],])
  waves <- wave_ts(aligned$wave_x[20000], aligned$wave_y[20000])
  look <- align_stress(tides, waves, depth = 40)
})


ggplot(look) + geom_line(aes(x = Time_step, y = shear_mean))


#### looping over pixels? ####

profvis({
  test <- pmap(aligned[20000:20005, c("tide_x", "tide_y", "wave_x", "wave_y")], 
               function(tide_x, tide_y, wave_x, wave_y) {
                 tides <- tide_ts(u[tide_x, tide_y,], v[tide_x, tide_y,])
                 waves <- wave_ts(wave_x, wave_y)
                 stress <- align_stress(tides, waves, depth = 40) })
})

## Hits memory limit
# profvis({
#   test <- future_pmap(aligned[20000:20005, c("tide_x", "tide_y", "wave_x", "wave_y")], 
#                function(tide_x, tide_y, wave_x, wave_y) {
#                  tides <- tide_ts(u[tide_x, tide_y,], v[tide_x, tide_y,])
#                  waves <- wave_ts(wave_x, wave_y)
#                  stress <- align_stress(tides, waves, depth = 40) })
# })

tic()
tide_pixels <- select(aligned, tide_x, tide_y) %>% 
  distinct() %>% # drop duplicated calls for pixels
  pmap(function(tide_x, tide_y) tide_ts(u[tide_x, tide_y,], v[tide_x, tide_y,])) # Get tidal time series for each pixel
toc()

profvis::profvis({
wave_pixels <- select(aligned, wave_x, wave_y) %>% 
  distinct() %>%   # drop duplicated calls for pixels
  slice(1:100) %>% 
  pmap(wave_ts)    # Get wave time series for each unique sampled pixel
})

## too slow, make 3 large combined wave arrays and get a list of time series like you have for tides.