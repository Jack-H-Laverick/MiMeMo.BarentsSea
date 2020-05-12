
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
  mutate(wave_x = x_index - min(x_index) + 1,                               # Adjust indices to sample at now we cropped the files
         wave_y = y_index - min(y_index) + 1) %>% 
  select(-c(x_index, y_index))

SINMOD_mask <- readRDS("./Objects/SINMOD Targets.rds") %>%                  # Import locations of traget pixels in SINMOD grid
  mutate(tide_x = V1 - min(V1) + 1,                                         # Adjust indices to sample at now we cropped the files
         tide_y = V2- min(V2) + 1) %>% 
  select(-c(V1, V2))

#### Align grids ####

## Which ECMWF and SINMOD cells fall on the sediment/Global Fishing Watch grid?

GFW_grid <- st_make_grid(domains, what = "centers", cellsize = 0.1) %>%     # Make a 0.1 degree grid across the model domain
  st_sf(dummy = 1)                                                          # Convert tp sf for joining

targets1 <- st_join(GFW_grid, select(ECMWF_mask, wave_x, wave_y, Shore))
targets2 <- st_join(GFW_grid, select(SINMOD_mask, tide_x, tide_y, Shore))

aligned <- cbind(targets1, targets2[,c("tide_x", "tide_y")]) %>% 
  select(-c(dummy, ..1)) %>% 
  sfc_as_cols(names = c("Longitude", "Latitude")) %>% 
  st_drop_geometry()

tide_pixels <- select(aligned, tide_x, tide_y) %>%                          # Which are the unique grid cells we need to calculate for?
  distinct()                                                                # We can save time indexing duplicate cells later

wave_pixels <- select(aligned, wave_x, wave_y) %>% 
  distinct()

#ggplot(targets2) + geom_sf()

#### Waves ####

raw <- nc_open("./Data/ECMWF_Waves/waves.nc")   # Open file containing all the data
swh <-  ncvar_get(raw, "swh")                   # Import significant wave height
mwp <-  ncvar_get(raw, "mwp")                   # Import mean wave period
mwd <-  ncvar_get(raw, "mwd")                   # Import mean wave direction
nc_close(raw)                                   # Close file

empty <- apply(swh, c(1,2), empty)              # Which pixels never have waves (probably land), same result for three variables

swh[is.na(swh)] <- 0                            # replace NA's with 0, ice means no waves, not NA for me
mwp[is.na(mwp)] <- 0
mwd[is.na(mwd)] <- 0

swh[empty] <- NA                                # Put back NA's on pixels which never have waves
mwp[empty] <- NA
mwd[empty] <- NA

Wave_step <- seq(ISOdate(2003, 01, 1, 0), by = "3 hours", length.out = length(swh[40,40,])*28) # Calculate time steps for the dataset

day_2_month <- function(vec, chunk_length) {
  
  split(vec, ceiling(seq_along(vec)/chunk_length)) %>% 
    map(rep, times = 28) %>% 
    unlist() }

wave_ts <- function(swh, mwp, mwd, Wave_step) {
  
  month <- cbind(day_2_month(swh, 8), 
                 day_2_month(mwp, 8), 
                 day_2_month(mwd, 8))

  wave_ts <- matrix(t(month), ncol = ncol(month), byrow = TRUE) # Duplicate the months to get a daily tace (quickly)
  
  colnames(wave_ts) <- c("swh", "mwp", "mwd")          # reinstate coumn names

  wave_ts <- zoo::zoo(wave_ts, Wave_step)       # Add time steps and convert to zoo object
}

Wave_list <- map2(wave_pixels$wave_x, wave_pixels$wave_y,                             # For each unique pixel
                         ~ wave_ts(swh[.x,.y,], mwp[.x,.y,], mwd[.x,.y,], Wave_step)) # Pull a time series of wave data

saveRDS(Wave_list, "./Objects/Wave_ts.rds")     # Save out list of time series

rm(swh, mwp, mwd, Wave_list)                    # Clear memory

#### Tides ####

raw <- nc_open("./Data/SINMOD/tides.nc")   # Open file containing all the data
u <-  ncvar_get(raw, "u_east")             # Import u tidal currents
v <-  ncvar_get(raw, "u_east")             # Import v tidal currents
nc_close(raw)                              # Close file

empty <- apply(u, c(1,2), empty)           # Which pixels never have water movement (probably land), same result for two variables

u[is.na(u)] <- 0                           # replace NA's with 0, unless there's never water movement, a pixel shold be still when NA
v[is.na(v)] <- 0

u[empty] <- NA                             # Put back NA's on pixels which never have water movement
v[empty] <- NA

Tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = length(u[40,40,])) # Calculate time steps for the dataset

tide_ts <- function(u, v) {
  
  tide_ts <- zoo::zoo(vectors_2_direction(u, v), # Convert UV to speed and direction
                      Tide_step)                 # Convert to zoo object to align time series
}            # Convert UV vectors to a time series of speed and direction

Tide_list <- map2(tide_pixels$tide_x, tide_pixels$tide_y,         # For each unique pixel
                  ~ tide_ts(u[.x,.y,], v[.x,.y,]))                # Pull a time series of tide data

saveRDS(Tide_list, "./Objects/Tide_ts.rds")

#### Create look up table ####

Water_pairs <- left_join(aligned, mutate(tide_pixels, tide_entry = row_number())) %>% 
  left_join(mutate(wave_pixels, wave_entry = row_number())) %>% 
  select(-ends_with(c("_y", "_x")))
                         
saveRDS(Water_pairs, "./Objects/Water look up table.rds")
