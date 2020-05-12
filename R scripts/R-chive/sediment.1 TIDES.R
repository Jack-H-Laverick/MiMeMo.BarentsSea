
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Tides.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr", "ncdf4") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop uneeded data which would get included in new NM files

SINMOD_mask <- readRDS("./Objects/SINMOD Targets.rds")                      # Import locations of traget pixels in SINMOD grid

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("path", "file"), sep = "_") %>%                  # Extract the year and month from the file name
  separate(file, into = c("year", "month"),  
           remove = FALSE, sep = 4) %>%                                     # Extract the year and month from the file name
  mutate(path = paste0(path, "_"),                                          # Replace the dropped separator
         month = str_sub(month, end = -4),                                  # Drop file extension to get number
         year = as.integer(year)) %>%                                       # Set time as integers 
  mutate(month = as.integer(month)) %>% 
  filter(!file %in% c("200301.nc", "ncdump") &                              # First time step isn't accessible, and don't want the dump file
          year < 2011) %>%                                                  # We only need data which overlaps with waves heights from ECMWF
  mutate(vars = list(c("u_east", "v_north")),                               # List the variables we want to extract
         start = list(c(min(SINMOD_mask$V1), min(SINMOD_mask$V2), 1, 1)),   # Instruct where to start reading from
         count = list(c(max(SINMOD_mask$V1)-min(SINMOD_mask$V1), max(SINMOD_mask$V2)-min(SINMOD_mask$V2), -1, -1))) # Instruct how long to read for (a rough first crop)

#### Space ####

# raw <- nc_open(paste0(all_files$path[1], all_files$file[1]))
# 
# ldepths <- ncvar_get(raw, "LayerDepths")
# depths <- ncvar_get(raw,  "depth") 
# 
# Lats <- ncvar_get(raw, "gridLats")
# Lons <- ncvar_get(raw, "gridLons")
# 
# coords <- reshape2::melt(Lons) %>% 
#   as.data.table() %>% 
#   .[as.data.table(reshape2::melt(Lats)), 
#     on = c("Var1", "Var2")] %>% 
#   setnames(c("V1", "V2", "Longitude", "Latitude")) %>% 
#   .[Latitude %between% c(65, 85) & Longitude %between% c(0,90)]  # Clip to points within rough crop
# 
# tic()
# voronoi <- st_as_sf(setDF(coords), coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>% 
#   voronoi_grid(st_transform(domains, crs = 4326)) %>% 
#   select(-geometry)
# toc()
# ggplot(voronoi) + geom_sf(aes(fill = Cell_area))

#### Function for a single month ####

get_tides <- function(year, month, path, file, vars, start, count, crop) {
  
timesteps <- seq(ISOdate(year = year, month = month, day=1, hour = 0, min = 1), by = "2 hours", length.out = 336) # Get values for timesteps

nc_raw <- ncdf4::nc_open(paste0(path, file))                                  # Open up a netcdf file to see it's raw contents (var names)

extracted <- map(vars, ~{                                                     # For each variable
  ncvar_get(nc_raw, varid = .x, start = start,                                # Import roughly cropped grid
                             count = count) %>% 
  apply(c(1,2,4), mean, na.rm=TRUE) %>%                                       # Average across all depths
  as.data.table(key = c("V1", "V2", "V3"), value.name = .x)})                 # Create a dataframe keyed in space and time and reset the variable name

bound <- merge(extracted[[1]], extracted[[2]]) %>%                            # Bind the two currents together
  .[,':='(V1 = V1 + min(coords$V1) - 1,                                       # Need to ensure the indices align between 
          V2 = V2 + min(coords$V2) - 1)]                                      # the cropping window and the newly imported data

clipped <- merge(as.data.table(crop, key = c("V1", "V2")), bound, allow.cartesian = TRUE) # Clip to model domain adding polygons
clipped[,':='(Time = timesteps[V3],                                           # Change timestep index to real time 
              V1 = NULL,                                                      # Drop indices we don't need anymore
              V2 = NULL,
              V3 = NULL)] %>%
  cbind(., vectors_2_direction(.$u_east, .$v_north))                         

saveRDS(object = clipped, file = paste0("./Objects/Tides/SINMOD-", year, "-", month, ".rds")) # Save out subset
}                  # Pull the depth averaged currents from a file

#### use function ####

tic()
test <- future_pmap(all_files, get_tides, crop = voronoi, .progress = TRUE)
toc()

#### plot a check #####

#trial_plot <- st_as_sf(setDF(clipped), sf_column_name = "geomc") # replace sf formatting to plot

#ggplot(filter(trial_plot, V3 == 1)) + geom_sf(aes(fill = u_east, geometry = geomc)) +
#  theme_minimal() 

# ggplot(trial_plot) + geom_sf(aes(fill = u_east, geometry = geomc), lwd = 0) +
#   theme_minimal() +
#   transition_manual(V3)

