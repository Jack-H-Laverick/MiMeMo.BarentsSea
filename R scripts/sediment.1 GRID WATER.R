
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "tictoc", "ncdf4")         # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

ECMF_example <- list.files("./Data/ECMWF Waves/", full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%
  slice(1) %>% 
  rename(File = "value") %>% 
  mutate(Year = str_sub(File, start = -7, end = -4))

SINMOD_example <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("path", "file"), sep = "_") %>%                  # Extract the year and month from the file name
  mutate(path = paste0(path, "_")) %>%                                      # Replace the dropped separator
  filter(!file %in% c("200301.nc", "ncdump")) %>%                           # First time step isn't accessible, and don't want the dump file
  slice(1)                                                                  # Take 1 file as an example

#### Functions ####

Window <- function(file, w, e, s, n) {
  
  # file <- all_files[1,]$File ; w = 0 ; e = 180 ; s = 0 ; n = 90
  
  raw <- ncdf4::nc_open(file)
  lon <- raw$dim$longitude$vals %>% dplyr::between(w, e)
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
} # Extract the positions to clip the netcdf file to, and the values for the smaller grid

#### Waves ECMWF ####

Space <- Window(all_files[1,]$File, w = 0, e = 76, s = 65, n = 84)          # Get values to crop a netcdf file spatially at import. 

tic()
domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>%  # Get the data grid
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Convert to SF
  voronoi_grid(area = select(domains, Shore)) %>%                               # Calculate overlap with domain and share of area per pixel
  mutate(x_index = match(Longitude, Space$Lons) + Space$Limits$Lon_start - 1,   # Find x index in the netcdf file each pixel sits at
         y_index = match(Latitude, Space$Lats) + Space$Limits$Lat_start - 1)    # Find y index in the netcdf file each pixel sits at
toc()

ggplot(domains_mask) + geom_sf(aes(fill = Shore), lwd = 0.1)                # Check

saveRDS(domains_mask, "./Objects/ECMWF Targets.rds")                        # Save

#### Tides SINMOD ####

raw <- nc_open(paste0(SINMOD_example$path[1], SINMOD_example$file[1]))      # Open example file

ldepths <- ncvar_get(raw, "LayerDepths")                                    # Check a variable
depths <- ncvar_get(raw,  "depth")                                          # Check a variable

Lats <- ncvar_get(raw, "gridLats")                                          # Pull latitudes
Lons <- ncvar_get(raw, "gridLons")                                          # Pull longitudes

coords <- reshape2::melt(Lons) %>%                                          # Reshape the matrix to a 3 column data.frame
  as.data.table() %>%                                                       # Convert to a data.table for speed
  .[as.data.table(reshape2::melt(Lats)),                                    # Bind latitudes treated in the same way
    on = c("Var1", "Var2")] %>%                                             # using the matrix indices
  setnames(c("V1", "V2", "Longitude", "Latitude")) %>%                      # Rename
  .[Latitude %between% c(65, 85) & Longitude %between% c(0,90)]             # Clip to points within rough crop

voronoi <- st_as_sf(setDF(coords), coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>% # Convert to sf 
  voronoi_grid(st_transform(domains, crs = 4326)) %>%                       # Work out the share of model domain for each SINMOD pixel
  select(-geometry)                                                         # drop repeated geometry column

ggplot(voronoi) + geom_sf(aes(fill = Cell_area))                            # Check

saveRDS(voronoi, "./Objects/SINMOD Targets.rds")                            # Save
