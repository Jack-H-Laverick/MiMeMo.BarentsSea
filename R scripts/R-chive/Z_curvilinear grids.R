
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "furrr", "tictoc", "viridis", "ggnewscale", "ncdf4") # List handy data packages
Geo_packages <- c("mapproj", "rnaturalearth", "sf", "rgdal", "stars")       # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

#### Play with ALLARC grid ####

nc_raw <- nc_open("~/allarc_coordinates.nc")                                # This is the full grid before Robert cropped it and introduced 0s

LatALLARC <- ncvar_get(nc_raw, "nav_lat")                                   # Pull latitude
LonALLARC <- ncvar_get(nc_raw, "nav_lon")                                   # Pull longitude

which(LatALLARC == y[1,1] & LonALLARC == x[1,1], arr.ind = TRUE)            # Cut out ALLARC columns which match MiMeMo CROP
which(LatALLARC == y[1,ncol(y)] & LonALLARC == x[1,ncol(x)], arr.ind = TRUE)# Where in the big grid matches each corner of MiMeMo?
which(LatALLARC == y[nrow(y),1] & LonALLARC == x[nrow(x),1], arr.ind = TRUE)  
which(LatALLARC == y[nrow(y), ncol(y)] & LonALLARC == x[nrow(x),ncol(x)], arr.ind = TRUE)

crop_lat <- LatALLARC[1025:1259,208:397]                                    # Crop ALLARC to same limit as MiMeMo
crop_lon <- LonALLARC[1025:1259,208:397]                                    # The dimesions of the new matrices match MiMeMo        
values = matrix(output$Bathymetry, nrow=235, ncol=190)                      # Map example value onto the grid

unique(Space$nc_lon[crop_lon != Space$nc_lon])                              # Which values in the old matrix have been changed?
unique(Space$nc_lat[crop_lat != Space$nc_lat])                              # Looks like things worked, because we only lost the points with coords 0,0

s <- st_as_stars(values) %>%                                                # Pass matrix of extracted variable
  st_as_stars(curvilinear=list(X1 = crop_lon, X2 = crop_lat))               # Pass coordinate matrices and start the grid is curved

# So geom_stars doesn't like a curvilinear grid, convert to SF polygons as a work around
hack <- st_as_sf(s, as_points = FALSE, merge = FALSE) %>%                   # Convert each raster cell to it's own polygon
  st_transform(crs = 3035)                                                  # Reproject

ggplot() + geom_sf(data = hack, aes(fill = A1), colour = NA) +
  geom_sf(data = world) +
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) 
  
