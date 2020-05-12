
# Open the GEBCO netcdf file and limit to a workable size, reshape for tidyverse, and extract contours

#### Set up ####

rm(list=ls())                                                   # Wipe the brain

packages <- c("tidyverse", "data.table", "ncdf4")               # List packages
lapply(packages, library, character.only = TRUE)                # Load packages

#### Pull the contents of Netcdf file ####

nc_raw <- nc_open("./Data/GEBCO_2019.nc")                       # Open up the netcdf file to see it's raw contents (var names)
nc_lat <- ncvar_get(nc_raw, "lat")                              # Extract a matrix of all the latitudes
nc_lon <- ncvar_get(nc_raw, "lon")                              # Extract a matrix of all the longitudes
nc_close(nc_raw)                                                # You must close an open netcdf file when finished to avoid data loss
rm(nc_raw)

#### Shrink to Arctic ####

## Things are a bit large to work with, so lets try a conservative crop of the data to help. -100,0,120,90 is visible in the polar projection I'm using

# Clip longitudes
W <- 0.5 * length(nc_lon) ; E <- 300/360 * length(nc_lon)       # What positions in the vector are 0 & 120 Degrees W?
nc_lon <-nc_lon[c(W:E)]                                         # Clip to mid longitudes

# Clip latitudes
S <- nrow(nc_lat) * 150/180 ; N <- nrow(nc_lat)                 # Where is 60 Degrees N to the North Pole?
nc_lat <- nc_lat[c(S:N)] %>%                                    # Clip to high latitude (60 - 90)
  as.data.frame()                                               # Convert array to dataframe

# Pull restricted bathymetry
nc_raw <- nc_open("./Data/GEBCO_2019.nc")                       # Open up the netcdf file to see it's raw contents (var names)
nc_bath <- ncvar_get(nc_raw, "elevation", start = c(W, S), 
                     count =c((E - W + 1), (N - S + 1)))        # Extract a matrix of all the concentration estimates, reading in to the clipped area
nc_close(nc_raw)                                                # You must close an open netcdf file when finished to avoid data loss
rm(nc_raw)

nc_bath <- as.data.frame(nc_bath)

#### Extract contours ####

Con_bath <- nc_bath[seq(1, nrow(nc_bath), 8), seq(1, ncol(nc_bath), 8)] # reduce load further
Con_lon <- nc_lon[seq(1, length(nc_lon), 8)]                    # reduce load
Con_lat <- nc_lat[seq(1, nrow(nc_lat), 8),]                     # reduce load

cont <- contourLines(Con_lon, Con_lat, as.matrix(Con_bath), 
                     levels = c(-30,-40,-50,-60,-200,-1000)) %>%# Extract contours
  lapply(data.frame, stringsAsFactors = FALSE) %>%              # Convert results to dataframes
  rbindlist(id = TRUE) %>%                                      # Bind a list of dataframes (the fast way) with an id column
  saveRDS(file = "./Objects/Bathymetry_lines.rds")

#### Reduce resolution and reshape the full bathymetry file ####

nc_bath <- nc_bath[,-1]                                         # First latitude is negative causing problems
nc_lat <- nc_lat[-1,]
colnames(nc_bath) <- nc_lat                                     # Move latitudes into column names for gather call later
nc_lon <- nc_lon[seq(1, length(nc_lon), 2)]                     # Select every other longitude

nc_bath <- nc_bath[seq(1, nrow(nc_bath), 2), seq(1, ncol(nc_bath), 2)] %>%     # take every other column and row to reduce load
  as.data.frame() %>%
  add_column(Longitude = nc_lon) %>%                            # Add in longitudes
  pivot_longer(-Longitude, names_to = "Latitude", values_to = "Elevation") %>% # format convert to long
  mutate(Latitude = as.numeric(Latitude))                       # Change column type
saveRDS(nc_bath, file = "./Objects/Bathymetry_points.rds")
