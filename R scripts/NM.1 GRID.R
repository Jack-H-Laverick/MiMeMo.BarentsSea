
# Create a spatial grid to bind extracted NM model outputs to, including distance from shore and bathymetry
# Remember to mount the idrive by typing midrive into the Konsole

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("MiMeMo.tools", "tidyverse", "tictoc", "ncdf4", "pbapply") # List handy data packages
Geo_packages <- c("rnaturalearth", "sf", "stars", "RANN")                   # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  filter(subregion %in% c("Northern America", "Northern Europe", "Eastern Europe")) %>%
  st_transform(crs = 3035)                                                  # Assign polar projection

nc_bath <- readRDS("./Objects/Bathymetry_points.rds")  # Get bathymetry

File <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>% 
  .[1]                                                                      # Name an example NM file

Space <- get_spatial(File)                                                  # Pull space from example NM data file
Data <- read_ncdf(File)                                                     # Pull a variable to illustrate grid

#### Re-crop ALLARC grid ####

nc_raw <- nc_open("./Data/allarc_coordinates.nc")                           # This is the full grid before Robert cropped it and introduced 0s

LatALLARC <- ncvar_get(nc_raw, "nav_lat")                                   # Pull latitude
LonALLARC <- ncvar_get(nc_raw, "nav_lon")                                   # Pull longitude

#which(LatALLARC == y[1,1] & LonALLARC == x[1,1], arr.ind = TRUE)            # Cut out ALLARC columns which match MiMeMo CROP
#which(LatALLARC == y[1,ncol(y)] & LonALLARC == x[1,ncol(x)], arr.ind = TRUE)# Where in the big grid matches each corner of MiMeMo?
#which(LatALLARC == y[nrow(y),1] & LonALLARC == x[nrow(x),1], arr.ind = TRUE)  
#which(LatALLARC == y[nrow(y), ncol(y)] & LonALLARC == x[nrow(x),ncol(x)], arr.ind = TRUE)

crop_lat <- LatALLARC[1025:1259,208:397]                                    # Crop ALLARC to same limit as MiMeMo
crop_lon <- LonALLARC[1025:1259,208:397]                                    # The dimesions of the new matrices match MiMeMo        
values <- matrix(Data$votemper[,,5,], nrow=235, ncol=190)                   # Map example value onto the grid

unique(Space$nc_lon[crop_lon != Space$nc_lon])                              # Which values in the old matrix have been changed?
unique(Space$nc_lat[crop_lat != Space$nc_lat])                              # Looks like things worked, because we only lost the points with coords 0,0

s <- st_as_stars(values) %>%                                                # Pass matrix of extracted variable
  st_as_stars(curvilinear=list(X1 = crop_lon, X2 = crop_lat)) %>%           # Pass coordinate matrices and start the grid is curved
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                            # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  st_transform(crs = crs)                                                  # Reproject

ggplot() + geom_sf(data = s, aes(fill = A1), colour = NA) +
  geom_sf(data = world) +
  zoom
  
#### Convert to a dataframe ####

grid <- crop_lat %>%                                                        # Create a spatial dataframe, grab latitudes
  as.numeric() %>%                                                          # Make numeric
  as_tibble() %>%                                                           # Convert to tibble/dataframe
  rename(Latitude = value) %>%                                              # Rename single column
  mutate(Longitude = as.numeric(crop_lon))                                  # Add in Longitudes

#### Extract GEBCO bathymetry at points ####

closest <- nn2(nc_bath[,c(2,1)], grid[,1:2], k = 1, searchtype = "priority") %>% # Fast nearest neighbour search
  sapply(cbind) %>% as_tibble                                               # Format as dataframe

Depths <- nc_bath[["Elevation"]] %>%                                        # Select the depths
  .[closest$nn.idx]  

grid <- mutate(grid, Bathymetry = Depths)                                   # Add to grid

ggplot(grid, aes(x = Longitude, y = Latitude, colour = Bathymetry)) +       # Check the bathymetry looks believable
  geom_point()

#### Calculate distance from shore for points ####

grid <- st_as_sf(grid, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Set dataframe to SF format
  st_transform(crs) 

dist <- st_distance(grid, world) %>% pbapply(1,min)                         # Calculate distance from point to each polygon, pull the minimum
grid$Shore_dist <- dist

ggplot() +                                                                  # Check distances look believable
  geom_sf(data = grid, aes(colour = Shore_dist)) +                   
  geom_sf(data = world) +
  zoom

saveRDS(grid, file = "./Objects/Fixed_grid.rds")                            # Save
