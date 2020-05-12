
# Create a spatial grid to bind extracted NM model outputs to, including distance from shore and bathymetry

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

File <- list.files("../../../../import/fish/southampton/combined/timestep/", pattern = "P.nc", full.names = TRUE) %>% 
  .[1]                                                                      # Name an example NM file

Space <- get_spatial(File)                                                  # Pull space from example NM data file
Data <- read_ncdf(File)                                                     # Pull a variable to illustrate grid

#### Check grid ####

values <- matrix(Data$DET[,,5,], nrow= 490, ncol=445)                       # Map example value onto the grid
 
s <- st_as_stars(values) %>%                                                # Pass matrix of extracted variable
  st_as_stars(curvilinear=list(X1 = Space$nc_lon, X2 = Space$nc_lat)) %>%   # Pass coordinate matrices and start the grid is curved
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                            # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  st_transform(crs = crs)                                                   # Reproject

ggplot() + geom_sf(data = s, aes(fill = A1), colour = NA) +
  geom_sf(data = world) +
  zoom 
  
#### Convert to a dataframe ####

grid <- as.numeric(Space$nc_lat) %>%                                        # Create a spatial dataframe, grab latitudes
  as.numeric() %>%                                                          # Make numeric
  as_tibble() %>%                                                           # Convert to tibble/dataframe
  rename(Latitude = value) %>%                                              # Rename single column
  mutate(Longitude = as.numeric(Space$nc_lon))                              # Add in Longitudes

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

saveRDS(grid, file = "./Objects/grid_detritus.rds")                         # Save
