
# readRDS("./Objects/Months/")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                                # Wipe the brain

Tidy_packages <- c("MiMeMo.tools", "tidyverse", "viridis", "data.table", "furrr", "tictoc") # List handy data packages
Geo_packages <- c("stars", "sf", "rnaturalearth")                            # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)       # Load packages
source("./R scripts/@_Region file.R")                                        # Define project region 

plan(multiprocess)                                                           # Choose the method to parallelise by with furrr

Transects <- list(S = readRDS("./Objects/Boundary_transects.rds") %>%        # Import transects to sample at
                       filter(Shore == "Offshore" | Neighbour != "Offshore") %>% # Drop Inshore to Offshore transects(avoid double accounting with Offshore to inshore transects)
                       split(list(.$current)), 
                  D = readRDS("./Objects/Boundary_transects.rds") %>%        # Import transects to sample at
                      filter(Shore == "Offshore" & Neighbour == "Ocean") %>% # The deep layer of the model only has exchange between offshore and the sea (and a vertical component dealt with separately)
                      split(list(.$current)))

grid <- readRDS("./Objects/Fixed_grid.rds")                                  # Joining data to the grid reorders the datapoints
lat <- matrix(grid$Latitude, nrow=235, ncol=190)                             # Reshape to a matrix for stars
lon <- matrix(grid$Longitude, nrow=235, ncol=190)                                  

#### Create a spatial object to bind data to ####

raster <- pull(grid, Bathymetry) %>%                                         # Extract the values in a current column
  matrix(nrow=235, ncol=190) %>%                                             # Convert current data to matrix for stars
  st_as_stars() %>%                                                          # Set up stars object
  st_as_stars(curvilinear=list(X1 = lon, X2 = lat)) %>%                      # Pass coordinate matrices and state the grid is curved
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                             # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  st_transform(crs = crs)                                                    # Reproject

Data_small <- readRDS("./Objects/Months/NM.1.1980.rds") %>% 
  ungroup() %>%                                                              # Looks like I forgot to ungroup before saving
  select(Bathymetry, Longitude, Latitude, Depth) %>% 
  filter(Depth == "S") %>%                                                   # Seperate shallow and deep data
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Specify original projection (crs) 
  st_transform(crs = crs) 

cells <- st_join(raster, Data_small) %>%  
  drop_na() %>%                                                              # Shrink grid to NM dataset
  select(Longitude, Latitude)

#### Intersections ####

Intersections <- list(S = list(Zonal = st_intersects(Transects[["S"]][["Zonal"]], cells),
                               Meridional = st_intersects(Transects[["S"]][["Meridional"]], cells)),
                      D = list(Zonal = st_intersects(Transects[["D"]][["Zonal"]], cells),
                               Meridional = st_intersects(Transects[["D"]][["Meridional"]], cells)))

Transects_DF <- map(Transects, ~{ map(.x, st_drop_geometry)})    # Drop SF formatting to play nicely
cells <- st_drop_geometry(cells)

#### Extract water exchanges between horizontal compartments ####

tic()
Flows <- list.files("./Objects/Months/", full.names = T) %>%               # Get the names of all data files
  future_map(Sample, transects = Transects_DF, intersections = Intersections, .progress = T) %>% # Sample the currents in each file and aggregate
   rbindlist() %>%                                                         # Bind into a dataframe
   saveRDS("./Objects/H-Flows.rds")                                        # Save
toc()
# 1401.244, 23.35 minutes
  