
# Make a bathymetry and distance from shore gird which matches the NEMO-MEDUSA outputs

#### Set up ####

Tidy_packages <- c("tidyverse", "pbapply")                             # List handy data packages
Geo_packages <- c("sf", "rnaturalearth",  "RANN")                      # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE) # Load packages

nc_bath <- readRDS("Polar_Bathymetry_halfres.rds")                     # Get bathymetry

grid <- readRDS("~/Data/MiMeMo/MMM grid.rds") %>%                      # Get Nemo-Medusa Grid of lon lat from Roberts clipped data
  select(Longitude, Latitude)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%        # Get a world map
  filter(subregion %in% c("Northern America", "Northern Europe", "Eastern Europe")) %>%
  st_transform(crs = 3035)                                             # Assign polar projection

#### Match NEMO-MEDUSA points to GEBCO bathymetry points ####

closest <- nn2(nc_bath[,1:2], grid[,1:2], k = 1, searchtype = "priority") %>% # fast nearest neighbour search
  sapply(cbind) %>% as_tibble                                          # Format as dataframe

grid$Bathymetry<- nc_bath[closest$nn.idx,3]                            # Grab the depths by index

ggplot(grid, aes(x = Longitude, y = Latitude, colour = Bathymetry)) +  # Check the bathymetry looks believable
         geom_point()

#### Calculate distance form shore for NEMO-MEDUSA grid points ####

#grid <- readRDS("~/Data/NEMO - MEDUSA/GEBCO_grid2.rds")            

close <- st_as_sf(grid, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Set dataframe to SF format
  sf::st_transform(3035) 

dist <- st_distance(close, world) %>% pbapply(1,min)                   # Calculate distance from point to each polygon, pull the minimum
close$Shore_dist <- dist

#saveRDS(close, file = "~/Data/MiMeMo/MMM_GEBCO_grid.rds")             # Save
  