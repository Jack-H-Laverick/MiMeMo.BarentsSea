
#### example ####

# https://www.jla-data.net/eng/spatial-aggregation/

library(sf)
library(dplyr)
library(ggplot2)

# NC counties - a shapefile shipped with the sf package
counties <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = T) %>% 
  select(BIR79)  %>% # a single variable: births for period 1979-84
  st_transform(crs = 6543) %>%  # a quaint local projection
  mutate(cty_area = units::drop_units(st_area(.))) # county area (in feet)

# NC state borders
state <- counties %>% 
  summarize()

# three semi-random cities in NC
points <- data.frame(name = c("Raleigh", "Greensboro", "Wilmington"),
                     x = c(-78.633333, -79.819444, -77.912222),
                     y = c(35.766667, 36.08, 34.223333)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(crs = st_crs(counties))  # same as counties


voronoi_grid <- function(points, area) {
  voronoi <- points %>% 
    st_geometry() %>%                                   # To get sfc from sf
    st_union() %>%                                      # To get a sfc of MULTIPOINT type
    st_voronoi(envelope = st_geometry(area)) %>%        # Voronoi polygon for the area
    st_collection_extract(type = "POLYGON") %>%         # A list of polygons
    st_sf() %>%                                         # From list to sf object
    st_intersection(state) %>%                          # Cut to shape of NC state
    st_join(points) %>%                                 # put names back
    mutate(Cell_area = units::drop_units(st_area(.)))   # Area of each polygon
}


voronoi <- voronoi_grid(points, state)

# display the Voronoi polygons
ggplot() +
  geom_sf(data = voronoi, aes(fill = Cell_area), alpha = .5) +
  geom_sf(data = state, lwd = .75, fill = NA) + 
  geom_sf(data = points, color = "red", pch = 4, size = 2) +
  labs(fill = "Zone")


#### Mine ####


rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "tictoc", "furrr", "ncdf4")                # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("./Data/Light and air temp", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c(NA, "Year", NA), 
           remove = FALSE, sep = "_y") %>%                                  # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                # Drop file extension to get number
  separate(value, into = c(NA, NA, NA, "Type", NA, NA), 
           remove = FALSE, sep = "[/_]") %>%                                  # Extract the data type and period from the file name
  rename(File = "value")

examples <- group_by(all_files, Type) %>% slice(1) %>% ungroup()            # Get one example for each file type

Space <- Window(examples[1,]$File, w = 0, e = 76, s = 65, n = 84)           # Get values to crop a netcdf file spatially at import. Both data types share a grid

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)



tic("voronoi")
domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>% # Get the data grid
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) #%>% # Convert to SF
#   st_join(domains) %>%                                                      # Link to model domain 
#   drop_na() %>%                                                             # Crop
#   select(-c(Elevation, area)) #%>% 
# #voronoi_grid(domains[2,])                                                 # Weight points within the model domain
#ggplot() + geom_sf(data = domains_mask, aes(fill = Cell_area))
toc()

test <- map(1:2, ~{voronoi_grid(domains_mask, domains[.x,])}) %>% 
  bind_rows() %>%                                               # Weight points within the model domain
  sf::st_sf(geomc = .$geometry, crs = 4326)                      # Reinstate attributes of the geometry column

ggplot() + geom_sf(data = test, aes(fill = Cell_area))


test <- voronoi_grid(domains_mask, domains)  

ggplot() + geom_sf(data = test, aes(fill = Cell_area, colour = Shore))


points <- domains_mask
area <- domains[1,] 

ggplot() +
  geom_sf(data = area) +
  geom_sf(data = points)

voronoi <- points %>% 
  st_geometry() %>%                                   # To get sfc from sf
  st_union() %>%                                      # To get a sfc of MULTIPOINT type
  st_voronoi(envelope = st_geometry(area)) %>%        # Voronoi polygon for the area
  st_collection_extract(type = "POLYGON") %>%         # A list of polygons
  st_sf() %>%                                         # From list to sf object
  st_join(points) %>%                                 # put names back
  st_intersection(area) %>%                          # Cut to shape of NC state
  mutate(Cell_area = units::drop_units(st_area(.)))   # Area of each polygon

  ggplot() +
    #geom_sf(data = area) +
    geom_sf(data = voronoi) +
    geom_sf(data = points) +
    NULL
  
  
  area <- domains 
  
  test <- purrr::map(1:nrow(area), ~{
    voronoi <- points %>% 
      st_geometry() %>%                                   # To get sfc from sf
      st_union() %>%                                      # To get a sfc of MULTIPOINT type
      st_voronoi(envelope = st_geometry(area[.x,])) %>%        # Voronoi polygon for the area
      st_collection_extract(type = "POLYGON") %>%         # A list of polygons
      st_sf() %>%                                         # From list to sf object
      st_join(points) %>%                                 # put names back
      st_intersection(area[.x,]) %>%                          # Cut to shape of NC state
      mutate(Cell_area = units::drop_units(st_area(.)))   # Area of each polygon
  }) %>% 
    dplyr::bind_rows() %>%                                            # Weight points within the model domain
    sf::st_sf(geomc = .$geometry, crs = 4326)
  
  ggplot() +
    #geom_sf(data = area) +
    geom_sf(data = test, aes(fill = Shore, alpha = 0.1)) +
    geom_sf(data = points) +
    NULL
  