
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr")                                   # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to paralelise by with furrr

all_files <- list.files("./Data/Rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  mutate(Year = str_sub(value, start = -7, end = -4)) %>%                   # Drop file extension to get number
  rename(File = "value")

domains <- readRDS("./Objects/Domains.rds")                                 # Import domain polygon

crs = 3035                                                                  # Preferred CRS for your region

# Get an example set of points to plot
example <- raster::raster(all_files$File[1], varname = "nav_lat") %>% 
  raster::as.data.frame(xy = TRUE) %>% 
  dplyr::left_join(raster::as.data.frame(raster::raster(all_files$File[1], varname = "nav_lon"), xy = TRUE)) %>% 
  dplyr::left_join(raster::as.data.frame(raster::raster(all_files$File[1], varname = "sorunoff"), xy = TRUE)) %>% 
  sf::st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>% 
  dplyr::select(-c(x,y)) %>% 
  tidyr::drop_na()
  
#### Check domain overlaps point estimates of river runoff ####

overlap <- ggplot() +
#  geom_sf(data = river_expansion, fill = "orange", colour = "orange") +        # Uncomment me once you've built your extra polygons
  geom_sf(data = domains, fill = "red", colour = "red") +
  geom_sf(data = st_transform(example, crs = crs), aes(colour = runoff_land_into_ocean), size = 0.2) +
  theme_minimal() +
  zoom +
  NULL
overlap 

#ggsave_map("./Figures/river_overlap.png", overlap)

#### Create additional polygons to capture more points ####

river_expansion <- matrix(c(13, 73,                        # Specify longitudes, latitudes
                            0, 80,
                            0, 85,
                            63, 85,
                            73, 77,
                            30, 71,
                            13, 73),                       # Remember to close the polygon
                          ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "extra",.)
st_crs(river_expansion) <- st_crs(4326)                                          
river_expansion <- st_transform(river_expansion, crs = crs)

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import original domain polygon
  st_union() %>%                                                            # Join inshore and offshore zone
  st_union(river_expansion) %>%                                             # Expand the polygon to catch distributed river run off 
  nngeo::st_remove_holes() %>%                                              # Remove holes
  st_sf()                                                                   # reinstate class

#### Extract rivers ####

get_river <- function(File, Year, domain) {
  
data <- raster::raster(File, varname = "nav_lat") %>% raster::as.data.frame(xy = TRUE) %>% 
  dplyr::left_join(raster::as.data.frame(raster::raster(File, varname = "nav_lon"), xy = TRUE)) %>% 
  dplyr::left_join(raster::as.data.frame(raster::brick(File, varname = "sorunoff"), xy = TRUE)) %>% 
  sf::st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>% 
  dplyr::select(-c(x,y)) %>% 
  tidyr::drop_na() %>%
  sf::st_transform(sf::st_crs(domain)) %>% 
  sf::st_join(domain) %>% 
  sf::st_drop_geometry() %>% 
  tidyr::drop_na() %>% 
  colSums()

names(data) <- c(1:12)

result <- data.frame(Year = Year, Month = 1:12, Runoff = data)

return(result)
}

rivers <- future_pmap(all_files, get_river, domains, .progress = TRUE) %>% # Extract summed river output per month in the model domain
  data.table::rbindlist()


