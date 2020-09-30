
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "furrr")                   # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to paralelise by with furrr

all_files <- list.files("./Data/Rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  mutate(Year = str_sub(value, start = -7, end = -4)) %>%                   # Drop file extension to get number
  rename(File = "value")

domains <- readRDS("./Objects/Domains.rds")                                 # Import domain polygon

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
  geom_sf(data = river_expansion, fill = "orange", colour = "orange") +        # Uncomment me once you've built your extra polygons
  geom_sf(data = domains, fill = "red", colour = "red") +
  geom_sf(data = st_transform(example, crs = crs), aes(colour = runoff_land_into_ocean), size = 0.2) +
  theme_minimal() +
  zoom +
  NULL
overlap 

#ggsave_map("./Figures/river_overlap.png", overlap)

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import original domain polygon
  st_union() %>%                                                            # Join inshore and offshore zone
  st_union(river_expansion) %>%                                             # Expand the polygon to catch distributed river run off 
  nngeo::st_remove_holes() %>%                                              # Remove holes
  st_sf()                                                                   # reinstate class

#### Extract rivers ####

rivers <- future_pmap(all_files, get_rivers, domains, .progress = TRUE) %>% # Extract summed river output per month in the model domain
  data.table::rbindlist()
