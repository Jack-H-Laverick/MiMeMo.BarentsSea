
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "raster", "data.table")           # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to paralelise by with furrr

all_files <- list.files("./Data/Rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc") #%>%

domains <- readRDS("./Objects/Domains.rds")                                 # Import domain polygon

# Get an example set of points to plot
# example <- raster(all_files[1], varname = "nav_lat") %>% 
#   as.data.frame(xy = TRUE) %>% 
#   dplyr::left_join(as.data.frame(raster(all_files[1], varname = "nav_lon"), xy = TRUE)) %>% 
#   dplyr::left_join(as.data.frame(raster(all_files[1], varname = "sorunoff"), xy = TRUE)) %>% 
#   sf::st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>% 
#   dplyr::select(-c(x,y)) %>% 
#   tidyr::drop_na()
  
#### Check domain overlaps point estimates of river runoff ####

# overlap <- ggplot() +
# #  geom_sf(data = river_expansion, fill = "orange", colour = "orange") +        # Uncomment me once you've built your extra polygons
#   geom_sf(data = domains, fill = "red", colour = "red") +
#   geom_sf(data = st_transform(example, crs = crs), aes(colour = runoff_land_into_ocean), size = 0.2) +
#   theme_minimal() +
#   zoom +
#   NULL
# overlap 

#ggsave_map("./Figures/river_overlap.png", overlap)

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import original domain polygon
  st_union() %>%                                                            # Join inshore and offshore zone
  st_union(river_expansion) %>%                                             # Expand the polygon to catch distributed river run off 
  nngeo::st_remove_holes() %>%                                              # Remove holes
  st_sf() %>%                                                               # reinstate class
  mutate(Keep = T)

#### Work out which pixels are in the model domain ####

tictoc::tic()
coords <- raster(all_files[1], varname = "nav_lat") %>%
  as.data.frame(xy = TRUE) %>%                                              # Get a dataframe of xy positions and latitudes
  dplyr::left_join(as.data.frame(raster(all_files[1], varname = "nav_lon"), xy = TRUE)) %>% # Bind to the same for longitudes
  dplyr::left_join(as.data.frame(area(raster(all_files[1], varname = "nav_lon"), na.rm = F, weights = F), xy = TRUE)) %>% # Bind to the same for longitudes
  st_as_sf(coords = c("nav_lon", "nav_lat"), crs = 4326) %>%                # Convert to sf
  st_transform(sf::st_crs(domains)) %>%                                     # Transform points to the same crs as domain polygon
  st_join(domains) %>%                                                      # Check which points are in the domain
  st_drop_geometry() %>%                                                    # Drop sf formatting
  drop_na() %>%                                                             # Drop points outside the domain
  mutate(Area = layer * 1000000) %>%                                        # Work out the area of each pixel (in m^2)
  dplyr::select(-c(Keep, layer))  

#### Extract data ####

rivers <- future_map(all_files, ~{
  data <- brick(.x, varname = "sorunoff") %>%                               # Extract river runoff
  as.data.frame(na.rm = T, xy = T) %>%                                      # Convert to XY dataframe of non-empty pixels
  pivot_longer(starts_with("X9."), names_to = "Month", values_to = "Runoff") %>% # Collect all months into one column
    mutate(Month = str_replace(Month, "X9.96920996838687e.36.", ""),        # Clean character string indicating month
           Year = str_sub(.x, start = -7, end = -4))                        # Extract Year from file name
}, .progress = TRUE) %>%                        
  data.table::rbindlist()                                                   # Bind

#### Summarise ####

setDT(rivers, key = c("x", "y"))                                            #  Set to data.table for quick filtering by pixel
setDT(coords, key = c("x", "y"))

summary <- rivers[coords,][,                             # Summarise pixels in the model domain
  .(Runoff = sum(Runoff, na.rm = T) * Area),             # By totaling Kg of water by pixel area 
  by = c("Month", "Year")] %>%                           # By time step
  mutate(Runoff = Runoff*86400) %>%                      # scaled to daily from per seconds 
  drop_na()                                              # Drop pixels which didn't contribute water for some time steps

tictoc::toc()
saveRDS(rivers, "./Objects/River volume input.rds")

#### Plot ####

ggplot(data = summary %>% 
         mutate(Date = as.Date(paste("01", Month, Year, sep = "/"), format = "%d/%m/%Y"))) + 
  geom_area(aes(x = Date, y = Runoff), fill = "blue") +
  theme_minimal() +
  labs(y = expression("Freshwater input (Kg.D"^{-1}*")"), 
       caption = "NEMO-MEDUSA input of reshwater discharge from rivers into model domain") +
  NULL

ggsave("./Figures/NEMO-MEDUSA/River runoff.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")
