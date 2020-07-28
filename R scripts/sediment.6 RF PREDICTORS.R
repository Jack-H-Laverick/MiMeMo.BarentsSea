
## could speed up the rasterise calls by using st_intersects

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis", "tictoc")             # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages

domains <- readRDS("./Objects/Domains.rds") %>%                               # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

nc_bath <- readRDS("./Objects/Bathymetry_points.rds") %>%                     # Import bathymetry
  filter(Latitude > 60) %>%                                                   # Shrink a bit
  st_as_stars()                                                               # Convert to stars
st_crs(nc_bath) <- st_crs(4326)                                               # set lat-lon crs

world <- ne_countries(scale = "medium", returnclass = "sf")                   # Get a world map

box <- st_bbox(domains) 

# voronoi2 <- function(points, area) {
#   
#   result <- purrr::map(1:nrow(area), ~{                            # For each polygon in area
#     voronoi <- points %>%                                          # Take the grid points
#       sf::st_geometry() %>%                                        # To get sfc from sf
#       sf::st_union() %>%                                           # To get a sfc of MULTIPOINT type
#       sf::st_voronoi(envelope = sf::st_geometry(area[.x,])) %>%    # Voronoi polygon for the area
#       sf::st_collection_extract(type = "POLYGON") %>%              # A list of polygons
#       sf::st_sf() %>%                                              # From list to sf object
#       sf::st_join(points) %>%                                      # put names back
#       sf::st_intersection(area[.x,]) %>%                           # Cut to shape of target area
#       dplyr::mutate(Cell_area = units::drop_units(sf::st_area(.))) # Area of each polygon
#   } ) %>%
#     dplyr::bind_rows() %>%                                         # Combine the results from each area
#     sf::st_sf(geometry = .$geometry, crs = 3035)                      # Reinstate attributes of the geometry column
# }
# 
# # tic()
# # samples2 <- expand.grid(Longitude = seq(box[1], box[3], by = 0.01),           # Get points spread across the domain
# #                         Latitude = seq(box[4], box[2], by = -0.01)) %>%                  # in 0.01 degrees
# #   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Convert to sf
# #   st_transform(crs = 3035) %>% 
# #   voronoi2(st_transform(domains, crs = 3035))
# # toc()
# # 
# # ggplot(samples2) + geom_sf(aes(fill = Cell_area), size = 0)

star <- st_as_stars(box, dx = 0.01, dy = 0.01, values = 0)

samples <- st_as_sf(star, as_points = TRUE, merge = FALSE) %>%                # Switch the GFW grid into a collection of sampling points
  st_transform(crs = 4326)                                                    # Set CRS

####  Regrid GEBCO ####

star_extract <- function(x, y, fun = NULL, na.rm = FALSE) {
  x = as(x, "Raster")
  y = as(y, "Spatial")
  raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)
}
 
bathymetry <- samples %>%                                                     # Start with the GFW grid
  mutate(elevation = star_extract(nc_bath, ., fun = mean)) %>%                # Sample the Bathymetry at GFW
  dplyr::select(-values)                                                      # Drop unneccessary column

#### Compute terrain variables ####

star_terrain <- function (x, y, opt) {
  x = as(x, "Raster")
  #x = raster::stack(x)
  #n = raster::nlayers(x)
  #for (i in 1:n) {
  #  x[[i]] = raster::terrain(x = x[[i]],  unit = "degrees", opt)  
  #}
  x = raster::terrain(x = x,  unit = "degrees", opt)
  y = as(y, "Spatial")
  
  terrain <- raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)

  return(terrain)
}                                    # Wrapper for using terrain function from raster package

bath_star <- st_rasterize(bathymetry, deltax = 0.01, deltay = 0.01)
plot(bath_star)                                                               # Plot as a check, ggplot can't cope with the full file

tic()
Terrain <- star_terrain(x = bath_star,                                        # From rasterised bathymetry 
                        y = bathymetry,                                       # Using samples (which also have bathymetry information appended)
                        c("slope", "TPI", "TRI", "roughness")) %>%            # Calculate variables in raster::terrain function
  data.frame(bathymetry) %>%                                                  # Bind into a dataframe and add bathymetry
  st_as_sf()                                                                  # Reinstate SF class
toc()

#### Sample sediment ####

Sediment <- readOGR(dsn="./Data/NGU oversikt", 
                    layer = "KornstorrelseFlate_oversikt") %>%                # Import NGU shapefile
  st_as_sf(crs = 4326) %>%                                                    # Assign CRS                                   
  select(Sed_class = SEDKORNSTR) %>% 
  st_join(Terrain, ., left = TRUE)                                            # Sample polygons by bathymetric grid
  
Sed_class_star <- st_rasterize(Sediment["Sed_class"], deltax = 0.01, deltay = 0.01) # Extract bottom classification

ggplot() +
  geom_stars(data = Sed_class_star["Sed_class"]) +
  scale_fill_viridis(name = 'Sediment', na.value = NA ) +
  geom_sf(data = world, fill = "black") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  NULL

ggsave("./Figures/sediment/Classed sediment.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Sample be shear stress ####

Stress <- st_join(Sediment,                                                   # Join previous data
                 readRDS("./Objects/Stress95.rds")) %>%                       # to bed shear stress estimates
          distinct(.keep_all = TRUE)

ggplot(Stress, aes(x = Longitude, y = Latitude, fill = Sed_class)) +          # Check the bathymetry looks believable
  geom_raster()

Stress <- select(Stress, -c(Shore, Elevation, Depth, area, Cell_area, Longitude, Latitude)) # Drop excess columns

saveRDS(Stress, "./Objects/RF_sediment_observations.rds")          
