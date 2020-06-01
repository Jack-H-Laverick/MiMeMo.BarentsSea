
## could speed up the rasterise calls by using st_intersects

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis", "tictoc")             # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth", "nngeo")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages

domains <- readRDS("./Objects/Domains.rds") %>%                               # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

nc_bath <- readRDS("./Objects/Bathymetry_points.rds") %>%                     # Import bathymetry
  filter(Latitude > 60) %>%                                                   # Shrink a bit
  st_as_stars()                                                               # Convert to stars
st_crs(nc_bath) <- st_crs(4326)                                               # set lat-lon crs

world <- ne_countries(scale = "medium", returnclass = "sf")                   # Get a world map

box <- st_bbox(domains)

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
  x = raster::stack(x)
  n = raster::nlayers(x)
  for (i in 1:n) {
    x[[i]] = raster::terrain(x = x[[i]],  unit = "degrees", opt)  
  }

  y = as(y, "Spatial")
  
  terrain <- raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)

  return(terrain)
}                                    # Wrapper for using terrain function from raster package

bath_star <- st_rasterize(bathymetry, deltax = 0.01, deltay = 0.01)
plot(bath_star)                                                               # Plot as a check, ggplot can't cope with the full file

Terrain <- map(c("slope", "aspect", "TPI", "TRI", 
                 "roughness", "flowdir"),                                     # Variables to calculate in terrain function
             star_terrain,                                                    # Calculate each in turn
             x = bath_star,                                                   # From rasterised bathymetry
             y = bathymetry) %>%                                              # Using samples (which also have bathymetry information appended)
  data.frame(bathymetry) %>%                                                  # Bind into a dataframe and add bathymetry
  st_as_sf()                                                                  # Reinstate SF class

#### Sample sediment ####

Sediment <- readOGR(dsn="./Data/NGU oversikt", 
                    layer = "KornstorrelseFlate_oversikt") %>%                # Import NGU shapefile
  st_as_sf(crs = 4326) %>%                                                    # Assign CRS                                   
  select(SEDKORNSTR) %>% 
  st_join(Terrain, ., left = TRUE) %>%                                        # Sample polygons by bathymetric grid
  mutate(Hard = if_else(SEDKORNSTR > 179 | SEDKORNSTR == 1, 1, 0, missing = NaN)) # Create a column for Hard/Soft bottom

Sed_class_star <- st_rasterize(Sediment["SEDKORNSTR"], deltax = 0.01, deltay = 0.01) # Extract bottom classification
Sed_Hard_star <- st_rasterize(Sediment["Hard"], deltax = 0.01, deltay = 0.01) # Extract hard/soft classification

ggplot() +
  geom_stars(data = Sed_class_star["SEDKORNSTR"]) +
  scale_fill_viridis(name = 'Sediment', na.value = NA ) +
  geom_sf(data = world, fill = "black") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  NULL

ggsave("./Figures/sediment/Classed sediment.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Sample be shear stress ####

Stress <- st_join(Sediment,                                                   # Join previous data
                 readRDS("./Objects/Stress95.rds"))                           # to bed shear stress estimates

ggplot(Stress, aes(x = Longitude, y = Latitude, fill = SEDKORNSTR)) +         # Check the bathymetry looks believable
  geom_raster()

Stress <- select(Stress, -c(Shore, Elevation, area, Cell_area, Longitude, Latitude)) # Drop excess columns

saveRDS(Stress, "./Objects/RF_sediment_observations.rds")          
