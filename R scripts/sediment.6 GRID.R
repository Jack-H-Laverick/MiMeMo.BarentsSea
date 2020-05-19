
## could speed up the rasterise calls by using st_intersects

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis")                       # List handy data packages
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

#### GFW extraction ####

# extract <- function(data) {
#   
#   day <- read.csv(data, header = TRUE) %>%
#     filter(between(lat_bin, box[1]*100, box[2]*100), between(lon_bin, box[3]*100, box[4]*100))
# }
# 
# Data <- list.files(path = "./Data/GFW daily_csvs", pattern ="*.csv", full.names = TRUE) #%>%              # Very slow! reads in 11gb of raw data 
# #         sapply(read.csv, header = TRUE, simplify = FALSE) 
# 
# GFW_day <- extract(Data[1]) %>%
#   mutate(Latitude = lat_bin/100, Longitude = lon_bin/100) %>%
#   select(-c(lat_bin, lon_bin)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)                   # Specify original projection (crs) 
# 
# raster <- st_rasterize(GFW_day["fishing_hours"], deltax = 0.01, deltay = 0.01)
# 
# ggplot() +
#   geom_stars(data = raster)

## plotting as SF grid cells definitely slow, but allows the correct projection with uneven cell sizes

grid <- st_as_sf(star, as_points = TRUE, merge = FALSE)    # Convert to SF dataframe for modelling.
  
ggplot() +
  geom_sf(data = grid, colour = "red", fill = NA) +
  geom_sf(data = world) +
  #geom_sf(data = GFW_day) +
  NULL

#fast <- st_join(GFW_day, grid)                             # Only plot grid cells with activity

#ggplot() +
#  geom_sf(data = fast, colour = "red", fill = NA) +
#  geom_sf(data = world)
  

#### sediment grid ####

samples <- st_as_sf(star, as_points = TRUE, merge = FALSE) %>%               # Switch the GFW grid into a collection of sampling points
  st_transform(crs = 4326)                                                   # Set CRS

Sediment <- readOGR(dsn="./Data/NGU oversikt", 
                    layer = "KornstorrelseFlate_oversikt") %>%               # Import FAO shapefile
  st_as_sf(crs = 4326) %>%                                                   # Assign CRS                                   
  st_join(samples, ., left = TRUE) %>%                                       # Sample polygons with GFW grid
  mutate(Hard = if_else(SEDKORNSTR > 179 | SEDKORNSTR == 1, 1, 0, missing = NaN)) # Create a column for Hard/Soft bottom

Sed_class_star <- st_rasterize(Sediment["SEDKORNSTR"], deltax = 0.01, deltay = 0.01) # Extract bottom classification
Sed_Hard_star <- st_rasterize(Sediment["Hard"], deltax = 0.01, deltay = 0.01) # Extract hard/soft classification
#Sed_star_crop <- Sed_star[domains]                                          # "Cropping" assigns NAs to those outside the domain, we want to use all the sediment data to train the model, so do this after or we lose data

ggplot() +
  geom_stars(data = Sed_class_star["SEDKORNSTR"]) +
  scale_fill_viridis(name = 'Sediment', na.value = NA ) +
  geom_sf(data = world, fill = "black") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  NULL

 ggsave("./Figures/sediment/Classed sediment.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

####  Regrid GEBCO ####

bath_star <- samples %>%                                                       # Start with the GFW grid
  mutate(elevation = raster_extract(nc_bath, ., fun = mean)) %>%               # Sample the Bathymetry at GFW
  dplyr::select(-values) %>%                                                   # Drop unneccessary column
  st_rasterize(deltax = 0.01, deltay = 0.01)                                   # Convert SF to Stars

plot(bath_star)                                                                # Plot as a check, ggplot can't cope with the full file

#### Compute terrain variables ####

star_terrain <- function (opt) {
  x = as(bath_star, "Raster")
  x = raster::stack(x)
  n = raster::nlayers(x)
  for (i in 1:n) {
    x[[i]] = raster::terrain(x = x[[i]],  unit = "degrees", opt)  
  }
  x = stars::st_as_stars(x) 
  st_crs(x) <- st_crs(4326)
  return(x)
}                            # Wrapper for using terrain function from raster package

vars <- c("slope", "aspect", "TPI", "TRI", 
          "roughness", "flowdir") %>%                           # Variables to calculate in terrain function
  purrr::map(star_terrain)                                      # Calculate each in turn

#### Combine data ####

Stars <- c(bath_star, Sed_class_star, Sed_Hard_star,            # Binding in an ugly (but functional) way for stars
           vars[[1]], vars[[2]], vars[[3]], vars[[4]], 
           vars[[5]], vars[[6]], along = 3)                     # Along third dimension of the array

ggplot() +                                                      # With variables along a dimension it is possible to facet
  geom_stars(data = Stars, interpolate = TRUE) + 
  facet_wrap(~new_dim) +
  scale_fill_viridis(na.value = NA) + 
  theme_minimal() + 
  labs(x = NULL, y = NULL)

Stars <- split(Stars, "new_dim")                                # Moving the third dimension to attributes, to show it can be done

ggplot() +                                                      # With variables as attributes it's easy to plot just the layer
  geom_stars(data = Stars["X1"], interpolate = TRUE) + 
  scale_fill_viridis(na.value = NA) + 
  theme_minimal() + 
  labs(x = NULL, y = NULL)

Stars <- st_as_sf(Stars, as_points = FALSE, merge = FALSE) %>%   # Convert to SF dataframe for modelling.
  setNames(nm = c("Bathymetry", "Sed_class", "Sed_hard", "Slope", 
                  "Aspect", "TPI", "TRI", "Roughness", "Flowdir", "geometry"))                                                   # Name attribute

saveRDS(Stars, "./Objects/RF_sediment_observations.rds")          


