
# Calculate additional bathymetry values which may be useful predictors 
# in a random forest model for sediment types.
# Rasterising point data can also be helpful when selecting values on the boundaries of the model domain

#### Setup ####

rm(list=ls())                                                   # Wipe the brain

Tidy_packages <- c("tidyverse", "viridis")                      # List handy data packages
Geo_packages <- c("raster", "sf", "rnaturalearth",  "stars")    # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages

world <- ne_countries(scale = "medium", returnclass = "sf") %>% # Get a world map
  st_transform(crs = 3035)                                      # Assign polar projection

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")       # Load SF polygons of the MiMeMo model domains

nc_bath <- readRDS("~/Data/Bathymetry GEBCO/Polar_Bathymetry_halfres.rds") # Get bathymetry
# st_transform(domains, crs = 4326)                             # tells me max and min extent so I can filter nc_bath further

#### Rasterise ####

small <- filter(nc_bath, Latitude > 60) %>%                     # Reduce data further
  slice(seq(1, nrow(.), by = 5)) %>%                            # Thin in the area of interest
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% # st dataframe to SF format
  st_transform(crs = 3035)                                      # Drop points outside of the polygons

smaller <- small %>%                                            # Reduce data further
  st_join(domains) %>%                                          # Clip to the model domains.
  drop_na(Region)                                               # Drop points outside of the polygons

raster_all <- st_rasterize(small, st_as_stars(st_bbox(small),   # Convert to a full raster
          nx = 1000, ny = 1000, values = NA_real_))

raster_domain <- st_rasterize(smaller, st_as_stars(st_bbox(smaller), # Create a raster of just the domains
          nx = 1000, ny = 1000, values = NA_real_))

# ggplot() + geom_stars(data = raster_all) +
#   scale_fill_viridis(option = "viridis", na.value = NA) +
#   geom_sf(data = domains, fill = NA)                          # Check lines up with domain

saveRDS(raster_all, "~/Data/Bathymetry GEBCO/Raster small bathymetry.rds")

#### Calculate terrain features from elevation (bathymetry) data in domains ####

raster_all <- readRDS("~/Data/Bathymetry GEBCO/Raster full bathymetry.rds")
#raster_derived <- readRDS("~/Data/Bathymetry GEBCO/Raster domain terrain.rds" )

star_terrain <- function (opt) {
  x = as(raster_all, "Raster")
  x = raster::stack(x)
  n = raster::nlayers(x)
  for (i in 1:n) {
    x[[i]] = raster::terrain(x = x[[i]],  unit = "degrees", opt)  
  }
  stars::st_as_stars(x)
}                            # Wrapper for using terrain function from raster package

#st_crs(raster_all) <- st_crs(3035) 

plot(star_terrain("slope"))


vars <- c("slope", "aspect", "TPI", "TRI", 
         "roughness", "flowdir") %>%                            # Variables to calculate in terrain function
  map(star_terrain)                                             # Calculate each in turn
  
raster_derived <- c(vars[[1]], vars[[2]], vars[[3]],
                    vars[[4]], vars[[5]], vars[[6]])            # Bind in an ugly way to keep as a stars object

ggplot() + 
  geom_stars(data = raster_derived[1]) +
  scale_fill_viridis(option = "viridis", na.value = NA) +
  theme_minimal() + 
  labs(x = NULL, y = NULL)
  
# ggsave("~/Data/Bathymetry GEBCO/FIG_domain_Slope.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#saveRDS(raster_all2, "~/Data/Bathymetry GEBCO/Raster full bathymetry2.rds")
#saveRDS(raster_derived, "~/Data/Bathymetry GEBCO/Raster domain terrain.rds" )

