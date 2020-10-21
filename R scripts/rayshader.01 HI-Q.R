
# Pretty 3D rendering, max resolution DEM

#### Set up ####

rm(list=ls())                                                                       # Wipe the brain

packages <- c("tidyverse", "data.table", "ncdf4", "stars", "rayshader", "tictoc")   # List packages
lapply(packages, library, character.only = TRUE)                                    # Load packages

nc_raw <- nc_open("./Data/GEBCO_2019.nc")                                           # Access GEBCO bathymetry
nc_lat <- ncvar_get(nc_raw, "lat")                                                  # Extract the latitudes
nc_lon <- ncvar_get(nc_raw, "lon")                                                  # Extract the longitudes
nc_close(nc_raw)                                                                    # You must close an open netcdf file when finished to avoid data loss
rm(nc_raw)                                                                          # Drop the file

#### Extract Area ####

S <- nrow(nc_lat)*(7.8/9) ; W <- length(nc_lon)*1/2 ; E <- length(nc_lon)*4.3/6     # For Mercatore

Bathymetry <- read_ncdf("./Data/GEBCO_2019.nc", ncsub = cbind(
  start = c(W, S), count =c((E-W+1), (43200 - S +1)))) #%>%
#  st_transform(crs = 3035)  

# plot(Bathymetry)

matrix <- Bathymetry$elevation %>% as.numeric() %>% 
  matrix(nrow = nrow(Bathymetry$elevation), ncol= ncol(Bathymetry$elevation))

mat <- matrix[seq(nrow(matrix), 1, by = -3),]                               # Use for full resolution, divide zscales by 10                

#### Plot area ####

shadow = ray_shade(mat, zscale = 0.1, lambert = FALSE, multicore = TRUE)
ambient = ambient_shade(mat, zscale = 5, multicore = TRUE)

mat %>%
  sphere_shade(zscale = 1, texture = "imhof2") %>%
  add_shadow(shadow, 0.5) %>%
  add_shadow(ambient) %>%
  plot_3d(mat, zscale = 20, fov = 60, theta = 60, phi = 30, 
          windowsize = c(1170, 585), zoom = 0.3,
          water = TRUE, waterdepth = 0, wateralpha = 0.75, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5) 
#z = 450, y = 800, x = -600
tic()
render_highquality(print_scene_info = T, parallel = TRUE, lightintensity = 0, samples = 1500, filename = "rayshade-hi.png", 
                   scene_elements = bind_rows(rayrender::sphere(z = 0, y = 1750, x = 600, radius = 50,
                                                  material = rayrender::light(color = "white", intensity = 4500)),
                                              rayrender::text3d(label = "Barents Sea", angle = c(30, 60, 0), z = 500, y = 800, x = -650, 
                                                  text_height = 150, material = rayrender::light(color = "white", intensity = 100))),  
                   clamp_value = 2, aperture = 120, # Bigger aperture, more blur
                   min_variance = 0)
toc()
