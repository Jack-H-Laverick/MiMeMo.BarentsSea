
# Open the GEBCO netcdf file and limit to a workable size, reshape for tidyverse, and extract contours

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

#S <- nrow(nc_lat)*(7.8/9) ; W <- length(nc_lon)*1/2 ; E <- length(nc_lon)*4.7/6    # For Polar projection
S <- nrow(nc_lat)*(7.8/9) ; W <- length(nc_lon)*1/2 ; E <- length(nc_lon)*4.3/6     # For Mercatore

Bathymetry <- read_ncdf("./Data/GEBCO_2019.nc", ncsub = cbind(
  start = c(W, S), count =c((E-W+1), (43200 - S +1)))) #%>%
#  st_transform(crs = 3035)  

# plot(Bathymetry)

matrix <- Bathymetry$elevation %>% as.numeric() %>% 
  matrix(nrow = nrow(Bathymetry$elevation), ncol= ncol(Bathymetry$elevation))

# 8192 x 8192 maximum textured syrface allowed by RGL
thin_matrix <- matrix[seq(nrow(matrix), 1, by = -27), seq(1, ncol(matrix), by = 9)] # Fast plotting to get view right
high_matrix <- matrix[seq(nrow(matrix), 1, by = -3),]                               # Use for full resolution, divide zscales by 10                

#### Create overlay ####

domain <- readRDS("./Objects/Domains.rds") %>% 
  st_transform(crs = 4326)

Latitudes <- nc_lat[S:(S+(43200 - S))]
Longitudes <- nc_lon[W:(W+(E-W))]

thin_crop <- expand.grid(Latitude = Latitudes[seq(1, length(Latitudes), by = 9)], 
                         Longitude = Longitudes[seq(length(Longitudes), 1, by = -27)]) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  st_join(domain) %>% 
  mutate(In = !is.na(Shore))                         # If a pixel is NA it's not in the domain

thin_crop2 <- matrix(thin_crop$In, nrow = nrow(thin_matrix), ncol= ncol(thin_matrix), byrow = TRUE)

thin_overlay <- array(c(matrix(1, nrow = nrow(thin_matrix), ncol= ncol(thin_matrix)),  # R
                        matrix(0, nrow = nrow(thin_matrix), ncol= ncol(thin_matrix)),  # G
                        matrix(0, nrow = nrow(thin_matrix), ncol= ncol(thin_matrix)), # B
                        matrix(0, nrow = nrow(thin_matrix), ncol= ncol(thin_matrix))), # Transparency
                        dim = c(nrow(thin_matrix), ncol= ncol(thin_matrix), 4))

thin_overlay[,,1][thin_crop2] <- 255
thin_overlay[,,2][thin_crop2] <- 255
thin_overlay[,,3][thin_crop2] <- 0
thin_overlay[,,4][thin_crop2] <- 0.8

thin_overlay <- aperm(thin_overlay, c(2,1,3))

## High resolution overlay 

#high_crop <- expand.grid(Latitude = Latitudes, 
#                         Longitude = Longitudes[seq(length(Longitudes), 1, by = -3)]) %>% 
#  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
#  st_join(domain) %>% 
#  mutate(In = !is.na(Shore))                         # If a pixel is NA it's not in the domain

#high_crop2 <- matrix(high_crop$In, nrow = nrow(high_matrix), ncol= ncol(high_matrix), byrow = TRUE)

#high_overlay <- array(c(matrix(1, nrow = nrow(high_matrix), ncol= ncol(high_matrix)),  # R
#                        matrix(0, nrow = nrow(high_matrix), ncol= ncol(high_matrix)),  # G
#                        matrix(0, nrow = nrow(high_matrix), ncol= ncol(high_matrix)),  # B
#                        matrix(0, nrow = nrow(high_matrix), ncol= ncol(high_matrix))), # Transparency
#                      dim = c(nrow(high_matrix), ncol= ncol(high_matrix), 4))

#high_overlay[,,1][high_crop2] <- 255
#high_overlay[,,2][high_crop2] <- 255
#high_overlay[,,3][high_crop2] <- 0
#high_overlay[,,4][high_crop2] <- 0.8

# high_overlay <- aperm(high_overlay, c(2,1,3))

#### Plot area ####

mat <- thin_matrix
#overlay <- high_overlay

montshadow = ray_shade(mat, zscale = 0.1, lambert = FALSE, multicore = TRUE)
montamb = ambient_shade(mat, zscale = 5, multicore = TRUE)

mat %>%
  sphere_shade(zscale = 1, texture = "imhof2", ) %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb) %>%
#  add_overlay(overlay = overlay, alphacolor = "red", alphalayer = 0.001, alphamethod = 'multiply') %>% 
#add_overlay(overlay = overlay, alphalayer = 0.1, alphamethod = 'multiply') %>% 
  
  plot_3d(mat, zscale = 100, fov = 0, theta = 60, phi = 30, 
          windowsize = c(1280, 640), zoom = 0.55,
          water = TRUE, waterdepth = 0, wateralpha = 0.75, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5) 
#Sys.sleep(30)                                                                        # Pause for RGL to open
#render_snapshot("./Figures/bathymetry/Rayshade")                                     # Save the current view in the RGL window

# tic()   
# render_camera(theta=60,phi=30,fov=60,zoom= 0.3)                                       # Change camera view after initial plotting 
# toc()
# 
# tic()
# render_highquality(print_scene_info = T, parallel = TRUE, lightintensity = 0, samples = 2000, #filename = "rayshade.png", 
#                    scene_elements = bind_rows(rayrender::sphere(z = 0, y = 200, x = 100, radius = 5,
#                    material = rayrender::light(color = "white", intensity = 10000)),
#                    rayrender::text3d(label = "Barents Sea", angle = c(30, 60, 0), z = 75, y = 100, x = -100, 
#                                      text_height = 30, material = rayrender::light(color = "white", intensity = 100))),  
#                    clamp_value = 2, aperture = 20) # Bigger aperture, more blur,0,0))), 
# 
# toc()
