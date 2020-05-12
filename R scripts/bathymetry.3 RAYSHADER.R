
# Open the GEBCO netcdf file and limit to a workable size, reshape for tidyverse, and extract contours

#### Set up ####

rm(list=ls())                                                                       # Wipe the brain

packages <- c("tidyverse", "data.table", "ncdf4", "stars", "rayshader")             # List packages
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

#### Plot area ####

mat <- high_matrix

montshadow = ray_shade(mat, zscale = 0.1, lambert = FALSE)
montamb = ambient_shade(mat, zscale = 5)

mat %>%
  sphere_shade(zscale = 1, texture = "imhof2") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb) %>%
  
  plot_3d(mat, zscale = 10, fov = 0, theta = 60, phi = 30, 
          windowsize = c(1840, 1150), zoom = 0.55,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(600)                                                                       # Pause for RGL to open
render_snapshot("./Figures/bathymetry/Rayshade")                                     # Save the current view in the RGL window
