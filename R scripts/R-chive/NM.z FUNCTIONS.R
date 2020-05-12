
##**## A tidy place to keep functions and reduce clutter in programmes

#### NEMO - MEDUSA data extraction    ####

empty <- function(x) all(is.na(x))                      # Quick function looking for areas with no data
get_weights <- function(top, bottom)           {
  #top <- 200                                                                    # shallowest depth of the slice
  #bottom <- 1000                                                                # What's the bottom of the slice? for incorporating into a function
  
  weights <- array(NA, c(235,190,38))                                          # Initialise an array to hold weights
  first <- weights[,,1]
  first[] <- mean(Space$nc_depth[1:2]) - top %>% rep(times = 44650)            # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(Space$nc_depth[1:2])                              # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                    # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,38] <- mask_bathy - mean(Space$nc_depth[37:38])                    # The remaining water column thickness is the sea floor - the deepest midpoint. 
  
  for (i in 2:37) { 
    #i <- 23  
    last_midpoint <- mean(Space$nc_depth[(i-1):i])                               # Find the mid depth to the layer above
    next_midpoint <- mean(Space$nc_depth[i:(i+1)])                               # Find the mid depth to the layer below
    
    if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
    if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice
    
    weights[,,i] <- below - above %>% rep(times = 44650)                         # Calculate layer thickness and repeat to fill the array
    
    marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
    weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
    
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA 
  
  return(weights)
}    # Return the weights for averaging across depths, specifying the top and bottom layer of a slice
get_weights.W <- function(top, bottom)         {
  
  weights <- array(NA, c(235,190,39))                                            # Initialise an array to hold weights
  first <- weights[,,1]
  first[] <- mean(DepthsW[1:2]) - top %>% rep(times = 44650)                     # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(DepthsW[1:2])                                       # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                      # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,39] <- mask_bathy - mean(DepthsW[38:39])                             # The remaining water column thickness is the sea floor - the deepest midpoint. 
  
  for (i in 2:38) { 
    #i <- 23  
    last_midpoint <- mean(DepthsW[(i-1):i])                                      # Find the mid depth to the layer above
    next_midpoint <- mean(DepthsW[i:(i+1)])                                      # Find the mid depth to the layer below
    
    if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
    if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice
    
    weights[,,i] <- below - above %>% rep(times = 44650)                         # Calculate layer thickness and repeat to fill the array
    
    marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
    weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
    
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA 
  
  return(weights)
}    # Return the weights for averaging across depths, specifying the top and bottom layer of a slice
get_weights.old <- function(top, bottom)       {
  #top <- 200                                                                    # shallowest depth of the slice
  #bottom <- 1000                                                                # What's the bottom of the slice? for incorporating into a function
  
  weights <- array(NA, c(nrow(Space$nc_lon),ncol(Space$nc_lon), 27))           # Initialise an array to hold weights
  first <- weights[,,1]
  first[] <- mean(Space$nc_depth[1:2]) - top %>% rep(times = nrow(Space$nc_lon) * ncol(Space$nc_lon)) # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(Space$nc_depth[1:2])                              # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                    # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,27] <- mask_bathy - mean(Space$nc_depth[26:27])                    # The remaining water column thickness is the sea floor - the deepest midpoint. 
  
  for (i in 2:26) { 
    #i <- 23  
    last_midpoint <- mean(Space$nc_depth[(i-1):i])                               # Find the mid depth to the layer above
    next_midpoint <- mean(Space$nc_depth[i:(i+1)])                               # Find the mid depth to the layer below
    
    if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
    if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice
    
    weights[,,i] <- below - above %>% rep(times = nrow(Space$nc_lon) * ncol(Space$nc_lon)) # Calculate layer thickness and repeat to fill the array
    
    marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
    weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
    
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA 
  
  return(weights)
}    # Return the weights for averaging across depths, specifying the top and bottom layer of a slice
get_spatial <- function(file)                  {
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_lat <- ncvar_get(nc_raw, "nav_lat")               # Extract a matrix of all the latitudes
  nc_lon <- ncvar_get(nc_raw, "nav_lon")               # Extract a matrix of all the longitudes
  nc_depth <- ncvar_get(nc_raw, "deptht")                                    # Extract a matrix of depths
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  all <- list("nc_lat" = nc_lat, "nc_lon" = nc_lon, "nc_depth" = nc_depth)
  return(all)
}    # Pull spatial structure netcdf file
Compartmentalise <- function(work)             {
  #  levels(work$Depth) <- depth_levels                                       # Recode factor levels
  
  work <- split(work, f = work$Shore) %>%                                  # Split by shore region
    .[sapply(., function(x) dim(x)[1]) > 0]                                # Drop empty elements introduced by interactions
  
  inshore <- data.frame(work[["Inshore"]]) %>%                             # Maniuplate just Inshore observations 
    filter(Depth == "S") %>%                                               # There is no deep compartment inshore
    filter(Bathymetry > -60 | Shore_dist < 20000) %>%                      # Reinstate conditions for inshore zone, as clipping polygon is slightly larger
    mutate(weights = abs(Bathymetry)) 
  
  inshore$weights[inshore$weights > 60] <- 60                              # Overwrite the weights used for deep nearshore pixels. 
  
  offshore <- data.frame(work[["Offshore"]]) %>%                           # Manipulate offshore observations
    filter(between(Bathymetry, -400, -60) & Shore_dist > 20000) %>%        # Reinstate conditions for offshore zone, as cliiping polygon is slightly larger
    mutate(weights = abs(Bathymetry)) 
  
  offshore$weights[offshore$weights > 60 & offshore$Depth == "S" ] <- 60   # Observation in water deeper than 60 m with a shallow label should have a water thickness of 60 m
  offshore$weights[offshore$Depth == "D" ] <- offshore$weights[offshore$Depth == "D" ] - 60 # All deep water observations have a water thickness of the bathymetry - the shallow layer thickness
  
  result <- rbind(inshore, offshore)
  return(result)
}    # Refilter and get weights for depths and shore zones *use this once on the spine dataframe for speed, then join

stratify  <- function(data, depth, weights)    {
  
  # data <- nc_zonal ; depth <- Deep_mark ; weights <- dw                   # testing
  
  new <- data[,,depth] * weights[,,depth]                                      # Select slice of depths to average, multiply values by the weights
  empties <- apply(new, c(1,2), empty)                                         # Find pixels with all depths shown by NA (locations of fake 0s)
  
  new2 <- apply(new, c(1,2), sum, na.rm = TRUE)                                # Sum the weighted values at a pixel
  denominator <- apply(weights[,,depth], c(1,2), sum, na.rm = TRUE)            # Sum the weights
  weighted_mean <- new2/denominator                                            # Divide by the sum of the weights
  weighted_mean[empties] <- NA                                                 # Sum replaces an all NA dimension with 0, overwrite these by position
  return(weighted_mean)                                                       
}    # Take a range of depths from an array and average into a single matrix for the layer, weighted by thickness of water around each depth observation

get_sea   <- function(path, file)              {
  
  print(str_glue("{file} Extracting Salinity, Temperature, and Sea Ice concentration"))
  nc_raw <- nc_open(paste0(path, file))                                        # Open up a netcdf file to see it's raw contents (var names)
  nc_saline <- ncvar_get(nc_raw, "vosaline", start3D, count3D)                 # Extract an array of salinities
  nc_temp <- ncvar_get(nc_raw, "votemper", start3D, count3D)                   # Extract an array of temperatures
  nc_ice <- ncvar_get(nc_raw, "soicecov")                                      # Extract a matrix of ice fractions
  nc_close(nc_raw)                                                             # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                        # Grab the tidied dataframe of lat-longs
    mutate(Ice_conc = as.numeric(nc_ice),                                      # Append new variable to coordinates (no depths for ice)
           Salinity = as.numeric(stratify(nc_saline, Shallow_mark, sw)),       # Collapse shallow salinity into 2D and convert to long format
           Temperature = as.numeric(stratify(nc_temp, Shallow_mark, sw)),      # Collapse shallow temperatures into 2D and convert to long format
           Depth = "S")                                                        # Introduce depth column
  
  deep <- output %>%                                                           # Grab the tidied dataframe of lat-longs
    mutate(Ice_conc = NA,                                                      # Insert empty column to allow fast binding by position
           Salinity = as.numeric(stratify(nc_saline, Deep_mark, dw)),
           Temperature = as.numeric(stratify(nc_temp, Deep_mark, dw)),
           Depth = "D")                                                        # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                              # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                                 # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull ice, and then salinity and temperature in 2 depth bands from grid_T_ files
get_bio   <- function(path, file)              { 
  
  print(str_glue("{file} Extracting Dissolved Inorganic Nitrogen and Chlorophyll"))
  nc_raw <- nc_open(paste0(path, file))                                        # Open up a netcdf file to see it's raw contents (var names)
  nc_DIN <- ncvar_get(nc_raw, "DIN", start3D, count3D)                         # Extract an array for the variable
  nc_CHD <- ncvar_get(nc_raw, "CHD", start3D, count3D)
  nc_CHN <- ncvar_get(nc_raw, "CHN", start3D, count3D)
  nc_Chl <- nc_CHD + nc_CHN ; rm(nc_CHD, nc_CHN)
  nc_close(nc_raw)                                                             # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                        # Grab the tidied dataframe of lat-longs
    mutate(DIN = as.numeric(stratify(nc_DIN, Shallow_mark, sw)),               # Collapse shallow DIN into 2D and convert to long format
           Chlorophyll = as.numeric(stratify(nc_Chl, Shallow_mark, sw)),       # Collapse shallow chlorophyll into 2D and convert to long format
           Depth = "S")                                                        # Introduce depth column
  
  deep <- output %>%                                                           # Grab the tidied dataframe of lat-longs
    mutate(DIN = as.numeric(stratify(nc_DIN, Deep_mark, dw)),
           Chlorophyll = as.numeric(stratify(nc_Chl, Deep_mark, dw)),
           Depth = "D")                                                        # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                              # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                                 # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull nitrogen and chlorophyll from ptrc_T_ files 
get_ice   <- function(path, file)              { 
  
  print(str_glue("{file} Extracting Ice presence, and Ice and Snow thickness"))
  nc_raw <- nc_open(paste0(path, file))                                        # Open up a netcdf file to see it's raw contents (var names)
  nc_Ice <- ncvar_get(nc_raw, "ice_pres")                                      # Extract a matrix of ice presence
  nc_Ithick <- ncvar_get(nc_raw, "iicethic")                                   # Extract ice thicknesses
  nc_Sthick <- ncvar_get(nc_raw, "isnowthi")                                   # Extract snow thicknesses
  nc_close(nc_raw)                                                             # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                        # Grab the tidied dataframe of lat-longs
    mutate(Ice_pres = as.numeric(nc_Ice),                           
           Ice_Thickness = as.numeric(nc_Ithick), 
           Snow_Thickness = as.numeric(nc_Sthick),
           Depth = "S")                                                        # Introduce depth column
  
  deep <- output %>%                                                           # Grab the tidied dataframe of lat-longs
    mutate(Ice_pres = NA,                           
           Ice_Thickness = NA, 
           Snow_Thickness = NA,
           Depth = "D")                                                        # Introduce depth column
  
  all <- rbind(shallow, deep) %>%                                              # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                                 # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull ice presence, and snow and ice thickness from icemod_ files 
get_vertical   <- function(path, file)         { 
  
  print(str_glue("{file} Extracting Vertical water movements"))
  nc_raw <- nc_open(paste0(path, file))                                        # Open up a netcdf file to see it's raw contents (var names)
  nc_vel <- ncvar_get(nc_raw, "vovecrtz", start3DW, count3DW)                  # Extract an array for the variable
  nc_dif <- ncvar_get(nc_raw, "votkeavt", start3DW, count3DW)
  nc_close(nc_raw)                                                             # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                        # Grab the tidied dataframe of lat-longs
    mutate(Vertical_velocity = as.numeric(stratify(nc_vel, Shallow_mark_W, sww)),     # Collapse shallow DIN into 2D and convert to long format
           Vertical_diffusivity = as.numeric(stratify(nc_dif, Shallow_mark_W, sww)), # Collapse shallow chlorophyll into 2D and convert to long format
           Depth = "S")                                                        # Introduce depth column
  
  deep <- output %>%                                                           # Grab the tidied dataframe of lat-longs
    mutate(Vertical_velocity = as.numeric(stratify(nc_vel, Deep_mark_W, dww)),
           Vertical_diffusivity = as.numeric(stratify(nc_dif, Deep_mark_W, dww)),
           Depth = "D")                                                        # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                              # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                                 # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull vertical veolcity and vertical eddy diffusivity from W files 
get_merid <- function(path, file)              {
  
  print(str_glue("{file} Extracting Meridional currents"))
  nc_raw <- nc_open(paste0(path, file))                                      # Open up a netcdf file to see it's raw contents (var names)
  nc_merid <- ncvar_get(nc_raw, "vomecrty", start3D, count3D)                # Pull meridinal currents
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                      # Grab the tidied dataframe of lat-longs
    mutate(Meridional = as.numeric(stratify(nc_merid, Shallow_mark, sw)),    # Collapse shallow meridional currents into 2D and convert to long format
           Depth = "S")                                                      # Introduce depth column
  
  deep <- output %>%                                                         # Grab the tidied dataframe of lat-longs
    mutate(Meridional = as.numeric(stratify(nc_merid, Deep_mark, dw)),       # Collapse, reshape and append deepwater data
           Depth = "D")                                                      # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                            # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                               # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull meridinal currents from grid_V_ files
get_zonal <- function(path, file)              {
  
  print(str_glue("{file} Extracting Zonal currents"))
  nc_raw <- nc_open(paste0(path, file))                                      # Open up a netcdf file to see it's raw contents (var names)
  nc_zonal <- ncvar_get(nc_raw, "vozocrtx", start3D, count3D)                # Pull zonal current
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                      # Grab the tidied dataframe of lat-longs
    mutate(Zonal = as.numeric(stratify(nc_zonal, Shallow_mark, sw)),         # Collapse shallow meridional currents into 2D and convert to long format
           Depth = "S")                                                      # Introduce depth column
  
  deep <- output %>%                                                         # Grab the tidied dataframe of lat-longs
    mutate(Zonal = as.numeric(stratify(nc_zonal, Deep_mark, dw)),            # Collapse, reshape and append deepwater data
           Depth = "D")                                                      # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                            # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                               # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull zonal currents from grid_U_ files
get_detritus <- function(file)                 {
  
  print(str_glue("Extracting detrital nitrogen"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_detritus <- ncvar_get(nc_raw, "DET", start3D, count3D)                  # Pull detritus
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  
  shallow <- output %>%                                                      # Grab the tidied dataframe of lat-longs
    mutate(Detritus = as.numeric(stratify(nc_detritus, Shallow_mark, sw)),   # Collapse shallow meridional currents into 2D and convert to long format
           Depth = "S")                                                      # Introduce depth column
  
  deep <- output %>%                                                         # Grab the tidied dataframe of lat-longs
    mutate(Detritus = as.numeric(stratify(nc_detritus, Deep_mark, dw)),      # Collapse, reshape and append deepwater data
           Depth = "D")                                                      # Collapse, reshape and append deepwater data
  
  all <- rbind(shallow, deep) %>%                                            # Bind both sets, this pipeline avoids computationally demanding reshaping
    filter(Shore_dist > 0) %>%                                               # Remove points on land
    mutate(Depth = as.factor(Depth))  
  
  return(all)                                                           
}    # Pull zonal currents from grid_U_ files

type_in_month <- function(data)                {
  
  Type <- data[1,3]                                                         # Pull type from the file
  
  if(Type == "grid_T_") get <- get_sea                                      # Change the extracting function based on file contents
  if(Type == "grid_U_") get <- get_zonal
  if(Type == "grid_V_") get <- get_merid
  if(Type == "grid_W_") get <- get_vertical
  if(Type == "icemod_") get <- get_ice
  if(Type == "ptrc_T_") get <- get_bio
  
  Month.type <- data %>%                                                    # Take the year
    mutate(data = purrr::map2(Path, File, get)) %>%                         # Extract data from each file
    unnest(data) %>%                                                        # Extract all encoded data
    group_by(Longitude, Latitude, Depth, .drop=FALSE) %>%
    summarise_if(is.numeric, mean)
  return(Month.type)                                                  
}    # Extract the data from files in a month and average by depth
whole_month <- function(data)                  {
  
  Month <- data[1,5] ; Year <- data[1,4]                                      # Pull date
  
  Month <- split(data, f = list(data$Type)) %>%                             # Split out the files for this month by type, so they can be averaged together
    purrr::map(type_in_month) %>%                                           # Pull a whole month of data from a single file type
    reduce(full_join) %>%                                                   # Join together all the data packets
  # right_join(spine) %>%     # a)                                          # Cut out rows outside of polygons and attach compartment labels
    right_join(Window) %>%    # b)                                          # Cut out rows outside of plotting window
    saveRDS(., file = paste("./Objects/Months/NM", Month, Year, "rds", sep = "."))    # save out a data object for one whole month
}    # Collate the different datasources from a single month
detritus_month <- function(data)               {
  
  Month <- data[1,3] ; Year <- data[1,2]                                    # Pull date
  
  Month <- data %>%                                                         # Take the year
    mutate(data = purrr::map(data$value, get_detritus)) %>%                 # Extract detritus data from each file
    unnest(data) %>%                                                        # Extract all encoded data
    group_by(Longitude, Latitude, Depth, .drop=FALSE) %>%
    summarise_if(is.numeric, mean) %>% 
    right_join(Window) %>%                                                  # Cut out rows outside of plotting window
    saveRDS(., file = paste("./Objects/Detritus/Det", Month, Year, "rds", sep = "."))    # save out a data object for one whole month
}    # Collate the different datasources from a single month
Big_Currents <- function(data)                 {
  
  Month <- data[1,5] ; Year <- data[1,4]                                      # Pull date
  
  Month <- split(data, f = list(data$Type)) %>%                             # Split out the files for this month by type, so they can be averaged together
    purrr::map(type_in_month) %>%                                           # Pull a whole month of data from a single file type
    reduce(full_join) %>%                                                   # Join together all the data packets
    saveRDS(., file = paste("./Objects/Currents/c", Month, Year, "rds", sep = "."))    # save out a data object for one whole month
#return(Month)
}    # Pull just current data without cropping to domains, to allow boundary sampling

#### NEMO - MEDUSA drivers extraction ####

## used for Light and air temperature data which goes into NM, this data uses a different grid and has time stored differently

Window <- function(file, w, e, s, n)           {
  
  #file <- examples[1,]$File ; w = 0 ; e = 180 ; s = 0 ; n = 90
  
  raw <- nc_open(file)
  lon <- raw$dim$longitude$vals %>% between(w, e)
  W <- min(which(lon == TRUE))
  E <- max(which(lon == TRUE))
  
  lat <- raw$dim$latitude$vals %>% between(s, n)
  S <- min(which(lat == TRUE))
  N <- max(which(lat == TRUE))
  
  lons <- raw$dim$longitude$vals[W:E]
  lats <- raw$dim$latitude$vals[S:N]
  
  Limits <- data.frame("Lon_start" = W, "Lon_count" = E - W + 1, "Lat_start" = S, "Lat_count" = N - S + 1)
  
  Limits <- list(Lats = lats, Lons = lons, Limits = Limits)
  return(Limits)
}    # Extract the positions to clip the netcdf file to, and the values for the smaller grid

get_air <- function(File, Type, Year)          {
  
  #File <- Airtemp_files$File[1] ; Type <- Airtemp_files$Type[1] ; Year <- Airtemp_files$Year[1] # test
  if(Type == "SWF") months <- Light_months                                     # Get the file holding the months
  if(Type == "T150") months <- Airtemp_months                                  # For the time steps of this data
  
  nc_raw <- nc_open(File)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_var <- ncvar_get(nc_raw, Type, c(Space$Limits$Lon_start, Space$Limits$Lat_start, 1),  # Extract the variable of interest
                      c(Space$Limits$Lon_count, Space$Limits$Lat_count, -1)) # cropped to window, with all time steps
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  
  Data <- as.data.frame.table(nc_var, responseName = "Measured") %>%         # Reshape array as dataframe
    rename(Longitude = Var1, Latitude = Var2, Time_step = Var3) %>%          # Name the columns
    mutate(Longitude = rep(rep(Space$Lons,                                   # Replace the factor levels with dimension values
                               times = length(unique(Latitude))), times = length(unique(Time_step))),
           Latitude = rep(rep(Space$Lats,  
                              each = length(unique(Longitude))), times = length(unique(Time_step))),
           Time_step = rep(1:length(unique(Time_step)), 
                           each = length(unique(Latitude)) * length(unique(Longitude)))) %>% 
    left_join(domains_mask) %>%                                              # Crop to domain
    drop_na() %>%         
    left_join(months) %>%                                                    # Assign a month to each time step
    mutate(Year = Year,                                                      # Attach Year
           Type = Type)                                                      # Attach variable name
  
  if(Type == "SWF") Data <- group_by(Data, Month, Year, Type)                # We don't need to bother accounting for shore in light data
  if(Type == "T150") Data <- group_by(Data, Month, Year, Type, Shore)        # We care about shore for temperature, retain interesting columns
    
  Summary <- summarise(Data, Measured = mean(Measured))                      # Average by time step.
  
  return(Summary)
}    # Pull light or air temp and create monthly time series per zone

#### Data averaging                   ####

decadal <- function(saved)                     {
  
  import <- readRDS(file = saved) %>%                                   # Read in wide format data file
 #   select(-c(geometry, weights, Day, Bathymetry, Shore_dist)) %>%
    select(-c(weights, Day, Bathymetry, Shore_dist)) %>%
    rename(Decade = Year) 
  
  str_sub(import$Decade, -1, -1) <- "0"                                 # Overwite the 4th digit with a 0 to get the decade
  
  return(import)
}    # Read in files and relevant columns, make a decade column
strip_ice <- function(data)                    {
  if(data$Depth[1] == "D") {select(data, -c(starts_with("Ice"), Snow_Thickness))} else data}    # Strip snow and ice variables if in deep depth zone
summarise_sp <- function(decade)               {
  
  Averaged <- decade %>%
    group_by(Longitude, Latitude, Decade, Month, Shore, Depth) %>%    # Group by pixel and decade
    summarise_all(mean, na.rm = TRUE) %>%                              # Average data columns
    ungroup()                                                          # Ungroup
  return(Averaged)
}    # Average all columns over decade by pixel for spatial analysis
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}    # Function to pull the geometry column of an SF object into XY
reproj <- function(data)                       {
  
  data %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% # Specify original projection (crs) 
    st_transform(crs = crs) %>%                                   # Transform to crs specified in region file
    sfc_as_cols() %>%                                             # Extract geometry column for geom_segment to work
    st_set_geometry(NULL)                                         # Chuck geometry column
}    # Get an SF projected XY from Lat/lon
  
summarise_ts <- function(saved)                {
  
  # saved <- "./Objects/Months/NM.1.1981.rds"
  
  # test <- readRDS(file = saved) %>%                                    # Read in wide format data file
  #   filter(!weights < 0) %>% 
  #   mutate(weights = na_if(weights, 0)) %>%                                        # Replace 0 weights with NA so vector lengths match for weighted mean
  #   drop_na(Year, Shore) %>% 
  #   group_by(Shore, Year, Month, Depth)
  # 
  # summarise(test, Salinity_avg = weighted.mean(Salinity, weights, na.rm = TRUE),   # Get monthly mean salinity
  #                 Salinity_sd = weighted.sd(Salinity, weights, na.rm = TRUE))  # Get monthly mean salinity
  
  ## Try and get the vertical SDs to work
  
  Groups <- readRDS(file = saved) %>%                                          # Read in wide format data file
    filter(!weights < 0) %>%                                                   # Drop points on land
    mutate(weights = na_if(weights, 0)) %>%                                    # Replace 0 weights with NA so vector lengths match for weighted mean
    drop_na(Year, Shore) %>%                                                   # Drop points outside of the polygons
    group_by(Shore, Year, Month, Depth)
  
  Ice <- filter(Groups, Ice_pres > 0) %>%                                      # Remove ice free pixels before averaging thicknesses 
    summarise(Ice_Thickness_avg = mean(Ice_Thickness, na.rm = TRUE),           # Get monthly mean sea ice thickness             
              Snow_Thickness_avg = mean(Snow_Thickness, na.rm = TRUE),         # Get monthly mean snow thickness             
              # SD 
              Ice_Thickness_sd = sd(Ice_Thickness, na.rm = TRUE),              # Get monthly mean sea ice thickness             
              Snow_Thickness_sd = sd(Snow_Thickness, na.rm = TRUE))            # Get monthly mean snow thickness             
  
  Averaged <- Groups %>%
    summarise(Salinity_avg = weighted.mean(Salinity, weights, na.rm = TRUE),   # Get monthly mean salinity
              Temperature_avg = weighted.mean(Temperature, weights, na.rm = TRUE),
              DIN_avg = weighted.mean(DIN, weights, na.rm = TRUE),
              Chlorophyll_avg = weighted.mean(Chlorophyll, weights, na.rm = TRUE),
              Ice_pres = mean(Ice_pres, na.rm = TRUE),  #  ** needs to be scaled differently for compartment areas # fraction of pixels covered by ice             
              Ice_conc_avg = mean(Ice_conc, na.rm = TRUE),                     # Get monthly mean sea ice concentration             
              Vertical_diffusivity_avg = weighted.mean(Vertical_diffusivity, weights, na.rm = TRUE),
              Vertical_velocity_avg = weighted.mean(Vertical_velocity, weights, na.rm = TRUE),
              Meridional_avg = weighted.mean(Meridional, weights, na.rm = TRUE),
              Zonal_avg = weighted.mean(Zonal, weights, na.rm = TRUE),
              # SD 
              Salinity_sd = weighted.sd(Salinity, weights, na.rm = TRUE),      # Get monthly mean salinity
              Temperature_sd = weighted.sd(Temperature, weights, na.rm = TRUE),
              DIN_sd = weighted.sd(DIN, weights, na.rm = TRUE),
              Chlorophyll_sd = weighted.sd(Chlorophyll, weights, na.rm = TRUE),
              Ice_conc_sd = sd(Ice_conc, na.rm = TRUE),                        # Get monthly mean sea ice concentration             
              #Vertical_diffusivity_sd = weighted.sd(Vertical_diffusivity, weights, na.rm = TRUE),
              #Vertical_velocity_sd = weighted.sd(Vertical_velocity, weights, na.rm = TRUE),
              Meridional_sd = weighted.sd(Meridional, weights, na.rm = TRUE),
              Zonal_sd = weighted.sd(Zonal, weights, na.rm = TRUE)) %>%  
      left_join(Ice) %>%                                                              # Add in ice and snow thicknesses
      ungroup()
  
  return(Averaged) }    # Calculate monthly mean and SD per compartment as time series
summarise_ts_detritus <- function(saved)       {
 
  # saved <- "./Objects/Detritus/Det.1.1988.rds"
  
  Averaged <- readRDS(file = saved) %>%                                        # Read in wide format data file
    filter(!weights < 0) %>%                                                   # Drop points on land
    mutate(weights = na_if(weights, 0)) %>%                                    # Replace 0 weights with NA so vector lengths match for weighted mean
    drop_na() %>%                                                              # Drop points outside of the polygons and without weights
    group_by(Shore, Year, Month, Depth) %>% 
    summarise(Detritus_avg = weighted.mean(Detritus, weights, na.rm = TRUE),   # Get monthly mean detrital nitrogen
              Detritus_sd = weighted.sd(Detritus, weights, na.rm = TRUE)) %>%  
    ungroup()
  
  return(Averaged) }    # Calculate monthly mean and SD per compartment as time series

#### Plotting functions               ####

ts_plot <- function(var)                       {
  
  ts <- ggplot(TS, aes(x=date, y= get(var), colour = Compartment)) +
    geom_line(size = 0.2) +
    geom_smooth(span = 0.008, size = 0.2, se = FALSE) +
    theme_minimal() +
    theme(legend.position = "top") +
    labs(caption = paste("NM", var, "time series by compartment"), y = var) +
    NULL
  ggsave(paste0("./Figures/NEMO-MEDUSA/TS_", var, ".png"), plot = ts, width = 16, height = 10, units = "cm", dpi = 500)
  
}    # Plot any of the variables as a time series
point_plot <- function(data, var)              {
  
  decade <- data$Decade[1]; depth <- data$Depth[1]                             # Find out what the data is
  
  if(depth == "D" & var %in% c("Ice", "Ice_conc", "Ice_Thickness", "Snow_Thickness")) {
    print("Skipped deep ice plot") } else {
      
      print(paste("plotting", var, "for", decade, depth))                          # Show things are working
      
      map <- ggplot() +                                                            # Create the base
        coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
        theme_minimal() +
        labs(title = paste("Decade:", decade), 
             subtitle = paste("Water layer:", depth), x = NULL, y = NULL) +
        geom_point(data = data, aes(x=x, y=y, colour = get(var)), stroke = 0, size = 1.1, na.rm = TRUE) + 
        scale_colour_viridis(option = "viridis", name = var, na.value = "red") +
        facet_wrap(vars(Month)) +
        geom_sf(data = world) +
        # bathymetry
        new_scale_colour() +   
        geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
        scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
        zoom +
        NULL
      ggsave(paste0("./Figures/NEMO-MEDUSA/grids/map ", var, " ", depth, " ", decade, ".png"), 
             plot = map, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
    } 
}    # Plot any of the variables showing just points
stick_plot <- function(data)        {
  
  #data <- SP[[1]]
  
  current_uv_scalar <- 500000                                        # Establish the vector scalar for the currents
  
  direction <- slice(data, seq(1,nrow(data), by = 50)) %>% drop_na()  # Take a subset of arrows to plot for legibility, dropping arrows with NAs
  
  sticks <- ggplot(data) +
    geom_point(aes(x=x, y=y, colour = log(abs(Zonal)+abs(Meridional))+1), size = 0.65, stroke = 0, shape = 16) +
    scale_colour_viridis(option = "viridis", name = "Current\nspeed (m/s)", na.value = NA) +
    # Bathymetry
    new_scale_colour() +   
    geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.1, show.legend = "line") +
    scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "red", "-200" = "pink" , "-30" = "white")) +
    # Sticks
    geom_segment(data = direction, aes(x=x, xend = x + (Zonal * current_uv_scalar), 
                                       y=y, yend = y + (Meridional * current_uv_scalar)), colour = "black", size = 0.1) +
    geom_point(data = direction, aes(x=x, y=y), colour = "white", size = 0.2, stroke = 0, shape = 16) +
    # Land
    geom_sf(data = world, fill = "black", size = 0.2) +
    zoom +
    theme_minimal() +
    theme(panel.background = element_rect(fill="black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(caption = paste("Decade:", unique(data$Decade), "    Water layer:", unique(data$Depth)), x = NULL, y = NULL) +
    NULL
  
  ggsave_map(paste("./Figures/NEMO-MEDUSA/currents/currents ", unique(data$Depth), unique(data$Decade), ".png"), plot = sticks)
  # return(sticks)
}    # Plot currents, this adds arrows to the plot
