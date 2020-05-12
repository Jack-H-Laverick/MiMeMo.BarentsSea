
##**## A tidy place to keep functions and reduce clutter in programmes

#### Nemo - Medusa Data Extraction ####

empty <- function(x) all(is.na(x))                      # Quick function looking for areas with no data
get_weights <- function(top, bottom)           {
  #top <- 200                                                                    # shallowest depth of the slice
  #bottom <- 1000                                                                # What's the bottom of the slice? for incorporating into a function
  
  weights <- array(NA, c(490,264,64))                                          # Initialise an array to hold weights
  first <- weights[,,1]
  first[] <- mean(Space$nc_depth[1:2]) - top %>% rep(times = 129360)           # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(Space$nc_depth[1:2])                              # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                    # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,64] <- mask_bathy - mean(Space$nc_depth[63:64])                    # The remaining water column thickness is the sea floor - the deepest midpoint. 
  
for (i in 2:63) { 
#i <- 23  
  last_midpoint <- mean(Space$nc_depth[(i-1):i])                               # Find the mid depth to the layer above
  next_midpoint <- mean(Space$nc_depth[i:(i+1)])                               # Find the mid depth to the layer below

  if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
  if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice

  weights[,,i] <- below - above %>% rep(times = 129360)                        # Calculate layer thickness and repeat to fill the array

  marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
  weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
      
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA 
  
  return(weights)
}    # Return the weights for averaging across depths, specifying the top and bottom layer of a slice
get_weights.pred <- function(top, bottom)      {

  weights <- array(NA, c(490,264,75))                                            # Initialise an array to hold weights
  first <- weights[,,1]
  first[] <- mean(Space.pred$nc_depth[1:2]) - top %>% rep(times = 129360)                # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(Space.pred$nc_depth[1:2])                                   # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                      # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,75] <- mask_bathy - mean(Space.pred$nc_depth[74:75])                         # The remaining water column thickness is the sea floor - the deepest midpoint. 
  
  for (i in 2:74) { 
    #i <- 23  
    last_midpoint <- mean(Space.pred$nc_depth[(i-1):i])                                  # Find the mid depth to the layer above
    next_midpoint <- mean(Space.pred$nc_depth[i:(i+1)])                                  # Find the mid depth to the layer below
    
    if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
    if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice
    
    weights[,,i] <- below - above %>% rep(times = 129360)                        # Calculate layer thickness and repeat to fill the array
    
    marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
    weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
    
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA 
  
  return(weights)
}    # Return the weights for averaging across depths, specifying the top and bottom layer of a slice

# max_weights <- apply(weights, 3, max, na.rm = TRUE)
# 
# max_weights[1:5] ; max_weights[6:20] ; max_weights[21:35]
# 
# weights[,,30]
# 
# diff(Space$nc_depth)
# 
# sum(max_weights[21:35])#
# 
# #empties[empties] <- 1                                                         # rework so it can be visualised
# check <- data.frame(Longitude = output$Longitude, Latitude = output$Latitude, OOB = as.numeric(weighted_mean)) %>%
#   #  filter(OOB == 1) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Specify original projection (crs) 
#   st_transform(crs = 3035) %>%                                              # Transform to new polar centric projection
#   sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
#   st_set_geometry(NULL) %>%                                                 # chuck out geometry column
#   filter(between(x, 2600000, 6300000) & between(y, 4000000, 7700000))       # Remove points you won't see to contain colour scale
# 
# #is.nan(check$OOB) <- {NA} # **** figure this out
# 
# world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
#   st_transform(crs = 3035)                                                  # Assign polar projection
# 
# ggplot() +
#   geom_point(data = check, aes(x = x, y = y, colour = OOB), size = 0.5) +
#   scale_colour_viridis(option = "viridis", na.value = NA) +
#   # Land
#   geom_sf(data = world, fill = "grey", size = 0.2) +
#   coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
#   theme_minimal() +
#   NULL


#start = c(lim1, Equator), 
#count =c((lim2-lim1+1), (43200 - Equator+1)) # Extract a matrix of all the concentration estimates, reading in to the clipped area




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

get_depth <- function(file)                    {
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_depth <- ncvar_get(nc_raw, "deptht")                                    # Extract a matrix of depths
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  return(nc_depth)
}    # Pull depths from netcdf
get_spatial <- function(file)                  {
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_lat <- ncvar_get(nc_raw, "nav_lat")                                     # Extract a matrix of all the latitudes
  nc_lon <- ncvar_get(nc_raw, "nav_lon")                                     # Extract a matrix of all the longitudes
  nc_depth <- ncvar_get(nc_raw, "deptht")                                    # Extract a matrix of depths
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  all <- list("nc_lat" = nc_lat, "nc_lon" = nc_lon, "nc_depth" = nc_depth)
  return(all)
}    # Pull spatial structure netcdf file
get_spatial_clipped <- function(file)          {
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_lat <- ncvar_get(nc_raw, "nav_lat", start.ref, count.ref)               # Extract a matrix of all the latitudes
  nc_lon <- ncvar_get(nc_raw, "nav_lon", start.ref, count.ref)               # Extract a matrix of all the longitudes
  nc_depth <- ncvar_get(nc_raw, "deptht")                                    # Extract a matrix of depths
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  all <- list("nc_lat" = nc_lat, "nc_lon" = nc_lon, "nc_depth" = nc_depth)
  return(all)
}    # Pull spatial structure netcdf file
get_sea   <- function(file)                    {
  
  print(str_glue("getting Salinity, Temperature, and Sea Ice concentration data from {file}"))
  nc_raw <- nc_open(file)                                                      # Open up a netcdf file to see it's raw contents (var names)
  nc_saline <- ncvar_get(nc_raw, "vosaline", start3D, count3D)                 # Extract an array of salinities
  nc_temp <- ncvar_get(nc_raw, "votemper", start3D, count3D)                   # Extract an array of temperatures
  nc_ice <- ncvar_get(nc_raw, "soicecov", start2D, count2D)                    # Extract a matrix of ice fractions
  nc_turbocline <- ncvar_get(nc_raw, "somixhgt", start2D, count2D)             # Extract a matrix of turbocline depths
  nc_mixed <- ncvar_get(nc_raw, "somxl010", start2D, count2D)                  # Extract a matrix of mixed layer depths
  nc_close(nc_raw)                                                             # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                   # Grab the tidied dataframe of lat-longs
    mutate(Ice = as.numeric(nc_ice),                                           # Append new variable to coordinates (no depths for ice)
           Turbocline = as.numeric(nc_turbocline),                             # Append new variable to coordinates (no third dimension for turbocline)
           Mixed = as.numeric(nc_mixed),                                       # Append new variable to coordinates (no third dimension for mixed layer depth)
           S_Salinity = as.numeric(stratify(nc_saline, Shallow_mark, sw)),     # Collapse shallow salinity into 2D and convert to long format
           S_Temperature = as.numeric(stratify(nc_temp, Shallow_mark, sw)),    # Collapse shallow temperatures into 2D and convert to long format
           M_Salinity = as.numeric(stratify(nc_saline, Middle_mark, mw)),
           M_Temperature = as.numeric(stratify(nc_temp, Middle_mark, mw)),     # Collapse, reshape and append midwater data
           D_Salinity = as.numeric(stratify(nc_saline, Deep_mark, dw)),
           D_Temperature = as.numeric(stratify(nc_temp, Deep_mark, dw)))       # Collapse, reshape and append deepwater data
}    # Pull ice, turbocline, mixed layer depth, from S files and then salinity and temperature  in three depth bands
get_bio   <- function(file)                    { 
  print(str_glue("getting Dissolved Inorganic Nitrogen and Chlorophyll data from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_DIN <- ncvar_get(nc_raw, "DIN", start3D, count3D)                                         # Extract a matrix for the variable
  nc_CHD <- ncvar_get(nc_raw, "CHD", start3D, count3D)
  nc_CHN <- ncvar_get(nc_raw, "CHN", start3D, count3D)
  nc_Chl <- nc_CHD + nc_CHN ; rm(nc_CHD, nc_CHN)
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_DIN = as.numeric(stratify(nc_DIN, Shallow_mark, sw)),           # Collapse shallow DIN into 2D and convert to long format
           S_Chlorophyll = as.numeric(stratify(nc_Chl, Shallow_mark, sw)),   # Collapse shallow chlorophyll into 2D and convert to long format
           M_DIN = as.numeric(stratify(nc_DIN, Middle_mark, mw)),
           M_Chlorophyll = as.numeric(stratify(nc_Chl, Middle_mark, mw)),    # Collapse, reshape and append midwater data
           D_DIN = as.numeric(stratify(nc_DIN, Deep_mark, dw)),
           D_Chlorophyll = as.numeric(stratify(nc_Chl, Deep_mark, dw)))      # Collapse, reshape and append deepwater data
}    # Pull nitrogen and chlorophyll from P files 
get_merid <- function(file)                    {
  print(str_glue("getting Meridional current data from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_merid <- ncvar_get(nc_raw, "vomecrty", start3D, count3D)                # Pull meridinal currents
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Meridional = as.numeric(stratify(nc_merid, Shallow_mark, sw)),  # Collapse shallow meridional currents into 2D and convert to long format
           M_Meridional = as.numeric(stratify(nc_merid, Middle_mark, mw)),   # Collapse, reshape and append midwater data
           D_Meridional = as.numeric(stratify(nc_merid, Deep_mark, dw)))     # Collapse, reshape and append deepwater data
}    # Pull meridinal currents from V files
get_zonal <- function(file)                    {
  print(str_glue("getting Zonal current data from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_zonal <- ncvar_get(nc_raw, "vozocrtx", start3D, count3D)                # Pull zonal current
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Zonal = as.numeric(stratify(nc_zonal, Shallow_mark, sw)),       # Collapse shallow meridional currents into 2D and convert to long format
           M_Zonal = as.numeric(stratify(nc_zonal, Middle_mark, mw)),        # Collapse, reshape and append midwater data
           D_Zonal = as.numeric(stratify(nc_zonal, Deep_mark, dw)))          # Collapse, reshape and append deepwater data
}    # Pull zonal currents from U files
get_sal.pred <- function(file)                 {
  print(str_glue("getting Salinity predictions from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_saline <- ncvar_get(nc_raw, "vosaline", start3D, count3D)                                  # Pull meridinal currents
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Salinity = as.numeric(stratify(nc_saline, Shallow_mark_pred, swp)),  # Collapse shallow meridional currents into 2D and convert to long format
           M_Salinity = as.numeric(stratify(nc_saline, Middle_mark_pred, mwp)),   # Collapse, reshape and append midwater data
           D_Salinity = as.numeric(stratify(nc_saline, Deep_mark_pred, dwp)))     # Collapse, reshape and append deepwater data
}    # Pull predictions for salinity
get_temp.pred <- function(file)                {
  print(str_glue("getting Temperature predictions from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_temp <- ncvar_get(nc_raw, "votemper", start3D, count3D)                                  # Pull meridinal currents
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Temperature = as.numeric(stratify(nc_temp, Shallow_mark_pred, swp)),  # Collapse shallow meridional currents into 2D and convert to long format
           M_Temperature = as.numeric(stratify(nc_temp, Middle_mark_pred, mwp)),   # Collapse, reshape and append midwater data
           D_Temperature = as.numeric(stratify(nc_temp, Deep_mark_pred, dwp)))     # Collapse, reshape and append deepwater data
}    # Pull predictions for temperature
get_merid.pred <- function(file)               {
  print(str_glue("getting Meridional current predictions from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_merid <- ncvar_get(nc_raw, "vomecrty", start3D, count3D)                                  # Pull meridinal currents
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Meridional = as.numeric(stratify(nc_merid, Shallow_mark_pred, swp)),  # Collapse shallow meridional currents into 2D and convert to long format
           M_Meridional = as.numeric(stratify(nc_merid, Middle_mark_pred, mwp)),   # Collapse, reshape and append midwater data
           D_Meridional = as.numeric(stratify(nc_merid, Deep_mark_pred, dwp)))     # Collapse, reshape and append deepwater data
}    # Pull predictions for meridinal current
get_zonal.pred <- function(file)               {
  print(str_glue("getting Zonal current predictions from {file}"))
  nc_raw <- nc_open(file)                                                    # Open up a netcdf file to see it's raw contents (var names)
  nc_zonal <- ncvar_get(nc_raw, "vozocrtx", start3D, count3D)                                  # Pull zonal current
  nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  output %>%                                                                 # Grab the tidied dataframe of lat-longs
    mutate(S_Zonal = as.numeric(stratify(nc_zonal, Shallow_mark_pred, swp)), # Collapse shallow meridional currents into 2D and convert to long format
           M_Zonal = as.numeric(stratify(nc_zonal, Middle_mark_pred, mwp)),  # Collapse, reshape and append midwater data
           D_Zonal = as.numeric(stratify(nc_zonal, Deep_mark_pred, dwp)))    # Collapse, reshape and append deepwater data
}    # Pull predictions for zonal current

summarise_months <- function(month, get, type) {
  
  if(type == "S") {
    Monthly <- month %>%                                                       # Take the year
      mutate(data = purrr::map(value, get)) %>%                                # Extract data from each file
      unnest(data) %>%                                                         # Extract all encoded data
      #   drop_na() %>%                                                            # Drop NAs
      group_by(Longitude, Latitude, Year, Month) %>%                           # Group by pixel and time step
      summarise(S_Ice = mean(Ice, na.rm = TRUE),                               # Get monthly mean sea ice             
                S_Turbocline = min(Turbocline, na.rm = TRUE),                  # Get monthly minimum turbocline depth
                S_Mixed = min(Mixed, na.rm = TRUE),                            # Get monthly minimum mixed layer depth
                S_Salinity = mean(S_Salinity, na.rm = TRUE),                   # Get monthly mean salinity in the shallow depth layer
                S_Temperature = mean(S_Temperature, na.rm = TRUE),      
                M_Salinity = mean(M_Salinity, na.rm = TRUE),
                M_Temperature = mean(M_Temperature, na.rm = TRUE),      
                D_Salinity = mean(D_Salinity, na.rm = TRUE),
                D_Temperature = mean(D_Temperature, na.rm = TRUE))  %>%        # Calculate monthly average at each pixel
      ungroup()
    return(Monthly) }                                                        # You will need to mutate different columns depending on
  if(type == "P") {                                             
    Monthly <- month %>%                                                     # Take the year
      mutate(data = purrr::map(value, get)) %>%                              # Extract data from each file
      unnest(data) %>%                                                       # Extract all encoded data
      #     drop_na() %>%                                                          # Drop NAs
      group_by(Longitude, Latitude, Year, Month) %>%                         # Group by pixel and time step
      summarise(S_DIN = mean(S_DIN, na.rm = TRUE),  
                S_Chlorophyll = mean(S_Chlorophyll, na.rm = TRUE),      
                M_DIN = mean(M_DIN, na.rm = TRUE),
                M_Chlorophyll = mean(M_Chlorophyll, na.rm = TRUE),      
                D_DIN = mean(D_DIN, na.rm = TRUE),
                D_Chlorophyll = mean(D_Chlorophyll, na.rm = TRUE))  %>%                    # Calculate monthly average at each pixel
      ungroup()
    return(Monthly) }                                                        # the extraction function used
  if(type %in% c("V", "V.pred")) {                                                          # the extraction function used
    Monthly <- month %>%                                                     # Take the year
      mutate(data = purrr::map(value, get)) %>%                              # Extract data from each file
      unnest(data) %>%                                                       # Extract all encoded data
      #     drop_na() %>%                                                          # Drop NAs
      group_by(Longitude, Latitude, Year, Month) %>%                         # Group by pixel and time step
      summarise(S_Meridional = mean(S_Meridional, na.rm = TRUE),  
                M_Meridional = mean(M_Meridional, na.rm = TRUE),
                D_Meridional = mean(D_Meridional, na.rm = TRUE))  %>%                      # Calculate monthly average at each pixel
      ungroup()
    return(Monthly) }
  if(type %in% c("U", "U.pred")) {                                                          # the extraction function used
    Monthly <- month %>%                                                     # Take the year
      mutate(data = purrr::map(value, get)) %>%                              # Extract data from each file
      unnest(data) %>%                                                       # Extract all encoded data
      #     drop_na() %>%                                                          # Drop NAs
      group_by(Longitude, Latitude, Year, Month) %>%                         # Group by pixel and time step
      summarise(S_Zonal = mean(S_Zonal, na.rm = TRUE),  
                M_Zonal = mean(M_Zonal, na.rm = TRUE),
                D_Zonal = mean(D_Zonal, na.rm = TRUE))  %>%                                # Calculate monthly average at each pixel
      ungroup()
    return(Monthly)}                                        
  if(type == "S.pred") {                                                          # the extraction function used
    Monthly <- month %>%                                                     # Take the year
      mutate(data = purrr::map(value, get)) %>%                              # Extract data from each file
      unnest(data) %>%                                                       # Extract all encoded data
      group_by(Longitude, Latitude, Year, Month) %>%                         # Group by pixel and time step
      summarise(S_Salinity = mean(S_Salinity, na.rm = TRUE),                   # Get monthly mean salinity in the shallow depth layer
                M_Salinity = mean(M_Salinity, na.rm = TRUE),
                D_Salinity = mean(D_Salinity, na.rm = TRUE))  %>%            # Calculate monthly average at each pixel
      ungroup()
    return(Monthly)}                                                  
  if(type == "T.pred") {                                                          # the extraction function used
    Monthly <- month %>%                                                     # Take the year
      mutate(data = purrr::map(value, get)) %>%                              # Extract data from each file
      unnest(data) %>%                                                       # Extract all encoded data
      group_by(Longitude, Latitude, Year, Month) %>%                         # Group by pixel and time step
      summarise(S_Temperature = mean(S_Temperature, na.rm = TRUE),      
                M_Temperature = mean(M_Temperature, na.rm = TRUE),      
                D_Temperature = mean(D_Temperature, na.rm = TRUE))  %>%     # Calculate monthly average at each pixel
      ungroup()
    return(Monthly)}                                                  
  
}    # Extract the data from files in a month and average
decadal        <- function(data)               {
  
str_sub(data$Year, -1, -1) <- "0"                       # Overwite the 4th digit with a 0 to get the decade
  
  decades <- group_by(data, Year, Month, Longitude, Latitude) %>%                         # Group by pixel and decade
    rename(decade = Year) %>%
    summarise_all(mean, na.rm = TRUE) %>%                                    # Average data columns
    ungroup() %>%                                                            # Ungroup
    mutate(decade = as.factor(decade))                                       # Change decade to factor
  return(decades)
}    # Average all columns over decade by pixel
combine        <- function(files)              {
  
  type <- files[1,5]                                                         # Pull type from the file
  
  if(type == "S") get <- get_sea                                             # Change the extracting function based on file contents
  if(type == "P") get <- get_bio
  if(type == "V") get <- get_merid
  if(type == "U") get <- get_zonal

  if(type == "S.pred") get <- get_sal.pred                                   # Change the extracting function based on file contents
  if(type == "T.pred") get <- get_temp.pred                                  # Change the extracting function based on file contents
  if(type == "V.pred") get <- get_merid.pred
  if(type == "U.pred") get <- get_zonal.pred
  
  combined <- files %>% 
    split(., f = list(.$Year, .$Month)) %>%                                  # Create a list of dataframes for each separate month 
    .[sapply(., function(x) dim(x)[1]) > 0] %>%                              # Drop empty dataframes (Months which weren't observed but split introduces)
    lapply(summarise_months, get, type) %>%                                  # Perform the extraction
    rbindlist() %>%                                                          # Collapse the list into a dataframe
    decadal() %>%
    saveRDS(., file = paste("~/Data/NEMO - MEDUSA/weighted",type, "rds", sep = ".")) # save out the data object
  # return(combined)
}    # Extract over months, average, combine
# combine function has a save line, comment out if you don't want to automatically overwrite, No object will be returned to the environment though

#### Nemo - MEdusa Reshaping for plotting ####

reproj <- function(data)                       {
  
  points <- mutate(data, Month = as.factor(Month)) %>%                      # Change  integers to factor
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%           # Specify original projection (crs) 
    st_transform(crs = 3035)                                                # Transform to new polar centric projection
  
  levels(data$Month) <- Month_list                                          # Relabel factor levels
  
  return(points)  
}    # Reprojection and month column cleaning
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}    # Function to pull the geometry column of an SF object into XY
longify <- function(data, strip)               {
  
  name <- substr(strip[1], 3, nchar(strip[1]))                              # Pull the variable name 
  
  work <- select(data, c(decade, Month, x, y, strip)) %>%                   # Pull columns of interest from main data file
    gather(key = "depth", value = !!name, strip) %>%                        # Reshape for ease in a plotting function 
    mutate(depth = as.factor(depth),                                        # Convert depth to a factor for plotting
           type = substr(!!name, 1,3))                                      # Pull a variable code for plotting by
  
  levels(work$depth) <- depth_levels                                        # Recode factor levels
  levels(work$Month) <- Month_list                             
  
  assign(paste0("long_",name), work)                                        # Assign to a name created by paste
}    # Convert to long format
chained <- function(saved, strip)              {
  
  Reformatted <- readRDS(file = saved) %>%                                    # Read in wide format data file
    reproj() %>%                                                              # Set to polar projection
    sfc_as_cols() %>%                                                         # Extract geometry column for geom_segment to work
    st_set_geometry(NULL) %>%                                                 # chuck out geometry column
    filter(between(x, 2600000, 6300000) & between(y, 4000000, 7700000)) %>%   # Remove points you won't see to contain colour scale
    lapply(strip, longify, data =.)                                           # Convert to a list of sub-divided long format dataframes
}    # Link all the above functions
split_depth <- function(data)                  { 
  split(data, f = data$depth) %>% 
    .[sapply(., function(x) dim(x)[1]) > 0] }    # Simplify the splitting lines for use with lapply
split_decade <- function(data)                 { 
  split(data, f = data$decade) %>% 
    .[sapply(., function(x) dim(x)[1]) > 0] }    # Simplify the splitting lines for use with lapply

#### Nemo - Medusa Plotting functions ####

point_plot <- function(data)                   {
  
  type <- data$type[1]; decade <- data$decade[1]; depth <- data$depth[1]          # Find out what the data is
  
  print(paste("plotting", type, "data for", decade, depth, "water"))              # Show things are working
  
  base <- ggplot() +                                                            # Create the base
    coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
    theme_minimal() +
    labs(title = paste("Decade:", decade), 
         subtitle = paste("Water layer:", depth), x = NULL, y = NULL)
  if (type == "Ice")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Ice), stroke = 0, size = 1.1, na.rm = TRUE) + 
      scale_colour_viridis(option = "viridis", name = "Sea ice\nconcentration", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
  if (type == "Tur")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Turbocline), stroke = 0, size = 1.1, na.rm = TRUE) + 
      scale_colour_viridis(option = "viridis", name = "Mean minimum\nturbocline depth", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
  if (type == "Mix")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Mixed), stroke = 0, size = 1.1, na.rm = TRUE) + 
      scale_colour_viridis(option = "viridis", name = "Mean minimum\nmixed layer depth", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
  if (type == "Tem")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Temperature), stroke = 0, size = 1.1) + 
      #        geom_raster(data = test, aes(x=long, y=lat, fill = layer)) + 
      scale_colour_viridis(option = "inferno", name = "Temperature", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "azure")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }                                                     # Add a variable specific data layer       
  if (type == "Sal")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Salinity), stroke = 0, size = 1.1) + 
      scale_colour_viridis(option = "viridis", name = "Salinity", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
  if (type == "DIN")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = DIN), stroke = 0, size = 1.1) + 
      scale_colour_viridis(option = "viridis", name = "DIN", na.value = "red") +
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
  if (type == "Chl")    {
    base <- base +
      geom_point(data = data, aes(x=x, y=y, colour = Chlorophyll), stroke = 0, size = 1.1) + 
      scale_colour_viridis(option = "viridis", name = "Chlorophyll", na.value = "red") +
      #scale_colour_gradientn(colours = viridis_pal()(9), limits=c(0, 3), oob = scales::squish) + 
      facet_wrap(vars(Month)) +
      geom_sf(data = world) +
      # bathymetry
      new_scale_colour() +   
      geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
      scale_colour_manual(name = 'Depth (m)', values = c("-1000" = "white", "-200" = "grey40", "-30" = "black")) +
      coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
      NULL
    ggsave(paste("FIG_", type, " ", depth, " ", decade, ".png", sep = ""), 
           plot = base, scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
  }
}    # Plot any of the variables showing just points
stick_plot <- function(data, direction)        {
  
  current_uv_scalar <- 500000                                                  # Establish the vector scalar for the currents
  
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
    coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill="black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(title = paste("Decade:", unique(data$decade)), subtitle = paste("Water layer:", unique(data$depth)), x = NULL, y = NULL) +
    NULL
  
  ggsave(paste("FIG_Currents", unique(data$depth), unique(data$decade), ".png"), plot = sticks, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
  # return(sticks)
}    # Plot currents, this adds arrows to the plot
