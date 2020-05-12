
##**## A tidy place to keep functions and reduce clutter in programmes

#### Extracting vertical water exchanges ####

avg_day <- function (Path, File) {
  
  V_interface <- nc_remap(paste0(Path, File), vars = c("vovecrtz", "votkeavt"), vert_depths = 60) %>% # Interpolate at 60 m
    filter(Latitude != 0) %>%                                                 # Remove incorrectlly labelled points (outside domain anyway)
    rename(Velocity = vovecrtz, `Eddy Diffusivity` = votkeavt) %>%            # Rename variables
    inner_join(points, .)                                                     # Limit to points of interest
  
  ## Check CDO has formed the grid correctly
  # ggplot() + geom_sf(data = V_interface, aes(colour = Velocity))       
  
  Velocity <- mutate(V_interface, Flow = ifelse(Velocity >= 0, "Upwelling", "Downwelling")) %>% # Label upwelling and downwelling
    #  group_by(Flow) %>%                                                     # Group for averaging (splitting up and down welling)
    summarise(Value = mean(Velocity))                                         # Calculate mean vertical veolcities for each region
  
  day <- group_by(V_interface) %>%                                            # Repeat grouping for eddy diffusivitity
    summarise(Value = mean(`Eddy Diffusivity`)) %>%                           # Average by region
    mutate(Flow = "Eddy Diffusivity") %>%                                     # Attach a label (so we can group by variable when plotting timeseries)
    bind_rows(., Velocity)                                                    # Combine summaries for the time step
  return(day)
}

avg_month <- function(data) {
  
  month <- data %>%                                                           # Take the month
    mutate(data = purrr::map2(Path, File, avg_day)) %>%                       # Extract and average data from each day
    unnest(data) %>%                                                          # unnext the column holding the extraction
    group_by(Year, Month, Flow) %>%                                           # Group by information we want to retain
    summarise(Value = mean(Value)) %>%                                        # Average the values within a month
    ungroup()                                                                 # Ungroup for speed
  return(month)
}

#### Making transects at the boundaries of model domains ####

to_segments <- function(line)          { 
  
  g = st_geometry(line) %>% st_cast("POINT")                                # Grab the geometry and extract the points defining the line
  hd = head(g, -1)                                                          # Take all points except the last
  tl = tail(g, -1)                                                          # Take all points except the first
  map2(hd, tl, function(x,y) st_combine(st_sfc(x, y, crs = crs))) %>%      # Pair the points across vectors
    purrr::map(function(x) st_cast(x, "LINESTRING"))                        # Turn pairs of points into lines
}                    # Break a linestring into segments at the corners

boundaries <- function(domain)         {
  
  segments <- domain %>%
    pull(geometry) %>%                                                        # Open the list column containing the geometry
    purrr::map(to_segments) %>%                                               # Apply function to break each line in turn at the corners
    unlist(recursive = FALSE) %>%                                             # Bring all the segments into the same level
    do.call(c, .)                                                             # Bind
  
  Length <- st_length(segments) %>%                                           # Add in the length of each segment for weighting
    as.data.frame() %>%
    rowid_to_column(var = "Segment") %>%                                      # Add an ID for each segment
    st_sf(geometry = segments) 
  return(Length)
}                    # Roll over each domain and measure lengths

#### Labelling transects ####

check_grid <- function(segment)        {

  latlon <- Weighted[segment,] %>% 
    st_transform(4326) %>%                      # Convert to latitude and longitude 
    st_coordinates() %>%                        # Pull the coordinates
    round(digits = 3)                           # Round to drop conversion error
  
  Lon_same <- latlon[1,"X"] == latlon[2, "X"]   # Are the longitudes the same either end of the line?
  Lat_same <- latlon[1,"Y"] == latlon[2, "Y"]   # Are the latitudes the same either end of the line?
  
  check <- c(Lon_same, Lat_same) %>%            # Connect queries as a row for a dataframe
    t() %>% 
    as.data.frame()
  
  return(check)
}                    # Check if the segment runs parallel to latitude or longitude

direction <- function(segment)         {

 #segment <- 1                                                   # Testing function
 #segment <- 24246                                               
  
  midpoint <- st_line_sample(Checked[segment,], n = 1)           # Grab the midpoint of a line segment
  
  if(Checked[segment,]$Shore == "Offshore") domain <- filter(domains, Shore == "Offshore")# Change the domain polygon to match the boundary segment 
  if(Checked[segment,]$Shore == "Inshore") domain <- filter(domains, Shore == "Inshore")  # Change the domain polygon to match the boundary segment 
  if(Checked[segment,]$current == "Meridional") {
    minus <- c(0, -0.001)                                        # Adjust for point below the segment
    plus <-  c(0, +0.001)                                        # Adjust for point above the segment
    flow_shift <- c(+100, 0)                                     # Adjust for current indicator
    flow_plot <- geom_segment(aes(xend= min(flow[,"X"]), y = min(flow[,"Y"]), # PLotting line for current indicator 
                                  x = max(flow[,"X"]), yend = max(flow[,"Y"])), arrow = arrow())
  }            # Change the shift used for test point relative midpoint
  if(Checked[segment,]$current == "Zonal")      {
    minus <- c(-0.001, 0)
    plus <-  c(+0.001, 0)
    flow_shift <- c(0, +100)
    flow_plot <- geom_segment(aes(x = min(flow[,"X"]), y = min(flow[,"Y"]), 
                                  xend = max(flow[,"X"]), yend = max(flow[,"Y"])), arrow = arrow()) 
  }            # Based on current of interest
  
  coords <- midpoint %>% st_transform(4326)                      # Transform to lat-lon to ensure test points are perpendicular to the line
  
  mid_minus <- coords + minus                                    # Shift from the mid point
  st_crs(mid_minus) <- 4326                                      # set crs
  mid_minus <- st_transform(mid_minus, crs) %>%                 # change crs for plotting
    st_cast("POINT")
  
  mid_plus <- coords + plus                                      # Shift from the mid point
  st_crs(mid_plus) <- 4326                                       # set crs
  mid_plus <- st_transform(mid_plus, crs) %>%                   # change crs for plotting
    st_cast("POINT")
  
  flow <- st_union(x = mid_plus, y = mid_minus) %>%              # Link and shift points to make an arrow to illustrate flow
    st_cast("LINESTRING") %>% 
    + flow_shift
  st_crs(flow) <- crs 
  flow <- st_coordinates(flow)
  
  test <- st_sf(geometry = c(mid_plus, mid_minus), side = c("plus", "minus")) %>% # Create a full SF object to allow point in polygon analysis
    st_join(domain, join = st_intersects) %>%                    # Are the points either side of the boundary in a domain polygon?
    mutate(Contained = if_else(is.na(Shore), "out", "in")) %>%   # Label the points 
    mutate(Flip = if_else(side == "plus" & Contained == "out" |  # Determine whether positive currents flow in or out of the domain and so if they need to be flipped
                            side == "minus" & Contained == "in", TRUE, FALSE))
  
  neighbour <- filter(test, Contained == "out") %>%              # Grab the SF which is outside the focal polygon
    st_intersects(domains) %>%                                   # Which polygon DOES this point sit in? (a list of 1 number)
    as.numeric() %>%                                             # Drop unneccessary formatting
    domains$Shore[.]                                             # Pull the shore label for the polygon
  
  # window <- st_bbox(Checked[segment,])                           # Get a zoom which fits the segment of interest
  
  # ggplot() + 
  #   geom_sf(data = domain, fill = "yellow") +
  #   geom_sf(data = Checked[segment,]) + 
  #   geom_sf(data = midpoint, colour = "red") + 
  #   geom_sf(data = test, aes(colour = Contained), show.legend = "point") + 
  #   flow_plot +        # The arrow still doesn't plot perpendicularly for all locations on the projection, but the tests are working fine
  #   theme_minimal() +
  #   labs(x = NULL, y = NULL, subtitle = paste0("Should currents be flipped (for + numbers into polygon)? = ", test$Flip[1])) +
  #   coord_sf(xlim = c(window$xmin, window$xmax), ylim = c(window$ymin, window$ymax))
  
  summary <- filter(test, Contained == "in") %>%                 # Take the metadata associated with the point in the focal polygon
    dplyr::select(Shore, Flip) %>%                               # Keep only interesting columns
    st_set_geometry(NULL) %>%                                    # Drop unneccessary geometry
    mutate(#Outside = filter(test, Contained == "in")$Shore,
           Neighbour = as.character(neighbour)) %>%              # Attach the neighbouring polygon
    replace_na(list(Neighbour = "Ocean"))                        # If there wasn't a neighbouring polygon, assign ocean
    
  return(summary)
}                    # Test whether positive currents flow in or out of the model domain at a segment

#### Sampling Flows transects ####

extract <- function (var, Depth, Data) {

#  Depth = "D" ; var = "Zonal"
#  Depth = "S" ; var = "Meridional"
  
  Data <- Data[[Depth]]
  
  if(Depth == "S") Transects <- Shallow_transects       # Select the relevant transects for sampling
  if(Depth == "D") Transects <- Deep_transects          # Select the relevant transects for sampling

  if(Depth == "D" & var == "Zonal") Intersections <- Intersections_DZ          # Select the relevant transects for sampling
  if(Depth == "D" & var == "Meridional") Intersections <- Intersections_DM          # Select the relevant transects for sampling
  if(Depth == "S" & var == "Zonal") Intersections <- Intersections_SZ          # Select the relevant transects for sampling
  if(Depth == "S" & var == "Meridional") Intersections <- Intersections_SM          # Select the relevant transects for sampling
  
  Samples <- map(Intersections, function(x) mean(Data[x,var], na.rm = T)) %>% # Select the cells and calulate the average current per transect
    unlist() %>%                                                             # Collapse list to vector
    mutate(Transects[[var]], Sample = .) %>%                                 # Copy transect meta-data over to the samples
    drop_na(Sample) %>%                                                      # NM points on land return NA, remove these
    mutate(Sample = ifelse(Flip == TRUE, -1* Sample, Sample),                # Flip current direction if required
           Depth = Depth)

  return(Samples)
}                    # Pull the mean value of raster cells touched by transects

Sample <- function(file)               {
  
  #file <- "./Objects/Months/NM.1.1980.rds"
  #file <- "./Objects/Months/NM.1.1981.rds"
  
  Data <- readRDS(file) %>%                                                  # Read in a current file
    split(.$Depth) %>%                                                       # Seperate shallow and deep data
    map(.x = ., .f = left_join, x = cells) %>%                               # Reorder data onto the grid
    map(st_drop_geometry)                                                    # Drop geometry column for efficient extraction
    
    Summary <- map2(rep(c("Meridional", "Zonal"), each = 2), 
                  c("S", "D", "S", "D"), extract, Data = Data) %>%           # Extract for the combinations of depth and current
    rbindlist() %>%                                                          # Bind
    mutate(Flow = Sample * Weights,                                          # Weight the velocities by transect area
           Direction = ifelse(Sample > 0, "In", "Out")) %>%                  # Create tag for flows in and out of the box
    group_by(Shore, Depth, Direction, Neighbour) %>%                         # Data to retain when summing
    summarise(Flow = sum(Flow)) %>%                                          # Sum flows
    ungroup() %>%                                                            # Ungroup for speed
    mutate(Year = Data[["S"]]$Year[1], Month = Data[["S"]]$Month[1])         # Add date from original file

  return(Summary)
}                    # Perform flow extractions and summarise by box exchanges

#### Sampling Boundaries transects ####

extract_OOB <- function (Depth, Data, variables) {
  
  #  Depth = "D" 
  #  Depth = "S"
  
  Data <- Data[[Depth]]
  #variables <- c("DIN", "Chlorophyll", "Temperature", "Salinity")
  
  if(Depth == "S") { Transects <- Shallow_transects ; Intersections <- Intersections_S }       # Select the relevant transects for sampling
  if(Depth == "D") { Transects <- Deep_transects ; Intersections <- Intersections_D }          # Select the relevant transects for sampling
  
  Samples <- map(Intersections, function(x) colMeans(Data[x, variables], na.rm = T)) %>% # Select the cells and calulate the average current per transect
    rbind_list() %>%                                                             # Collapse list to vector
    bind_cols(Transects, .) %>%                                              # Copy transect meta-data over to the samples
    drop_na() %>%                                                            # NM points on land return NA, remove these
    mutate(Depth = Depth)
  
  return(Samples)
}                    # Pull the mean value of raster cells touched by transects

Sample_OOB <- function(file)               {
  
  #file <- "./Objects/Months/NM.1.1980.rds"
  #file <- "./Objects/Months/NM.1.1981.rds"
  variables <- c("DIN", "Chlorophyll", "Temperature", "Salinity")
  
  Data <- readRDS(file) %>%                                                  # Read in a current file
    split(.$Depth) %>%                                                       # Seperate shallow and deep data
    map(.x = ., .f = left_join, x = cells) %>%                               # Reorder data onto the grid
    map(st_drop_geometry)                                                    # Drop geometry column for efficient extraction
  
  Summary <- map(c("S", "D"), extract_OOB, Data, variables) %>%                  # Extract for the combinations of depth and current
    rbindlist() %>%                                                          # Bind
    group_by(Shore, Depth) %>%                                               # Data to retain when summing
    select(variables) %>% 
    summarise_all(mean) %>% 
    ungroup() %>%                                                            # Ungroup for speed
    mutate(Year = Data[["S"]]$Year[1], 
           Month = Data[["S"]]$Month[1],
           Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"),
           Compartment = paste(Shore, Depth)) %>% 
    pivot_longer(c(DIN, Chlorophyll, Temperature, Salinity), names_to = "Variable", values_to= "Measured") 
    
  return(Summary)
}                    # Perform flow extractions and summarise by box exchanges
