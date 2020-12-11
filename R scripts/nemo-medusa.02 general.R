
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Months.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr", "ncdf4") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("Path", "File"), sep = 61) %>%                   # Extract the year and month from the file name
  separate(File, into = c("Type", "Date"), 
           remove = FALSE, sep = -11) %>%                                   # Extract the year and month from the file name
  mutate(Date = str_sub(Date, end = -4))                                    # Drop file extension to get number

all_files[which(all_files$Date == "_2047105"),]$Type <- "grid_T_"           # One file has been incorrectly labelled, I've corrected the name. Now all timesteps have 6 files
all_files[which(all_files$Date == "_2047105"),]$Date <- "20471005"          

all_files  <- all_files %>%
  separate(Date, into = c("Year", "Month", "Day"),sep = c(4, 6)) %>%        # Extract the year and month from the file name 
  mutate(Year = as.integer(Year),                                           # Set time as integers 
         Month = as.integer(Month), 
         Day = as.integer(Day))  

examples <- group_by(all_files, Type) %>% slice(1)                          # Get one example for each file type
trial <- group_by(all_files, Type) %>% slice(1:3)                           # Get one example for each file type

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop uneeded data which would get included in new NM files

#### Set the spatial environment ####

Space <- get_spatial(paste0(examples[1,"Path"], examples[1,"File"]))        # Pull space from the first file

W <- nc_open(paste0(examples[4,"Path"], examples[4,"File"]))                # Get the different depth vector for W files
DepthsW <- W$dim$depthw$vals ; nc_close(W) ; rm(W) 

output <- readRDS("./Objects/Fixed_grid.rds")                               # Load in Bathymetry pulled from GEBCO
mask_bathy <- matrix(abs(output$Bathymetry), 235, 190)                      # Create a bathymetry matrix

#Space$shallow <- between(Space$nc_depth[1:38], 0, 60)                        # Find the positions in the depth vector for shallow slice
#Space$deep <- Space$nc_depth[1:38] > 60                                      # Find the elements in the array to extract for deep slice
#Space$shallow_W <- between(DepthsW[1:39], 0, 60)                             # And for W files
#Space$deep_W <- DepthsW[1:39] > 60                            

#Space$s.weights <- get_weights(0, 60)                                                    # Work out water column proportions for weighted means                                                      
#Space$d.weights <- get_weights(60, 400)
#Space$s.weights_W <- get_weights.W(0, 60)                                                 # And for W files                                                      
#Space$d.weights_W <- get_weights.W(60, 400)

Space$start3D <- c(1,1,1,1) ; Space$count3D <- c(-1,-1,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
Space$start3DW <- c(1,1,1,1) ; Space$count3DW <- c(-1,-1,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)

#spine <- st_join(output, domains) %>%                                       # Clip a set of lat-lons to the model domains. It will be faster to left join by these than to perform the spatial clip for each of the files
#  drop_na(Shore) %>%                                                        # Drop points outside of the polygons
#  filter(Shore_dist > 0)                                                    # Remove points on land

#spine <- bind_rows(mutate(spine, Depth = "S"), mutate(spine, Depth = "D")) %>%  # double up the dataframe with a Depth column
#  Compartmentalise()                                                        # Reperform filtering for compartments, as the cliiping polygons are conservative, and calculate weights for averaging

Window <- st_join(output, domains) %>% 
  sfc_as_cols() %>% 
  st_drop_geometry() %>% 
  filter(between(x = x, lims[["xmin"]], lims[["xmax"]]) &                   # Clip to plotting window
         between(x = y, lims[["ymin"]], lims[["ymax"]])) %>%                
  select(-c(x,y)) %>%                                                       # Drop columns for cropping to plot window
  mutate(Depth = "S") %>%                                                   # Add a depth column
  bind_rows(., mutate(., Depth = "D")) %>%                                  # Duplicate entries for second depth layer
  anti_join(filter(., Depth == "D" & Shore == "Inshore")) %>%               # Remove Inshore deep which doesn't exist                 
  mutate(weights = abs(Bathymetry)) %>%                                     # Get weights for pixel when calculating compartment averages
  mutate(weights = ifelse(Depth == "S" & weights > 60, 60,                  # Set shallow pixel thickness to 60 m deep, even if there is deep inshore pixels close to shore 
                    ifelse(Depth == "D", weights - 60, weights)))           # If it's a deep pixel reduce thickness by 60 m, otherwise keep thickness as is 

#### Clever weights function ####

top <- 0
mid <- 60
bottom <- 400

get_weights.tidy <- function(top, bottom) {
  #top <- 200                                                                    # shallowest depth of the slice
  #bottom <- 1000                                                                # What's the bottom of the slice? for incorporating into a function

  layers <- sum(between(Space$nc_depth, top, bottom))+1                          # The number of depth slices between the minimum and the maximum depth of a layer + the next
  shallowest_layer <- min((which(between(Space$nc_depth, top, bottom))))
  deepest_layer <- max((which(between(Space$nc_depth, top, bottom))))+1
  
  weights <- array(NA, c(nrow(Space$nc_lon),ncol(Space$nc_lon),                  # Initialise an array to hold weights
                         layers))           # The number of depth slices is how many are between the minimum and the maximum depth of a layer + the next
  first <- weights[,,1]
  first[] <- mean(Space$nc_depth[shallowest_layer:(shallowest_layer+1)]) - top %>% 
               rep(times = nrow(Space$nc_lon) * ncol(Space$nc_lon))            # Water above the first midpoint minus the depth at the top of the slice
  marks <- mask_bathy > mean(Space$nc_depth[shallowest_layer:(shallowest_layer+1)]) # Which cells contain model outputs deeper than the seafloor?
  first[!marks] <- mask_bathy[!marks] - top                                    # Replace these with the depth to sea floor
  weights[,,1] <- first
  
  weights[,,layers] <- mask_bathy - mean(Space$nc_depth[(deepest_layer - 1 ):deepest_layer]) # The remaining water column thickness is the sea floor - the deepest midpoint.
  
  for (i in 2:(layers-1)) {
    #i <- 23
    last_midpoint <- mean(Space$nc_depth[(shallowest_layer + i - 2):(shallowest_layer + i - 1)])  # Find the mid depth to the layer above
    next_midpoint <- mean(Space$nc_depth[(shallowest_layer + i - 1): (shallowest_layer + i)])     # Find the mid depth to the layer below
    
    if(top > last_midpoint) above <- top else above <- last_midpoint             # If the top of the slice is deeper than the previous midpoint, use the top of the slice
    if(bottom < next_midpoint) below <- bottom else below <- next_midpoint       # If the next midpoint is deeper than the bottom of the slice, use the bottom of the slice
    
    weights[,,i] <- below - above %>% rep(times = nrow(Space$nc_lon) * ncol(Space$nc_lon)) # Calculate layer thickness and repeat to fill the array
    
    marks <- mask_bathy > below                                                  # Is the seafloor deeper than the bottom of the layer?
    weights[,,i][!marks] <- mask_bathy[!marks] - above                           # If not, replace these with the depth to sea floor - the top of the water layer
    
  }                                                          # Roll through each matrix and calculate the water thickness using the next depth, bottom of the slice, or bathymetry, whichever is smaller
  no_weight <- weights[] <= 0; weights[no_weight] <- NA                        # Finally if a weight is <= 0 get NA
  
  return(weights)
}

## Need to calculate all the thicknesses and subset I think to avoid super deep weights at the bottom layer

thickness <- get_weights.tidy(top, mid) %>% 
           .[,,-(dim(.)[3]-1)] %>% 
           abind::abind(get_weights.tidy(mid, bottom), along = 3) %>% 
           as.data.table(value.name = "Thickness", key = c("V1", "V2", "V3")) %>% 
           .[,Depth := ifelse(V3 <= max((which(between(Space$nc_depth, top, mid)))), "S", "D")]

#### Build the monthly sumaries ####

# path <- all_files$Path[1]
# file <- all_files$File[1]
# vars <- c("vosaline", "votemper")
# grid <- output
# space <- Space

get_all_3D <- function(path, file, vars, start3D, count3D) {
  
  print(stringr::str_glue("{file} Extracting {vars}"))
  nc_raw <- ncdf4::nc_open(paste0(path, file))                                 # Open up a netcdf file to see it's raw contents (var names)
  
  data <- map(vars, ~{                                                         # For each variable targetted in this file
    nc_var <- ncdf4::ncvar_get(nc_raw, varid = .x, start3D, count3D) %>% # Pull the variable
      as.data.table(value.name = .x, key = c("V1", "V2", "V3"))}) %>%          # Convert the array to a data.table
      Reduce(function(...) merge(..., all.x = T), .)                             # Combine the variables
  
 ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
 #   dplyr::filter(Shore_dist > 0) %>%                                          # Remove points on land
  #  dplyr::mutate(Depth = as.factor(Depth))
  
  return(data)
}

look <- get_all_3D(all_files$Path[1], all_files$File[1], c("vosaline", "votemper"), c(1,1,1,1), c(-1,-1,38,-1))
key(look)

type_in_month_dt <- function(instructions) {
  
  Month.type <- instructions %>%                                          
    pmap(get_all_3D) %>% 
    rbindlist() %>%
    setkey() %>% 
    .[thickness]
  
  Month.type <- Month.type[,
     lapply(.SD, weighted.mean, w = Thickness, na.rm = T), 
     by = .(V1, V2, Depth)]#[,
#     V3 := NULL]
  return(Month.type)
}

trial <- overnight[[1]] %>% 
  filter(Type == "grid_T_") %>% 
  select(Path, File) %>%
  rename(path = Path, file = File) %>% 
  mutate(vars = list(vars),
         start3D = list(start3D),
         count3D = list(count3D))

look <- type_in_month_dt(trial)

#### Old ####

tic("Creating monthly data objects from netcdf files")                      # Time the data extraction

overnight <- all_files %>%  
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(whole_month, crop = Window, .progress = T, 
             grid = output, space = Space)                                  # Perform the extraction and save an object for each month (in parallel)
toc()                                                                       # Stop timing 
