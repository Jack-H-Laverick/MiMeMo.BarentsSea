
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

Space$shallow <- between(Space$nc_depth[1:38], 0, 60)                        # Find the positions in the depth vector for shallow slice
Space$deep <- Space$nc_depth[1:38] > 60                                      # Find the elements in the array to extract for deep slice
Space$shallow_W <- between(DepthsW[1:39], 0, 60)                             # And for W files
Space$deep_W <- DepthsW[1:39] > 60                            

Space$s.weights <- get_weights(0, 60)                                                    # Work out water column proportions for weighted means                                                      
Space$d.weights <- get_weights(60, 400)
Space$s.weights_W <- get_weights.W(0, 60)                                                 # And for W files                                                      
Space$d.weights_W <- get_weights.W(60, 400)

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

output <- st_drop_geometry(output)

#### Build the monthly sumaries ####

#tic("Creating monthly data objects from netcdf files")                      # Time the data extraction

#  profvis({
    
overnight <- all_files %>%  
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(whole_month, crop = Window, # Progress bar costs X amount of time
             grid = output, space = Space)                                  # Perform the extraction and save an object for each month (in parallel)
#  })
#  toc()                                                                       # Stop timing 


#### Looking for improvements ####

# library(profvis)
# library(microbenchmark)
# 
# test <- all_files %>%  
#   split(., f = list(.$Month, .$Year, .$Type)) %>% 
#   .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
#   .[[1]] # Get a DF of file names for each time step to summarise to
#
#test_big <- all_files %>%  
#  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
#  .[1:144]
#
# profvis({
#   
#   look <- type_in_month(test, grid = output, space = Space)
#   look_big <-future_map(test_big, whole_month, crop = Window, 
#                    grid = output, space = Space)                    # Perform the extraction and save an object for each month (in parallel)
#* next target is reduce
# })
# 
# test_bind <- matrix(1:336270, nrow = 30570, ncol = 11) %>%  as.data.frame
# 
# microbenchmark(rbind(test_bind, test_bind),
#                bind_rows(test_bind, test_bind),
#                rbindlist(list(test_bind, test_bind)))
