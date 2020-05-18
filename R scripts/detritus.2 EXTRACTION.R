
# Pull the contents of old NM files: Remember to mount the fish folder by navigating from the file browser to:
# saveRDS("./Objects/Detritus.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr", "ncdf4", "pbapply", "radiant.data") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("../../../../import/fish/southampton/combined/timestep/", pattern = "P.nc", full.names = TRUE) %>%                                                      # List all file names in a folder and below
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  separate('.', into = c(NA, "Metadata"), 
           remove = FALSE, sep = "//") %>%                                  
  separate(Metadata, into = c("Year", "Month", "Day", NA), 
           sep = c(4, 6, 8)) %>%                                            # Extract the year and month from the file name
  mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) %>% # Set time as integers
  filter(Year <2006) %>% 
  rename(File = ".") %>% 
  mutate(File = as.character(File))

#### Set the spatial environment ####

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop uneeded data which would get included in new NM files

Space <- get_spatial(all_files$File[1])                                    # Pull space from the first file

output <- readRDS("./Objects/grid_detritus.rds")                            # Load in Bathymetry pulled from GEBCO
mask_bathy <- matrix(abs(output$Bathymetry), 490, 445)                      # Create a bathymetry matrix

Shallow_mark <- between(Space$nc_depth[1:27], 0, 60)                        # Find the positions in the depth vector for shallow slice
Deep_mark <- Space$nc_depth[1:27] > 60                                      # Find the elements in the array to extract for deep slice

sw <- get_weights.old(0, 60)                                                # Work out water column proportions for weighted means                                                      
dw <- get_weights.old(60, 400)

start3D <- c(1,1,1,1) ; count3D <- c(-1,-1,27,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m

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
                          ifelse(Depth == "D", weights - 60, weights))) %>% # If it's a deep pixel reduce thickness by 60 m, otherwise keep thickness as is 
as.data.frame() #tibbles are slow

output <- st_drop_geometry(output) %>% as.data.frame()                      # tibbles slow down the script

#### Build the monthly sumaries ####

tic("Creating monthly data objects from netcdf files")                      # Time the data extraction

overnight <- all_files %>%  
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(detritus_month, .progress = T, targets = Window, start = start3D, 
             count = count3D, grid = output, shallow = Shallow_mark, 
             s.weights = sw, deep = Deep_mark, d.weights = dw)              # Perform the extraction and save an object for each month (in parallel)
toc()                                                                       # Stop timing 
              