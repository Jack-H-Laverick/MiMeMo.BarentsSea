
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Months.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "data.table", "furrr", "ncdf4")               # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe
  separate(".", into = c("Path", "File"), sep = 61) %>%                     # Extract the year and month from the file name
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

# Speed things up with a tighter crop
test <- Space$nc_lat > 65 & Space$nc_lat < 84 & Space$nc_lon > 6 & Space$nc_lon < 68
E <- min(which(test == TRUE, arr.ind = TRUE)[,"row"])
W <- max(which(test == TRUE, arr.ind = TRUE)[,"row"])
S <- min(which(test == TRUE, arr.ind = TRUE)[,"col"])
N <- max(which(test == TRUE, arr.ind = TRUE)[,"col"])
Limits <- data.frame("x_start" = E, "x_count" = W - E + 1, "y_start" = S, "y_count" = N - S + 1)

Space$shallow <- between(Space$nc_depth[1:38], 0, 60)                       # Find the positions in the depth vector for shallow slice
Space$deep <- Space$nc_depth[1:38] > 60                                     # Find the elements in the array to extract for deep slice
Space$shallow_W <- between(DepthsW[1:39], 0, 60)                            # And for W files
Space$deep_W <- DepthsW[1:39] > 60                            

output <- readRDS("./Objects/Fixed_grid2.rds")                              # Load in Bathymetry
bathymetry <- matrix(abs(output$Bathymetry), Limits$x_count, Limits$y_count)

Space$start3D <- c(Limits$x_start,Limits$y_start,1,1) ; Space$count3D <- c(Limits$x_count, Limits$y_count,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
Space$start3DW <- c(Limits$x_start,Limits$y_start,1,1) ; Space$count3DW <- c(Limits$x_count, Limits$y_count,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)

Space$s.weights <- get_weights(0, 60, bathymetry) #Add depths as function argument    # Work out water column proportions for weighted means                                                      
Space$d.weights <- get_weights(60, 400, bathymetry)
Space$s.weights_W <- get_weights.W(0, 60, bathymetry)                       # And for W files                                                      
Space$d.weights_W <- get_weights.W(60, 400, bathymetry)

Window <- st_join(output, domains) %>% 
  sfc_as_cols(names = c("x_crs", "y_crs")) %>% 
  st_drop_geometry() %>% 
  filter(between(x_crs, lims[["xmin"]], lims[["xmax"]]) &                   # Clip to plotting window
         between(y_crs, lims[["ymin"]], lims[["ymax"]])) %>%                
  select(-c(x_crs, y_crs)) %>%                                              # Drop columns for cropping to plot window
  mutate(Depth = "S") %>%                                                   # Add a depth column
  bind_rows(., mutate(., Depth = "D")) %>%                                  # Duplicate entries for second depth layer
  anti_join(filter(., Depth == "D" & Shore == "Inshore")) %>%               # Remove Inshore deep which doesn't exist                 
  mutate(weights = abs(Bathymetry)) %>%                                     # Get weights for pixel when calculating compartment averages
  mutate(weights = ifelse(Depth == "S" & weights > 60, 60,                  # Set shallow pixel thickness to 60 m deep, even if there is deep inshore pixels close to shore 
                    ifelse(Depth == "D", weights - 60, weights)))           # If it's a deep pixel reduce thickness by 60 m, otherwise keep thickness as is 

output <- st_drop_geometry(output) %>% 
  bind_rows(., .) %>% 
  mutate(Depth = as.factor(rep(c("S", "D"), each = (nrow(.)/2))))           # Recode depths from numeric to character

#### Build the monthly sumaries ####

tic("Creating monthly data objects from netcdf files")                     # Time the data extraction
 
 overnight <- all_files %>%
   split(., f = list(.$Month, .$Year)) %>%                                  # Get a DF of file names for each time step to summarise to
   .[sapply(., function(x) dim(x)[1]) > 0] %>%                              # Drop empty dataframes (Months which weren't observed but split introduces)
   future_map(whole_month, crop = Window, analysis = "StrathE2E",
              grid = output, space = Space, out_dir = "./Objects/Months",
              .progress = T)                                                # Perform the extraction and save an object for each month (in parallel)
 toc()                                                                      # Stop timing
# 2.15 hours