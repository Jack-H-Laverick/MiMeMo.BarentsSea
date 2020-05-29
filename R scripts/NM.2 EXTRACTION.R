
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Months.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr", "ncdf4") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe
  separate(".", into = c("Path", "File"), sep = 61) %>%                   # Extract the year and month from the file name
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

test <- Space$nc_lat > 65 & Space$nc_lat < 84 & Space$nc_lon > 6 & Space$nc_lon < 68 # Rough crop
E <- min(which(test == TRUE, arr.ind = TRUE)[,"row"])
W <- max(which(test == TRUE, arr.ind = TRUE)[,"row"])
S <- min(which(test == TRUE, arr.ind = TRUE)[,"col"])
N <- max(which(test == TRUE, arr.ind = TRUE)[,"col"])
Limits <- data.frame("x_start" = E, "x_count" = W - E + 1, "y_start" = S, "y_count" = N - S + 1)

Space$shallow <- between(Space$nc_depth[1:38], 0, 60)                       # Find the positions in the depth vector for shallow slice
Space$deep <- Space$nc_depth[1:38] > 60                                     # Find the elements in the array to extract for deep slice
Space$shallow_W <- between(DepthsW[1:39], 0, 60)                            # And for W files
Space$deep_W <- DepthsW[1:39] > 60                            

output <- readRDS("./Objects/Fixed_grid.rds")                               # Load in Bathymetry pulled from GEBCO
bathymetry <- matrix(abs(output$Bathymetry), Limits$x_count, Limits$y_count)# Create a bathymetry matrix

Space$s.weights <- get_weights(0, 60, bathymetry)                           # Work out water column proportions for weighted means                                                      
Space$d.weights <- get_weights(60, 400, bathymetry)
Space$s.weights_W <- get_weights.W(0, 60, bathymetry)                       # And for W files                                                      
Space$d.weights_W <- get_weights.W(60, 400, bathymetry)

# Space$start3D <- c(1,1,1,1) ; Space$count3D <- c(-1,-1,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
# Space$start3DW <- c(1,1,1,1) ; Space$count3DW <- c(-1,-1,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)
Space$start3D <- c(Limits$x_start,Limits$y_start,1,1) ; Space$count3D <- c(Limits$x_count, Limits$y_count,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
Space$start3DW <- c(Limits$x_start,Limits$y_start,1,1) ; Space$count3DW <- c(Limits$x_count, Limits$y_count,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)

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

output <- st_drop_geometry(output) %>% 
  bind_rows(., .) %>% 
  mutate(Depth = as.factor(rep(c("S", "D"), each = (nrow(.)/2))))                  # Recode depths from numeric to character

#### Build the monthly sumaries ####

tic("Creating monthly data objects from netcdf files")                      # Time the data extraction

#  profvis({

overnight <- all_files %>%
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(whole_month, crop = Window, 
             grid = output, space = Space, .progress = T)                   # Perform the extraction and save an object for each month (in parallel)
#  })
  toc()                                                                       # Stop timing
 
#### Looking for improvements ####

#  library(profvis)
#  library(microbenchmark)
#  
# test <- all_files %>%
#   split(., f = list(.$Month, .$Year, .$Type)) %>%
#   .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
#   .[[1]] # Get a DF of file names for each time step to summarise to
# 
# test_big <- all_files %>%  
#   split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
#   .[1:144]


#simple <- get_sea(path = test$Path[1], file = test$File[1],space = Space)
##**## target apply next
  
#tic() 
#  profvis({
    
#   look2 <- type_in_month(test, space = Space)
#    month <- whole_month(test_big[[1]], crop = Window, 
#                          grid = output, space = Space)                    # Perform the extraction and save an object for each month (in parallel)
    
# look_big <-future_map(test_big, whole_month, crop = Window, 
#                      grid = output, space = Space, .progress = TRUE)                    # Perform the extraction and save an object for each month (in parallel)
#  })
#toc() 
 
#month <- cbind(zonal = c(1:3, 4:6), merid = c(1:3, 4:6))  
 
#  Month <- test_big[[1]][1,5] ; Year <- test_big[[1]][1,4]                                    # Pull date
#  
#  Month <- split(test_big[[1]], f = list(test_big[[1]]$Type)) %>%                             # Split out the files for this month by type, so they can be averaged together
#    purrr::map(type_in_month, Space) %>%                                      # Pull a whole month of data from a single file type
#     #  purrr::reduce(dplyr::full_join) %>%                                    # Join together all the data packets
#    purrr::reduce(cbind) %>%                                                # Join together all the data packets
#    .[, !duplicated(colnames(.))] %>% 
#    cbind(rbind(output,output)) %>%
#    mutate(Depth = as.factor(ifelse(Depth ==1, "S", "D"))) %>%              # Recode depths from numeric to character
#    # dplyr::right_join(spine) %>%   # a)                                   # Cut out rows outside of polygons and attach compartment labels
#    dplyr::right_join(Window)
# check <- readRDS("./Objects/Months/NM.12.1984.rds")
 
# ggplot(check) + geom_point(aes(x = Longitude, y = Latitude, colour = Temperature), size = 0.1)
#  
 # simple <- cbind(temp = 1:10, 50)
 #  list <- list(cbind(a = 1:44650, b = 1:44650, c = 1:44650),
 #               cbind(d = 2:44651, e = 2:44651, f = 2:44651),
 #               cbind(d = 3:44652, e = 3:44652, f = 3:44652),
 #               cbind(d = 4:44653, e = 4:44653, f = 4:44653),
 #               cbind(g = 5:44654, h = 5:44654, i = 5:44654))
 # 
 #  
 # microbenchmark(reduce(list, cbind),
 #                do.call(cbind, list),
 #                do.call(cbind, lapply(as.data.frame,list)),
 #                cbind(list[[1]], list[[2]], list[[3]]),
 #                matrix(unlist(list), nrow = nrow(list[[1]])))
  
# list[[1:2]]
# 
# test_bind <- matrix(1:336270, nrow = 30570, ncol = 11) %>%  as.data.frame
# 
# microbenchmark(rbind(test_bind, test_bind),
#                bind_rows(test_bind, test_bind),
#                rbindlist(list(test_bind, test_bind)))

#   empty <- function(x) all(is.na(x))  
#   empty_lazy <- function(x) !any(!is.na(x))  
#   library(Rcpp)
#   
#   cppFunction('bool emptyC(NumericVector x) {
# return is_true(all(is_na(x)));
# }')
#   
# ignore <- 1:75  
# grab <- rep(NA, 75)
# improve <- ignore ; improve[37] <- NA   
# 
# empty(ignore)
# empty(grab)
# empty(improve)
# 
# emptyC(ignore)
# emptyC(grab)
# emptyC(improve)
# 
# empty_lazy(ignore)
# empty_lazy(grab)
# empty_lazy(improve)
# 
# empty_lazyC(ignore)
# empty_lazyC(grab)
# empty_lazyC(improve)
# 
# anyNA(ignore)
# anyNA(grab)
# anyNA(improve)
# 
# test <- improve
# 
#  microbenchmark(empty(test),
#                 empty_lazy(test),
#                 emptyC(test),
#                 anyNA(test))