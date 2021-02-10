
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "ncdf4")                             # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe
  separate(".", into = c("Path", "File"), sep = 61) %>%                     # Extract the year and month from the file name
  separate(File, into = c("Type", "Date"), 
           remove = FALSE, sep = -11) %>%                                   # Extract the year and month from the file name
  mutate(Date = str_sub(Date, end = -4))                                    # Drop file extension to get number

all_files[which(all_files$Date == "_2047105"),]$Type <- "grid_T_"           # One file has been incorrectly labelled, I've corrected the name. Now all timesteps have 6 files
all_files[which(all_files$Date == "_2047105"),]$Date <- "20471005"          

all_files  <- all_files %>%
  separate(Date, into = c("Year", "Month", NA),sep = c(4, 6)) %>%           # Extract the year and month from the file name 
  mutate(Year = as.integer(Year),                                           # Set time as integers 
         Month = as.integer(Month)) %>%
  filter(Type != "grid_W_")                                                 # Drop the vertical water movement files
  
domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop unneeded data which would get included in new NM files

crop <- readRDS("./Objects/Domains.rds") %>%                                # Load SF polygons of the MiMeMo model domains
  st_buffer(dist = 50000) %>%                                               # It needs to be a bit bigger for sampling flows at the domain boundary
  summarise() %>%                                                           # Combine polygons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("./Objects/Fixed_grid2.rds") %>%                      # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#### Build summary scheme ####

scheme <- scheme_strathE2E(get_spatial(paste0(all_files$Path[1], all_files$File[1]), grid_W = F),
                           Bathymetry, 60, 400, crop) %>% 
  select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   # Get a scheme to summarise for StrathE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry()                                                        # Drop sf formatting

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import
scheme <- scheme_reframe(scheme) 

ice_scheme <- filter(scheme, layer == 1) %>%                                # Ice data is stored as a matrix, so needs a new scheme
  arrange(group) %>% 
  transmute(n = xyindex_to_nindex(x, y, count[1]))

scheme_result <- arrange(scheme, group) %>%                                 # Create a meta-data object to attach to the summaries
  select(x, y, slab_layer, longitude, latitude, Shore, Bathymetry) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= 60 ~ 60,     # Weights for zonal averages by thickness of water column
                             slab_layer == "S" & Bathymetry < 60 ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= 400 ~ 340,
                             slab_layer == "D" & Bathymetry < 400 ~ (Bathymetry - 60)))

#### extract ####

tictoc::tic()
all_files %>%
  split(., f = list(.$Month, .$Year)) %>%                                   # Specify the timestep to average files over.
  future_map(NEMO_MEDUSA, analysis = "slabR", summary = scheme_result,
             scheme = scheme, ice_scheme = ice_scheme$n, start = start,  
             count = count, out_dir = "./Objects/Months", .progress = T)    # Perform the extraction and save an object for each month (in parallel)
tictoc::toc()
