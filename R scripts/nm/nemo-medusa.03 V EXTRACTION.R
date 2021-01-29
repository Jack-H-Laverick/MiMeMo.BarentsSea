
# Interpolate the exchange at the vertical boundary from NEMO-MEDUSA model output: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
library(furrr)                                                              # List packages
plan(multisession)

domain <- readRDS("./Objects/Domains.rds") %>%                              # Get the horizontal area to extract over 
  select(Shore) %>% 
  filter(Shore =="Offshore")

target_depth <- 60                                                          # Set the depth to interpolate to

example <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa/", # File to pull dimensions from 
                      recursive = T, full.names = TRUE, pattern = "grid_W")

#### Create summary scheme to interpolate a depth layer over a given area #####

Bathymetry <- readRDS("./Objects/Fixed_grid2.rds") %>%                      # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

scheme <- scheme_interp_slice(get_spatial(example, grid_W = T), target_depth, domain) # Get a scheme for linear interpolation between 2 depth layers

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import

scheme <- scheme_reframe(scheme) %>%                                        # Adjust scheme indices so they match the array subset
  left_join(Bathymetry) %>%                                                 # Attach bathymetry to summary scheme
  filter(depth < Bathymetry & target_depth < Bathymetry) %>%                # Drop points where the target depth or next deeper layer are below the sea floor
  group_by(y, x) %>%                                                        # Redefine the group column as removing points can disrupt this
  mutate(group = cur_group_id()) %>%                                        # Create a new grouping column for the summary scheme
  ungroup()

summary <- filter(scheme, layer == 1) %>%                                   # Create the metadata to attach to summaries
  arrange(group) %>%                                                        # Summaries will be returned in group order, so make sure these match
  mutate(depth = target_depth) %>%                                          # Lets return the depth we interpolated to
  select(x, y, longitude, latitude, depth)                                  # As well as horizontal information

#### Extract ####

W_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", 
                        recursive = TRUE, full.names = TRUE, pattern = "grid_W") %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe
  separate(".", into = c("Path", "File"), sep = 61) %>%                     # Extract the year and month from the file name
  separate(File, into = c("Type", "Date"), remove = FALSE, sep = -11) %>%   # Extract the year and month from the file name
  separate(Date, into = c("Year", "Month", NA),sep = c(4, 6)) %>%           # Extract the year and month from the file name 
  mutate(Year = as.integer(Year),                                           # Set time as integers 
         Month = as.integer(Month)) %>% 
  split(., f = list(.$Month, .$Year))                                       # Get a DF of file names for each time step to summarise to

tic()
future_map(W_files, NEMO_MEDUSA, analysis = "slabR",                        # Interpolate grid_W files in paralell
           out_dir = "./Objects/vertical boundary", scheme_w = scheme,
           start_w = start, count_w = count, summary = summary, .progress = T)
toc()
