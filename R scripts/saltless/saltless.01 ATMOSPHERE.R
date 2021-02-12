
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "tictoc", "furrr", "ncdf4", "stars")       # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("./Data/EMEP Atmosphere/", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c(NA, "Year"), 
           remove = FALSE, sep = "month.") %>%                              # Extract the year from the file name
  mutate(Year = str_sub(Year, end = 4)) %>%                                 # Drop file extension to get number
  rename(File = "value")

#### Functions ####

Window_emep <- function(file, w, e, s, n)           {
  
 # file <- all_files[1,]$File ; w = 0 ; e = 180 ; s = 0 ; n = 90
  
  raw <- nc_open(file)
  lon <- raw$dim$lon$vals %>% between(w, e)
  W <- min(which(lon == TRUE))
  E <- max(which(lon == TRUE))
  
  lat <- raw$dim$lat$vals %>% between(s, n)
  S <- min(which(lat == TRUE))
  N <- max(which(lat == TRUE))
  
  lons <- raw$dim$lon$vals[W:E]
  lats <- raw$dim$lat$vals[S:N]
  
  Limits <- data.frame("Lon_start" = W, "Lon_count" = E - W + 1, "Lat_start" = S, "Lat_count" = N - S + 1)
  
  Limits <- list(Lats = lats, Lons = lons, Limits = Limits)
  return(Limits)
}    # Extract the positions to clip the netcdf file to, and the values for the smaller grid

mync_get <- function(File, Variable)                {
  
  ncvar_get(nc_open(File), Variable, c(Space$Limits$Lon_start, Space$Limits$Lat_start, 1),  # Extract the variable of interest
            c(Space$Limits$Lon_count, Space$Limits$Lat_count, -1))                    # cropped to window, with all time steps
}    # Import a variable clipped to Window

get_air_deposition <- function(File, Year)          {
  
  #File <- all_files$File[1] ; Year <- all_files$Year[1]                     # test
  variables <- c("DDEP_OXN_m2Grid", "WDEP_OXN", "DDEP_RDN_m2Grid", "WDEP_RDN")

  Data <- map(variables, mync_get, File = File)                              # Extract the variables of interest

  Summary <- map(Data, as.data.frame.table, responseName = "Measured") %>%   # Reshape array as dataframe
    bind_rows(.id = "Variable") %>% 
    rename(Longitude = Var1, Latitude = Var2, Month = Var3) %>%              # Name the columns
    mutate(Variable = factor(Variable, labels = variables),                  # Recode variable names
           Longitude = rep(rep(rep(Space$Lons,                               # Replace the factor levels with dimension values
                                   times = length(Space$Lats)), times = 12), times = length(variables)),
           Latitude = rep(rep(rep(Space$Lats,  
                                  each = length(Space$Lons)), times = 12), times = length(variables)),
           Month = rep(rep(1:12, each = length(Space$Lats) * length(Space$Lons)), times = length(variables))) %>% 
    left_join(domains_mask) %>%                                              # Crop to domain
    drop_na() %>%         
    mutate(Year = Year,
           Oxidation_state = ifelse(grepl(pattern = "OXN", x = Variable, fixed = T), "O", "R"), 
           Deposition_state = ifelse(grepl(pattern = "DDEP", x = Variable, fixed = T), "Dry", "Wet")) %>% 
    group_by(Month, Year, Variable, Oxidation_state, Deposition_state, Shore) %>%        
    summarise(Measured = weighted.mean(Measured, weights)/14) %>%            # Average by time step weighted by area. Convert to millimols of N 
    ungroup()  
  
  return(Summary)
}    # Pull nitrogen deposition monthly time series per zone in a year file

#### Spatial ####

Space <- Window_emep(all_files[1,]$File, w = 0, e = 76, s = 65, n = 84)     # Get values to crop a netcdf file spatially at import. 

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

areas <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>%     # Get the data grid
  mutate(Dummy = 10) %>%                                                    # Add dummy data to convert to a stars grid
  st_as_stars()
st_crs(areas) <- st_crs(4326)                                               # set lat-lon crs

areas <- st_as_sf(areas, as_points = F, merge = F) %>%                      # Convert the stars grid into SF polygons 
  st_join(domains) %>%                                                      # Link to model domain 
  drop_na() %>%                                                             # Crop
  select(-c(Elevation, area, Dummy)) %>%                                    # Drop uneeded information
  mutate(weights = as.numeric(st_area(.)))                                  # Calculate the area of each cell for weighted averages

domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>% # Get the data grid
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>% # Convert to SF
  st_join(areas) %>%                                                        # Add the areas for weighting
  drop_na() %>%                                                             # Drop points which we didn't calculate weights for
  st_drop_geometry()                                                        # Drop SF formatting

#### Extract nitrogen depositons ####

Month_lengths <- data.frame(Month = seq(1,12,1), Days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
leap <- function(Year) ((Year %% 4 ==0) & (Year %% 100 != 0)) | (Year %% 400 == 0)  # Function to identify leap years

tic()
Deposition <- future_pmap_dfr(all_files, get_air_deposition, .progress = T) %>%    # Data extraction with parameters stored rowise in dataframe, executed in par
  left_join(Month_lengths) %>%                                                     # Add in days per month
  mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"), # Get date column for plotting
         Leap_year = leap(as.numeric(Year)),                                       # Which years are leap years?
         Days = ifelse(Leap_year == TRUE & Month == 2, 29, Days)) %>%              # Correct length of February in a leap year
  mutate(Measured = Measured / Days)   %>%                                         # Scale to average daily deposition  
  select(-c(Days, Leap_year))
  saveRDS(Deposition, "./Objects/Atmospheric N deposition.rds")
toc()

#### Plot ####

Deposition <- mutate(Deposition, Oxidation_state = factor(Oxidation_state, levels = c("O", "R"),
 labels = c(expression("Oxidised Nitrogen (NO"["y"]*")"), expression("Reduced Nitrogen (NH"["x"]*")"))))

ggplot(data = Deposition) + 
  geom_line(aes(x = Date, y = Measured, colour = Shore), size = 0.25) +
  theme_minimal() +
  facet_grid(cols = vars(Oxidation_state), rows = vars(Deposition_state), scales = "free_y", labeller = label_parsed) +
  labs(y = expression("mmols N m"^{-2}*"Day"^{-1}), caption = "EMEP Atmospheric Nitrogen deposition") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/saltless/Atmospheric N Deposition.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")


  