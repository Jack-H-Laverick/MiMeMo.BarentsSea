
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "sf", "tictoc", "furrr")                      # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("./Data/Light and air temp", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year", NA), 
           remove = FALSE, sep = "_y") %>%                                  # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                # Drop file extension to get number
  separate(value, into = c(NA, NA, NA, "Type", NA, NA), 
         remove = FALSE, sep = "[/_]") %>%                                  # Extract the data type and period from the file name
  rename(File = "value") %>% 
  filter(Type == "SWF") %>% 
  select(-Type)

examples <- slice(all_files)            # Get one example for each file type

Space <- Window(examples[1,]$File, w = 177, e = 179, s = 57, n = 58)           # Get values to crop a netcdf file spatially at import. Both data types share a grid

#### new_function ####

get_light <- function(File, Year, months) {
  
File <- examples$File
  
  nc_raw <- ncdf4::nc_open(File)                                             # Open up a netcdf file to see it's raw contents (var names)
  nc_var <- ncdf4::ncvar_get(nc_raw, "SWF", c(Space$Limits$Lon_start, Space$Limits$Lat_start, 1),  # Extract the variable of interest
                             c(Space$Limits$Lon_count, Space$Limits$Lat_count, -1)) # cropped to window, with all time steps
  ncdf4::nc_close(nc_raw)                                                           # You must close an open netcdf file when finished to avoid data loss
  Data <- as.data.frame.table(nc_var, responseName = "Measured") %>%         # Reshape array as dataframe
    dplyr::rename(Time_step = Var1) %>%   # Name the columns
    dplyr::mutate(Time_step = 1:length(unique(Time_step))) %>%
    dplyr::left_join(months) %>%                                             # Assign a month to each time step
    dplyr::mutate(Year = Year) %>%                                            # Attach Year
    dplyr::group_by(Month, Year) %>%          # We don't need to bother accounting for shore in light data
    dplyr::summarise(light = mean(Measured)) # Average by time step.
  
  return(Data)
}

#### Get Aberdeen sense check ####

Aberdeen <- rbind(readxl::read_excel(".//Data/E_block - daily integrals.xlsx", sheet = 1),
                  readxl::read_excel(".//Data/E_block - daily integrals.xlsx", sheet = 2),
                  readxl::read_excel(".//Data/E_block - daily integrals.xlsx", sheet = 3)) %>% 
  select(1:6) %>% 
  rename(Month = Month...2) %>% 
  group_by(Year, Month) %>% 
  summarise(light = mean(`Average_Irradiance uE/m2/s`)) %>% 
  mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"),
         Integral = micro_to_full(light*60*60*24))         # Scale to per day and convert units from uE to E

#### Extract Air light ####

Light_months <- data.frame(Time_step = seq(1,360, 1), Month = rep(1:12, each = 30))     # Add month, 30 days in a model with a 360 day year

Air <- future_pmap_dfr(all_files, get_light, Light_months, .progress = TRUE) %>%                        # Data extraction with parameters stored rowise in dataframe, executed in par
  ungroup() %>%
  mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y")) %>%          # Get date column for plotting
  filter(between(Year, 2007, 2009)) %>% 
  mutate(light = shortwave_to_einstein(light))          # Watts Integrated per day to einsteins (approximately)

#### Plot ####

ggplot() +
  geom_line(data = Air, aes(x = Date, y = light), size = 0.25) +
  geom_line(data = Aberdeen, aes(x = Date, y = Integral), size = 0.25, colour = "red") +
  theme_minimal() +
  labs(y = NULL, caption = "NEMO-MEDUSA driving data compared to instrumentation") +
  theme(legend.position = "top") +
  NULL

