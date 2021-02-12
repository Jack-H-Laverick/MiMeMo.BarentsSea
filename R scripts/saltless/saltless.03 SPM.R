
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "data.table")           # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                  # Project to Lat Lons

all_files <- list.files("./Data/SPM/", full.names = TRUE, pattern = ".nc") %>% # Get file metadata
  as.data.frame() %>%                                                       
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Date"), 
           remove = FALSE, sep = "_") %>%                                   # Extract the Date from the file name
  mutate(Year = as.numeric(str_sub(Date, end = 4)),                         # Extract the Year
         Month = as.numeric(str_sub(Date, start = 5, end = 6))) %>%         # Extract the Month
  select(-Date) %>% 
  rename(File = "value")

target <- select(all_files, - File) %>%                                     # Target to reintroduce dropped time steps
  zoo::zoo(ISOdate(.$Year, .$Month, 1, 0))                                  # Convert to a zoo object for interpolation

#### Extract data ####

data <- future_pmap(all_files, MiMeMo.tools::get_SPM,                       # Extract data from each file
                    crop = st_bbox(domains), .progress = T) %>%             # Roughly cropped to domain
  rbindlist()                                                               # bind into a data.table

#### Check which pixels fall in the model domain ####

coords <- select(data, latitude, longitude) %>%                             # Work out which pixels are in the domain
  distinct() %>%                                                            # Get unique locations
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert grid to SF
  st_join(domains) %>%                                                      # Link to model domain 
  drop_na(Shore) %>%                                                        # Drop points outside
  select(-c(Elevation, area)) %>%                                           # Drop unneeded information
  st_drop_geometry() %>%                                                    # Drop SF formatting
  mutate(Shore = as.factor(Shore))                                          

#ggplot() + geom_sf(data = coords, aes(colour = Shore)) +                   # Visual check of grid
#  geom_sf(data = domains, fill = NA)

#### Summarise ####

setDT(coords, key = c("longitude","latitude"))                              # Set data and coordinates to DT ordered by lat-lon
setDT(data, key = c("longitude","latitude"))

SPM <- data[coords,][,                                                      # For points in the model domain
  .(SPM = mean(SPM, na.rm = T)),                                            # Calculate mean SPM
  by = c("Month", "Year", "Shore")] %>%                                     # By monthly, year, and model zone
  split(.$Shore) %>%                                                        # For each model zone
  map(~{                                                                    # Interpolate missing months
  zoo <- zoo::zoo(.x, ISOdate(.x$Year, .x$Month, 1, 0))                     # Convert to a zoo object
  align <- zoo::merge.zoo(target, zoo, all = c(T,T))                        # Introduce missing time steps
        
  align$SPM <- zoo::na.approx(align$SPM, rule = 2)                          # Interpolate SPM
  
  align <- as.data.frame(align) %>%                                         # Coerce to dataframe and clean columns
    transmute(SPM = as.numeric(SPM),
              Shore = unique(.x$Shore),
              Year = as.numeric(Year.target),
              Month = as.numeric(Month.target))
  }) %>% 
  rbindlist() %>% 
  mutate(Date = as.Date(paste("01", Month, Year, sep = "/"), format = "%d/%m/%Y"),  # Get date column for plotting
         SPM = SPM * 1e3)                                                  # g to mg
saveRDS(SPM, "./Objects/Suspended particulate matter.rds")

#### Plot ####

ggplot(data = SPM) + 
  geom_line(aes(x = Date, y = SPM, colour = Shore), size = 0.25) +
  theme_minimal() +
  labs(y = expression("Suspended particulate matter mg.m"^{-3}*"Month"^{-1}), 
       caption = "GlobColour suspended particulate matter estimates from satellite") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/saltless/Suspended particulate matter.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")
