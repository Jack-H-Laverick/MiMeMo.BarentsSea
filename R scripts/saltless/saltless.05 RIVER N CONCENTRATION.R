
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(tidyverse)
library(sf)
source("./R scripts/@_Region file.R")                                       # Define project region 

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import domain
  st_union() %>%                                                            # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                              # Fill in the gap to capture rivers on Svalbard
  st_make_valid() %>%                                                 
  st_buffer(dist = 30000) %>%                                               # Increase the size to spill slightly onto land
  nngeo::st_remove_holes() %>%                                              # Remove the new holes
  st_as_sf() %>%                                                            # Reinstate formatting
  mutate(Keep = T)                                         

#### Spatial subsetting ####

rivers <- read.csv("./Data/River_N/original/GNM_database/mouth/mouth_coordinates.csv", sep = ";") %>% # Import river mouth positions
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%                        # Set as sf object
  st_transform(crs = crs) %>%                                               # Match model domain crs
  st_join(domains) %>%                                                      # Check which rivers fall in the model domain
  drop_na() %>%                                                             # Drop those outside
  st_drop_geometry()                                                        # Drop special formatting for speed

## Visual check we got the rivers we were after 
#ggplot(domains) +
#  geom_sf() +
#  geom_sf(data = rivers, aes(colour = Keep), size = 0) +
#  coord_sf(xlim = st_bbox(domains)[c("xmin", "xmax")], 
#           ylim = st_bbox(domains)[c("ymin", "ymax")])

#### Extract data ####

Nitrogen <- read.csv("./Data/River_N/original/GNM_database/mouth/Nload.csv", sep = ";") %>% # Import annual nitrogen load
  filter(basinid %in% rivers$basinid) %>%                                   # Limit to rivers of interest
  pivot_longer(-basinid, names_to = "Year", values_to = "Nitrogen")         # Get all years into a single column

N_concentration <- read.csv("./Data/River_N/original/GNM_database/mouth/discharge.csv", sep = ";") %>% # Get river volumes
  filter(basinid %in% rivers$basinid) %>%                                   # Limit to rivers of interest
  pivot_longer(-basinid, names_to = "Year", values_to = "Discharge") %>%    # Get years into one column
  left_join(Nitrogen) %>%                                                   # Pair nitrogen load with river volume
  mutate(Year = as.numeric(str_sub(Year, start = 2)),                       # Clean year column
         Nitrogen = Nitrogen * 1e6,                                         # Convert kg to milligrams
         Discharge = Discharge * 1e12) %>%                                  # Convert km^3 to l
  mutate(Concentration = Nitrogen/Discharge) %>%                            # Calculate N concentration for each river/year
  group_by(Year) %>%                                                        # Per year
  summarise(`DIN mg.l` = weighted.mean(Concentration, Discharge))           # Get the mean concentration of rivers, weighted towards bigger rivers

saveRDS(N_concentration, "./Objects/River DIN.rds")

ggplot(N_concentration) +
  geom_line(aes(x = Year, y = `DIN mg.l`))
