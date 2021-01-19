
## Combine EU and Norwegian fishing effort, then inflate by missing Russian landings to get International effort

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr", "sf")        # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need effort scaled per m^2
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

IMR <- readRDS("./Objects/IMR absolute fishing effort")                     # Import Norwegian fishing effort        

EU <- readRDS("./Objects/EU absolute fishing effort")                       # Import EU fishing effort

target <- read.csv("./Data/MiMeMo gears.csv") %>%                           # Load fishing gear classifications
  dplyr::select(Aggregated_gear, Gear_type) %>%                             # Select gear names
  distinct() %>%                                                            # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                      # Drop unused gears

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%    # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                             # Drop unnecessary columns
  st_union() %>%                                                            # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                              # Drop holes so we don't accidentally lose fishing near Svalbard
  st_make_valid() %>%                                                       # Still some residual holes so make valid
  nngeo::st_remove_holes()                                                  # And drop again

#### Get Russian correction factor ####

Inflation <- c("NOR_mobile_gear", "NOR_static_gear",                        # For each variable in the GFW file
               "RUS_mobile_gear", "RUS_static_gear",
               "REST_mobile_gear", "REST_static_gear") %>% 
  future_map(~{ brick("./Objects/GFW.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T) %>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "_") %>%          # Split variable name into flag and gear type
  mutate(Gear_type = str_to_sentence(Gear_type)) %>%                        # Capitalise to match to target
  group_by(Gear_type) %>%                                                   # Now for each gear type
  mutate(total_gear_effort = sum(Hours)) %>%                                # Total effort
  filter(Flag != "RUS") %>%                                                 # Don't need Russian data anymore
  summarise(Inflation = mean(total_gear_effort)/sum(Hours))%>%              # How do we get from non-Russian effort to our known total?
  ungroup() %>%
  right_join(target) %>%                                                    # Bind to all gear names
  mutate(Inflation = ifelse(Aggregated_gear %in% c("Harpoons", "Rifles", "Kelp harvesting"),
                            1, Inflation)) %>% 
  column_to_rownames('Aggregated_gear') %>%                                 # Match names to EU and IMR objects
  dplyr::select(Inflation) %>%                                              # Select only numeric column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)),]                                                   # Alphabetise rows to ensure a match with other objects

####  Scale to international effort ####

International <- (EU + IMR) * Inflation                                                           

transformed_International <- International / 365 * 60 / domain_size         # Convert to daily effort in seconds per m^2  

saveRDS(transformed_International, "./Objects/International effort by gear.rds")
