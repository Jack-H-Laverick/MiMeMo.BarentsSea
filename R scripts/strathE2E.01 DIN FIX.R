
#### Setup ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf")
lapply(packages, library, character.only = TRUE)                            # Load packages

domains <- readRDS("./Objects/Domains.rds")                                 # Import model domain

headers <- read.csv("./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", nrows=2, header=FALSE)
headers_names <- sapply(headers,paste,collapse="_")                         # Rebuild column names because they're split over 2 rows

data <- read.csv(file="./Data/BODC/JR016-006_CTD_nutrient_data_summary_BODC_fixed.csv", skip = 2, 
                 header=FALSE)

names(data) <- headers_names                                                # Fix awkward header names

data <- cbind(data[,1:8], select(data, starts_with(c("Ammonium", "Nitrate")))) %>% 
  drop_na() %>%                                                             # Only use complete cases
  st_as_sf(coords = c("Longitude (E)_","Latitude (N)_"), crs = 4326) %>%    # Convert to sf for spatial filter later
  st_transform(crs = 3035) %>%  
  group_by(`CTD cast_`) %>%                                                 # Per cast
  arrange(`Depth (m)_`, .by_group = TRUE)                                   # Order depths ascending
  
#### Calculate proportion ####

shallow_proportion <- data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth = "Shallow")                                                 # Label depth layer 

deep_proportion <- data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)_`, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth = "Deep")                                                    # Label depth layer 

final <- rbind(shallow_proportion, deep_proportion) %>%                     # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(`CTD cast_`, Depth) %>% 
  summarise(Ammonium = weighted.mean(`Ammonium_mM l-1`, weights),           # Weighted averages
            DIN =  weighted.mean(`Nitrate+nitrite_mM l-1` +`Ammonium_mM l-1`, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  mutate(Proportion = Ammonium/DIN) %>%                                     # Get a proportion of ammonium to total DIN
  st_join(domains) %>%                                                      # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na() %>% 
  group_by(Shore, Depth) %>%                        
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n())                                                    # Number of CTD casts contributing to each estimate

saveRDS(final, "./Objects/Ammonia to DIN.rds")
