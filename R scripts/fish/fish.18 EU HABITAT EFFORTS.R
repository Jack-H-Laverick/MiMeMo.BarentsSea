
#**# Distribute EU fishing effort over seabed habitats using GFW effort data

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

habitats <- readRDS("./Objects/Habitats.rds")                                 # Load habitat polygons

gears <- read.csv("./Data/MiMeMo gears.csv")                                  # Load fishing gear classifications

target <- expand.grid(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = unique(gears$Aggregated_gear))        # Get combinations of gear and guild

GFW_mobile <- brick("./Objects/GFW.nc", varname = "REST_mobile_gear") %>%     # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)

GFW_static <- brick("./Objects/GFW.nc", varname = "REST_static_gear") %>%     # For each class of gear
  calc(mean, na.rm = T)

EU <- rgdal::readOGR(dsn="./Data/EU fish/spatial_effort_2015-2018/") %>%      # Import EU effort shapefile
  st_as_sf() %>%                                                              # Convert to SF
  dplyr::select(year, quarter, ger_typ, rctngl_, ttfshdy) %>%                 # Drop some columns, ttfshdy is "total fishing days"
  rename(Gear_code = ger_typ)

EU_Arctic <- st_contains(Region_mask, EU, sparse = F) %>%                     # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU[.,] %>%                                                                  # Subset the Eu data spatially
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped")  
  
#### Proportion EU effort across seabed habitats by EU polygon ####

tictoc::tic()
habitat_weights <- rownames_to_column(EU_Arctic, var = "EU_polygon") %>%      # Create a column to track each IMR region and gear combination
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%                                                     # Isolate each shape for fast paralel processing
  future_map( ~ { st_intersection(.x, habitats) %>%                           # Crop the IMR region polygons to habitat types
      mutate(GFW = ifelse(Gear_type == "Mobile", exact_extract(GFW_mobile, ., fun = "sum"),          # Get the GFW fishing effort in each shape
                          exact_extract(GFW_static, ., fun = "sum")),
      Habitat = paste0(Shore, " ", Habitat)) %>%                              # Combine habitat labels
      st_drop_geometry() %>% 
      mutate(habitat_share = GFW / sum(GFW, na.rm = T)) %>%                   # Work out the proportion of activity in each piece split over habitats
      dplyr::select(habitat_share, EU_polygon, Aggregated_gear, Habitat)
    }, 
  .progress = T) %>% 
  data.table::rbindlist()

check <- habitat_weights %>%                                                  # Check shares sum to 1 by gear
  group_by(EU_polygon, Aggregated_gear) %>% 
  summarise(check = sum(habitat_share, na.rm = T))
tictoc::toc()

#### Absolute effort scaled by proportion of GFW activity in a polygon falling in the model domain ####

EU_effort <- readRDS("./Objects/EU corrected pixel fishing effort.rds") %>%   # This also means the correction for 0 years has already been done.
  dplyr::select(effort_contributions, Aggregated_gear, EU_polygon)

#### Scale and sum efforts by habitat type and gear ####

Absolute_effort_habitats <- left_join(habitat_weights, EU_effort) %>%         # Combine actual effort with habitat distribution
  mutate(effort = effort_contributions * habitat_share) %>%                   # Scale
  group_by(Aggregated_gear, Habitat) %>%                                      # Sum by gear and habitat combination
  summarise(effort = sum(effort, na.rm = T)) %>%                              # Total by group
  ungroup() %>%                                                               # Ungroup for speed
  drop_na() %>%                                                               # Drop habitats labelled as NA
  right_join(target) %>%                                                      # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = effort) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                           # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns
saveRDS(Absolute_effort_habitats, "./Objects/EU absolute habitat effort.rds") # Save

heatmap(Absolute_effort_habitats)

## How much of the corrected effort is allocated a habitat type?
sum(Absolute_effort_habitats, na.rm = T) / sum(EU_effort$effort_contributions, na.rm = T)

## Plot 

#ggplot(EU_Arctic) +
#  geom_col(aes(y = Habitat, x = effort, fill = Habitat), position = "dodge2") +
#  geom_text(data = filter(EU_Arctic, Habitat == "Offshore Silt",
#                            Gear_type == "mobile"), aes(y = Habitat, x = 0, group = year, label = year), 
#            position = position_dodge(0.9), angle = 0,  colour = "firebrick4", fontface = "bold", hjust = 0,
#            family = "AvantGarde") +
#  labs(y = "Habitats", x = "Fishing effort (hours)") +
#  facet_grid(cols = vars(Aggregated_gear)) +
#  theme_minimal() +
#  theme(legend.position = "none")
