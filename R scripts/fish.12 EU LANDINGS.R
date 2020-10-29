
# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr", "tictoc")# List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)                                                            # Set up parallel processing

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

domain <- readRDS("./Objects/Domains.rds") %>%                                # Load habitat polygons
  st_transform(crs = 4326)

gears <- read.csv("./Data/MiMeMo gears.csv")                                  # Load fishing gear classifications

guilds <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                       # Get guilds for FAO codes
  dplyr::select(FAO, Guild) %>% 
  rename(species = FAO) %>% 
  drop_na()
  
EU_landings <- str_glue("./Data/EU fish/spatial_landings_{2015:2018}/") %>%   # Programmatically build folder names
  future_map(~{ rgdal::readOGR(.x) %>%                                        # Import each EU effort shapefile
                st_as_sf() %>%                                                # Convert to SF
                dplyr::select(year, ger_typ, rctngl_, species, ttwghtl, ttvllnd)}, # Drop some columns, ttwghtl is "total weight landed",
            .progress = T) %>%                                                # ttvllnd is "total value landed"
  bind_rows() %>% 
  rename(Gear_code = ger_typ)

#### Locate EU effort inside habitat types ####

EU_Arctic <- st_contains(Region_mask, EU_landings, sparse = F) %>%            # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_landings[.,] %>%                                                         # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>%                                  # Create a column to track each EU polygon
  st_intersection(domain) %>%                                                 # Check how EU polygons fall relative to the model domain
  mutate(area_stat_rectangle = as.numeric(st_area(.))) %>%                    # Work out the area of all the pieces
  st_drop_geometry() %>%                                                      # Drop geometry column for simplicity
  group_by(EU_polygon) %>%                                                    # Per original EU polygon
  mutate(share = area_stat_rectangle / sum(area_stat_rectangle)) %>%          # Work out the proportion of the area in each piece in the model domain
  ungroup() %>% 
  mutate(tonne_contributions = ttwghtl*share,                                 # Adjust landed weight to account for breaking up EU polygons
         value_contributions = ttvllnd*share) %>%                             # Adjust landed value to account for breaking up EU polygons
  left_join(gears) %>%                                                        # Attach gear classifications
  left_join(guilds) %>%                                                       # Attach guild classifications
  group_by(year, Aggregated_gear, Guild) %>%                                  # By year, guild and gear
  summarise(Tonnes = sum(tonne_contributions, na.rm = TRUE),                  # Total weight
            Euros = sum(value_contributions, na.rm = TRUE)) %>%               # Total value
  ungroup() %>% 
  drop_na()                                                                   # Drop unassigned gears

saveRDS(EU_Arctic, "./Objects/Landings EU.rds")

## Plot 

ggplot(EU_Arctic) +
  geom_col(aes(y = Aggregated_gear, x = Tonnes, fill = Aggregated_gear), position = "dodge2") +
#   geom_text(data = filter(EU_Arctic, Habitat == "Offshore Silt",
#                             Class == "mobile"), aes(y = Habitat, x = 0, group = year, label = year), 
#             position = position_dodge(0.9), angle = 0,  colour = "firebrick4", fontface = "bold", hjust = 0,
#             family = "AvantGarde") +
  labs(y = "Gear classes", x = "Tonnes landed") +
  facet_grid(rows = vars(Guild)) +
  theme_minimal() +
  theme(legend.position = "none")
