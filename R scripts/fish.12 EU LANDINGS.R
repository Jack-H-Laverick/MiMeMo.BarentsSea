
# Calculate absolute fishing effort by gear across EU flags and habitats

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr", "tictoc")# List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)                                                            # Set up parallel processing

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

habitats <- readRDS("./Objects/Habitats.rds")                                 # Load habitat polygons
  
tic()
EU_landings <- str_glue("./Data/EU fish/spatial_landings_{2015:2018}/") %>%   # Programmatically build folder names
  future_map(~{ rgdal::readOGR(.x) %>%                                        # Import each EU effort shapefile
                st_as_sf() %>%                                                # Convert to SF
                dplyr::select(year, ger_typ, rctngl_, species, ttwghtl, ttvllnd)}, # Drop some columns, ttwghtl is "total weight landed",
            .progress = T) %>%                                                # ttvllnd is "total value landed"
  bind_rows() 
toc()
 
#### Classify gears as static or mobile ####

gears <- data.frame(ger_typ = c("FPO", "OTB", "PTB", "PTM", "NK",  "OTM", "OTT", "DRB"),
                    Gear = c("Pots", "Bottom Otter Trawl", "Bottom Pair Trawl", "Mid-water Pair Trawl",
                             "unknown", "Mid-water Otter Trawl", "Otter Twin Trawls", "Dredge"),
                    Class = c("static", "mobile", "mobile", "mobile", NA, "mobile", "mobile", "mobile"))

#### Classify species by guild ####

guilds <- data.frame(FAO = unique(EU_Arctic$species))

species <- data.table::fread("./Data/IMR/IMR species list.csv", sep = ',') %>%              # Get species look up table
  rename(Species = Tall)

joined <- left_join(guilds, species) %>% 
  mutate(`Engelsk navn` = case_when(FAO == "PCR" ~ "Tanner crab nei",
                                    FAO == "LCT" ~ "Arctic eelpout",
                                    FAO == "JPS" ~ "Metallic codling",
                                    T ~ `Engelsk navn`))

Old_file <- read.csv("./Old data/Fish Landings ICES-FAO/FAO_species_reference_list_v9.csv", header = TRUE)

#### Locate EU effort inside habitat types ####

EU_Arctic <- st_contains(Region_mask, EU_landings, sparse = F) %>%            # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_landings[.,] %>%                                                         # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>%                                  # Create a column to track each EU polygon
  st_intersection(habitats) %>%                                               # Split the EU polygons cross habitat types
  mutate(area_stat_rectangle = as.numeric(st_area(.))) %>%                    # Work out the area of all the pieces
  st_drop_geometry() %>%                                                      # Drop geometry column for simplicity
  group_by(EU_polygon) %>%                                                    # Per original EU polygon
  mutate(share = area_stat_rectangle / sum(area_stat_rectangle)) %>%          # Work out the proportion of the area in each piece split over habitats
  ungroup() %>% 
  mutate(tonne_contributions = ttwghtl*share,                                 # Adjust landed weight to account for breaking up EU polygons
         value_contributions = ttvllnd*share,                                 # Adjust landed value to account for breaking up EU polygons
         Habitat = paste0(Shore, " ", Habitat)) %>%                           # Combine habitat labels
  left_join(gears) %>%                                                        # Attach gear classifications
#  left_join(guilds) %>%                                                       # Attach guild classifications
  group_by(year, Class, Habitat) %>%                                          # By year, habitat and gear
  summarise(Tonnes = sum(tonne_contributions, na.rm = TRUE),                  # Total weight
            Euros = sum(value_contributions, na.rm = TRUE)) %>%               # Total value
  ungroup() %>% 
  drop_na()                                                                   # Drop unassigned gears

## Plot 

# ggplot(EU_Arctic) +
#   geom_col(aes(y = Habitat, x = effort, fill = Habitat), position = "dodge2") +
#   geom_text(data = filter(EU_Arctic, Habitat == "Offshore Silt",
#                             Class == "mobile"), aes(y = Habitat, x = 0, group = year, label = year), 
#             position = position_dodge(0.9), angle = 0,  colour = "firebrick4", fontface = "bold", hjust = 0,
#             family = "AvantGarde") +
#   labs(x = "Absolute EU activity rates", y = "Fishing days") +
#   facet_grid(cols = vars(Class)) +
#   theme_minimal() +
#   theme(legend.position = "none")
