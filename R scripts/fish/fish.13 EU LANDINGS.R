
# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)                                                            # Set up parallel processing

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

gear <- read.csv("./Data/MiMeMo gears.csv")                                   # Load fishing gear classifications

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                        # Get guilds for FAO codes
  dplyr::select(FAO, Guild) %>% 
  rename(species = FAO) %>% 
  drop_na() %>% 
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(species) %>%                                                       # 1 duplicated code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

GFW_mobile <- brick("./Objects/GFW.nc", varname = "NOR_mobile_gear") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)  

GFW_static <- brick("./Objects/GFW.nc", varname = "NOR_static_gear") %>%      # For each class of gear
  calc(mean, na.rm = T)

landings_target <- expand.grid(Guild = unique(guild$Guild), 
                               Aggregated_gear = unique(gear$Aggregated_gear))# Get combinations of gear and guild

EU_landings <- str_glue("./Data/EU fish/spatial_landings_{2015:2018}/") %>%   # Programmatically build folder names
  future_map(~{ rgdal::readOGR(.x) %>%                                        # Import each EU effort shapefile
                st_as_sf() %>%                                                # Convert to SF
                dplyr::select(year, ger_typ, rctngl_, species, ttwghtl, ttvllnd)}, # Drop some columns, ttwghtl is "total weight landed",
            .progress = T) %>%                                                # ttvllnd is "total value landed"
  bind_rows() %>% 
  rename(Gear_code = ger_typ)

EU_Arctic <- st_contains(Region_mask, EU_landings, sparse = F) %>%            # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_landings[.,] %>%                                                         # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>% 
  left_join(gear) %>%                                                         # Attach gear classifications
  left_join(guild)                                                            # Attach guild classifications
  
#### Scale EU landings by the proportion of fishing effort according to GFW in the model domain ####

tictoc::tic()
weights <- dplyr::select(EU_Arctic, EU_polygon, Gear_type) %>%                # Limit to information needed to calculate the proportion of fishing effort in the model domain
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%                          # Isolate each shape for fast parallel processing
  future_map( ~{                                                              # In parallel
    mutate(.x, total = if_else(Gear_type == "Mobile",                         # If this is a mobile gear
                               exact_extract(GFW_mobile, .x, fun = "sum"),    # Get all mobile fishing effort from GFW, else static effort
                               exact_extract(GFW_static, .x, fun = "sum"))) %>% # This is the total effort to scale features to within a polygon
    st_intersection(domain) %>%                                               # Crop the polygons to the model domain
    mutate(feature = if_else(Gear_type == "Mobile",                           # Now count fishing effort again
                             as.numeric(exact_extract(GFW_mobile, ., fun = "sum")),       
                             as.numeric(exact_extract(GFW_static, ., fun = "sum")))) %>%  
    st_drop_geometry()}, .progress = T) %>%                                   # Drop geometry for a non-spatial join
  data.table::rbindlist() %>%                                                 # Bind into a dataframe
  mutate(GFW_Scale = feature/total) %>%                                       # Get the proportion of effort per polygon in the domain
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the polygon replace NA with 1 to not use this for scaling
  dplyr::select(GFW_Scale, EU_polygon)
tictoc::toc()

#### Convert EU landings to a matrix by guild and gear ####

corrected_landings <- st_drop_geometry(EU_Arctic) %>%
  left_join(weights) %>% 
  mutate(corrected_weight = ttwghtl * GFW_Scale) %>%                        # Scale features effort per gear by the proportion of GFW activity by gear type in the model domain 
  group_by(Aggregated_gear, Guild, year) %>% 
  summarise(corrected_weight = sum(corrected_weight, na.rm = TRUE)) %>% 
  summarise(corrected_weight = mean(corrected_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  right_join(landings_target) %>%                                           # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the uneeded gear class
  replace_na(replace = list(corrected_weight = 0)) %>%                      # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = corrected_weight) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

saveRDS(corrected_landings, "./Objects/EU landings by gear and guild.rds")

heatmap(corrected_landings)
