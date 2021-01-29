
## Combine EU, Norwegian, Russian fishing effort, then get international effort proportion across habitats by gear

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr", "sf")        # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

gear <- read.csv("./Data/MiMeMo gears.csv") 

habitats <- readRDS("./Objects/Habitats.rds")                               # Load habitat polygons

target <- expand.grid(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = unique(gear$Aggregated_gear))       # Get combinations of gear and guild


domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need effort scaled per m^2
  sf::st_union() %>%                                                        # To match the final units for international effort
  sf::st_area() %>% 
  as.numeric()

Non_Russian <- (readRDS("./Objects/IMR absolute fishing effort.rds") +      # Add Norwegian fishing effort        
  readRDS("./Objects/EU absolute fishing effort.rds")) /                    # to EU fishing effort
  365 * 60 / domain_size                                                    # Convert to same units as international landings

#### Get Russian effort only ####

Russian_effort <- readRDS("./Objects/International effort by gear.rds") -   # Subtracting the above from
  Non_Russian                                                               # International effort gives us the Russsian effort

#### Russian effort over habitat types ####

Habitat_weights <- c("RUS_mobile_gear", "RUS_static_gear") %>% 
  future_map(~{ brick("./Objects/GFW.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() %>% 
  separate(Variable, into = c(NA, "Gear_type", NA), sep = "_") %>%          # Split variable name into flag and gear type
  group_by(Gear_type) %>% 
  transmute(Proportion = `.`/sum(`.`),                                      # Calculate the proportion of fishing effort in each row
            Habitat = paste0(Shore, " ", Habitat),
            Gear_type = str_to_sentence(Gear_type)) %>%                     # Capitalise to allow a join     
  ungroup() %>%
  left_join(distinct(dplyr::select(gear, Aggregated_gear, Gear_type))) %>%  # Join gear type to aggregated gears
  right_join(target) %>% 
  filter(Aggregated_gear != "Dropped") %>% 
  dplyr::select(-Gear_type) %>% 
  replace_na(replace = list(Proportion = 0)) %>%                            # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Proportion) %>%   # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                         # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

Russian <- t(t(Habitat_weights) * Russian_effort)                           # Scale effort by proportion over habitats

####  Scale to international effort ####

International <- Non_Russian + Russian                                      # Get international effort by gear and habitat

International_proportion <- t(t(International)/colSums(International))      # Scale as proportions within gears

International_proportion[,"Kelp harvesting"] <- 0                           # We force all kelp harvesting to happen
International_proportion["Inshore Rock", "Kelp harvesting"] <- 1            # over inshore rock.

heatmap(International_proportion)                                           # Visualise

saveRDS(International_proportion, "./Objects/International effort proportion by gear and habitat.rds")
