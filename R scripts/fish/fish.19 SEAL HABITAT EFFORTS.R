
#**# Distribute Seal hunting effort over seabed habitats in the mouth of the White sea.
#**# Our reading says seal hunting happens at the White Sea entrance in our model domain.
#**# The sealers take pups, and reproduction happens in the White Sea "whelping" area.

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

habitats <- readRDS("./Objects/Habitats.rds") %>%                             # Load habitat polygons
  st_transform(3035)

target <- data.frame(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = "Rifles")                             # Get combinations of gear and habitat

#### Define whelping area ####

whelping <- data.frame(x = 42.3, y = 66.9, Region = "Whelping") %>%           # Choose a point at the White Sea entrance
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%                              # Convert to an SF object
  st_transform(3035) %>%                                                      # Reproject to specify the buffer in meters
  st_buffer(120000)                                                           # Expand to a circle
  
ggplot() +
  geom_sf(data = habitats, aes(fill = Habitat)) +
  geom_sf(data = whelping,colour = "red", fill = NA)

#### Sample the habitats  ####

sample <- st_intersection(whelping, habitats) %>%                             # Cut habitats to the whelping area
  filter(Shore == "Inshore")                                                  # Limit activity to the inshore zone

ggplot(sample) +
  geom_sf(aes(fill = Habitat))

#### Distribute effort by area ####

effort <- mutate(sample, effort = as.numeric(st_area(sample))) %>%            # Calculate habitat areas
  st_drop_geometry() %>%                     
  transmute(effort = effort / sum(effort),                                    # Convert to proportions
            Habitat = paste0(Shore, " ", Habitat)) %>%                        # Create habitat column used elsewhere
  right_join(target) %>%                                                      # Join to combinations of all gears and habitats
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually 0
  pivot_wider(names_from = Aggregated_gear, values_from = effort) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                           # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns
saveRDS(effort, "./Objects/seal proportional habitat effort.rds")# Save
