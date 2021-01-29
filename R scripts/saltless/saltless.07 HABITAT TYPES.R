
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "sf", "stars")                     # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

sediment <- readRDS("../Sediment/Objects/modelled_sediment.rds") # Import full sediment grid

translate <- read.csv("./Data/Sediment habitats.csv") %>%        # Import sediment aggregations
  mutate(Sed_class = as.factor(SEDKORNSTR)) %>% 
  select(Sed_class, Habitat)                                     # Drop excess columns

#### Define geographic extent of each habitat type ####

habitats <- left_join(sediment, translate) %>%                   # Attach habitat labels to predicted NGU classes
  mutate(Sed_class = as.factor(Sed_class),                       # Convert to factors
         Habitat = as.factor(Habitat)) %>% 
  sfc_as_cols()                                                  # Get coordinates from sf formatting to define a raster

numeric_habitats <- mutate(habitats, Habitat = as.numeric(Habitat)) # Convert factor to numeric as st_rasterize expects numbers

polygons <- st_rasterize(numeric_habitats["Habitat"],            # Rasterize habiat labels   
                         nx = length(unique(habitats$x)),        # At the resolution of the original data
                         ny = length(unique(habitats$y))) %>% 
  st_as_sf(aspoints = FALSE, merge = TRUE) %>%                   # Merge pixels into contiguous polygons
  mutate(Habitat = factor(Habitat, labels = levels(habitats$Habitat))) %>% # Reinstate labels for factor
  group_by(Habitat) %>%  
  summarise(Habitat = Habitat[1])                                # Combine polygons into a single row per habitat

plot(polygons)

polygons <- st_intersection(st_make_valid(st_transform(polygons, crs = crs)), # Split sediment polygons along model zones
                            st_transform(domains, crs = crs)) %>% 
  select(-c(Elevation, area)) %>%                                # Drop excess data
  st_transform(crs = 4326)                                       # Switch back to mercator

saveRDS(polygons, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

proportions <- polygons %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")

