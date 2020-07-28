
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "tidyverse", "sf", "rnaturalearth", "viridis", "patchwork", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

Sediment <- readRDS("./Objects/modelled_sediment.rds")           # Import full sediment grif

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%  # Import quantitative values for classes
  mutate(Sed_class = as.factor(Sed_class))

world <- ne_countries(scale = "medium", returnclass = "sf")      # Get a world map

#### Transform categorical to continuous ####

Sed_quant <- left_join(Sediment, Translate) %>% 
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  sfc_as_cols()                                      # For quicker plotting

voronoi2 <- function(points, area) {
  
  result <- purrr::map(1:nrow(area), ~{                            # For each polygon in area
    voronoi <- points %>%                                          # Take the grid points
      sf::st_geometry() %>%                                        # To get sfc from sf
      sf::st_union() %>%                                           # To get a sfc of MULTIPOINT type
      sf::st_voronoi(envelope = sf::st_geometry(area[.x,])) %>%    # Voronoi polygon for the area
      sf::st_collection_extract(type = "POLYGON") %>%              # A list of polygons
      sf::st_sf() %>%                                              # From list to sf object
      sf::st_join(points) %>%                                      # put names back
      sf::st_intersection(area[.x,]) %>%                           # Cut to shape of target area
      dplyr::mutate(Cell_area = units::drop_units(sf::st_area(.))) # Area of each polygon
  } ) %>%
    dplyr::bind_rows() %>%                                         # Combine the results from each area
    sf::st_sf(geometry = .$geometry, crs = 3035)                      # Reinstate attributes of the geometry column
}

# tic()
# Sed_quant2 <- expand.grid(Longitude = seq(box[1], box[3], by = 0.01),           # Get points spread across the domain
#                         Latitude = seq(box[4], box[2], by = -0.01)) %>%                  # in 0.01 degrees
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Convert to sf
#   st_transform(crs = 3035) %>% 
#   voronoi2(st_transform(domains, crs = 3035))
# toc()
# 
# ggplot(samples2) + geom_sf(aes(fill = Cell_area), size = 0)

tic()
Sed_quant2 <-voronoi2(st_transform(Sed_quant, 3035), 
                      st_transform(domains, 3035))
toc()

tic()
ggplot() + 
  geom_raster(data = Sed_quant2, aes(x = x, y = y, fill = Sed_class)) + 
  scale_fill_viridis(name = 'Sediment\nclass (NGU)',  discrete = TRUE) +
  theme_minimal() +
  guides(fill = guide_legend(ncol = 2)) + # Adjustments for poster plotting
  NULL
toc()

tic()
ggplot() + 
  geom_sf(data = Sed_quant2, aes(fill = Sed_class)) + 
  scale_fill_viridis(name = 'Sediment\nclass (NGU)',  discrete = TRUE) +
  theme_minimal() +
  guides(fill = guide_legend(ncol = 2)) + # Adjustments for poster plotting
  NULL
toc()
# ggsave("./Figures/slides_predicted sediment.png", plot = last_plot(), scale = 1, width = 13.61, height = 8.42, units = "cm", dpi = 500, bg = 'transparent') # Poster

#### Split out to 4 maps ####

Sed_quant_long <- gather(Sed_quant2, Gravel:Hard, key = "Bottom", value = "Cover") 

ggplot() + 
  geom_raster(data = Sed_quant_long, aes(x = x, y = y, fill = Cover)) + 
  scale_fill_viridis(name = 'Cover (%)') +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15)) +
  facet_wrap(vars(Bottom)) +
  NULL

ggsave_map("./Figures/sediment/4 maps.png", plot = last_plot())

#### Proportion of habitat types in each model zone ####

aggregated <- Sed_quant_long %>%                         # Take Full set of sediment values
#  st_join(st_transform(domains, 3035)) %>%                                   # Attach model zone information to each cell
#  drop_na() %>%                                          # Drop cells outside of model domain
  st_drop_geometry() %>%                                 # Drop SF formatting
  group_by(Shore, area, Bottom) %>%                      # per bottom class by shore, and retaining model zone area for later scaling
   summarise(Cover = weighted.mean(Cover, Cell_area)) %>%# Calculate mean cover, weighted by cell size
  ungroup()                                              

scaling <- aggregated %>%                                # Calculate scaling values so all 8 habitats sum to 100%
  group_by(Shore, area) %>% 
  summarise(Scale_within = sum(Cover),                   # Whats the total amount of cover currently reported in each category? Probably 100% but good to check
            Scale_between = mean(area)) %>%              # Get a single instance of the area per shore zone 
  ungroup() %>% 
  mutate(Scale_within = Scale_within/100,                # What correction is needed to get 100% within a shore zone
         Scale_between = Scale_between/ (sum(Scale_between))) %>%  # What correction is needed to get 100% over both shore zones
  mutate(Scale = Scale_within * Scale_between)

Habitat_types <- left_join(aggregated, scaling) %>%      # Attach the scales
  mutate(Cover = Cover * Scale) %>%                      # Scale
  select(Shore, Bottom, Cover)                           # Keep only useful information

saveRDS(Habitat_types, "./Objects/Sediment area proportions.rds")

ggplot(Habitat_types) +
  geom_col(aes(x = Shore, y = Cover, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/sediment/Habitat types.png", width = 16, height = 8, units = "cm")

