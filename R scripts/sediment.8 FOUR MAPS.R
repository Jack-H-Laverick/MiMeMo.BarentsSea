
#RF <- readRDS("./Objects/Sediment model.rds")
#RF <- h2o.loadModel("./Objects/Sediment model")

#### Fill in gaps in the model domain ####

To_predict <- readRDS("./Objects/RF_sediment_observations.rds") %>% # Read in data
  filter(is.na(SEDKORNSTR)) %>%                                  # Limit to points we don't know about sediment
  st_join(., domains) %>%                                        # Locate points in model boxes
  select(-c(SEDKORNSTR, Hard)) %>%                               # Drop the sediment columns so we can drop NAs
  drop_na() %>%                                                  # Drop points outside model domain
  st_transform(crs = 3035)

#### Predict for unknown points ####

To_predict <- mutate(To_predict, Sed_class = predict(RF, To_predict, type = "class")) # Predicting Unkonwn Barents Sea points

ggplot() + geom_sf(data = To_predict, aes(fill = Sed_class), lwd = 0) + 
  scale_fill_viridis(name = 'Sediment\nclass',  discrete = TRUE) +
  labs(title = "Sediment predictions") +
  geom_sf(data = world, fill = "black") +
  zoom + 
  theme_minimal() +
  theme(legend.position = "None") +
  NULL

ggsave_map("./Figures/sediment/predicted sediment.png", plot = last_plot())

## conference slides

Obs <- readRDS("./Objects/RF_sediment_observations.rds") %>%     # Read in data
  drop_na() %>%                                                  # Drop point without all estimates ** might get more if you don't filter land out of bathymetry?
  mutate(Sed_class = as.factor(SEDKORNSTR)) %>%
  select(-c(SEDKORNSTR, Hard))                           

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%
  mutate(Sed_class = as.factor(Sed_class))

Full <- select(To_predict, -c(Shore, area, Elevation)) %>%
  rbind(Obs) %>%
  left_join(Translate)

ggplot() + geom_sf(data = Full, aes(fill = Sed_class), lwd = 0) + 
  scale_fill_viridis(name = 'Sediment\nclass (NGU)',  discrete = TRUE) +
  geom_sf(data = world, fill = "black") +
  zoom +
  theme_minimal() +
  guides(fill = guide_legend(ncol = 2)) + # Adjustments for poster plotting
  NULL

# ggsave("./Figures/slides_predicted sediment.png", plot = last_plot(), scale = 1, width = 13.61, height = 8.42, units = "cm", dpi = 500, bg = 'transparent') # Poster

#### Split out to 4 maps ####

Full <- gather(Full, Gravel:Hard, key = "Bottom", value = "Cover") 

ggplot() + 
  geom_sf(data = Full, aes(fill = Cover), lwd = 0) + 
  scale_fill_viridis(name = 'Cover (%)') +
  geom_sf(data = world, fill = "black", lwd = 0.1) +
  zoom +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15)) +
  facet_wrap(vars(Bottom)) +
  NULL

ggsave_map("./Figures/sediment/4 maps.png", plot = last_plot())

#### Proportion of habitat types in each model zone ####

Full %>%                                                  # Take Full set of sediment values
  st_join(st_transform(domains, crs)) %>%                # Attach model zone information to each cell
  drop_na() %>%                                          # Drop cells outside of model domain
  st_drop_geometry() %>%                                 # Drop SF formatting
  group_by(Shore, area, Bottom) %>%                      
  summarise(Cover = mean(Cover)) %>% 
  ungroup() -> Aggregated           # Calculate mean cover per bottom class by shore, retaining area for scaling

Aggregated %>%                                            # Calculate scaling values so all 8 habitats sum to 100%
  group_by(Shore, area) %>% 
  summarise(Scale_within = sum(Cover),                    # Whats the total amount of cover currently reported in each category? Probably 100% but good to check
            Scale_between = mean(area)) %>%               # Get a single instance of the area per shore zone 
  ungroup() %>% 
  mutate(Scale_within = Scale_within/100,                 # What correction is needed to get 100% within a shore zone
         Scale_between = Scale_between/ (sum(Scale_between))) %>%  # What correction is needed to get 100% over both shore zones
  mutate(Scale = Scale_within * Scale_between) -> Scaling

left_join(Aggregated, Scaling) %>%                        # Attach the scales
  mutate(Cover = Cover * Scale) %>%                       # Scale
  select(Shore, Bottom, Cover) -> Habitat_types           # Keep only useful information

saveRDS(Habitat_types, "./Objects/Sediment area proportions.rds")

ggplot(Habitat_types) +
  geom_col(aes(x = Shore, y = Cover, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/sediment/Habitat types.png", width = 16, height = 8, units = "cm")

