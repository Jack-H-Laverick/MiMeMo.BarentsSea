
# Create a random forest model to predict NGU sediment classes from bathymetric variables

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "randomForest", "viridis", "patchwork", "rnaturalearth") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

world <- ne_countries(scale = "medium", returnclass = "sf")      # Get a world map

Obs <- readRDS("./Objects/RF_sediment_observations.rds") %>%     # Read in data
  drop_na() %>%                                                  # Drop point without all estimates ** might get more if you don't filter land out of bathymetry?
  mutate(Sed_class = as.factor(Sed_class)) %>%
  select(-c(Sed_hard)) %>%                                       # Drop the sediment columns
  st_transform(crs = 3035)

Data <- Obs %>%                                                  # Keep Obs so we can plot the Data
  st_drop_geometry()                                             # Strip out spatial information (this will make the fit worse, but how can we justify this when predicting Greenland?)

To_predict <- readRDS("./Objects/RF_sediment_observations.rds") %>% # Read in data
  filter(is.na(Sed_class)) %>%                                   # Limit to points we don't know about sediment
  st_join(., domains) %>%                                        # Locate points in model boxes
  select(-c(Sed_class, Sed_hard)) %>%                            # Drop the sediment columns so we can drop NAs
  drop_na() %>%                                                  # Drop points outside model domain
  st_transform(crs = 3035)

window <- st_bbox(To_predict)

ggplot() + geom_sf(data = Obs, aes(fill = Sed_class), lwd = 0) + 
  scale_fill_viridis(name = 'Sediment\nclass',  discrete = TRUE) +
  labs(title = "Sediment observations",
       subtitle = "Norwegian Geological Survey") +
  geom_sf(data = world, fill = "black") +
  zoom +
  guides(fill = guide_legend(ncol = 2)) + # Adjustments for poster plotting
  theme_minimal() +
  NULL

#ggsave("./Figures/poster_RF sediment.png", plot = last_plot(), scale = 1, width = 16.84, height = 11.1, units = "cm", dpi = 500) # Poster

#### Split into training and validation ####

train <- sample(nrow(Data), 0.7*nrow(Data), replace = FALSE)     # Training:Validation, 70:30 (at random)

Training <- droplevels(Data[train,])                             # The subset might not capture all factor levels, if you don't "droplevles" the model will error
Validation <- droplevels(Data[-train,])

#### Create model ####

RF <- randomForest(Sed_class ~ ., data = Training, importance = TRUE) ; RF

summary(RF)

#### Importance of predictors ####

Importance <- varImpPlot(RF,type=2) %>%                          # Get the variables in order of importance
  data.frame() %>%
  rownames_to_column(var = "Predictor")           

Imp_plot <- ggplot() + 
  geom_point(data = Importance, aes(x = MeanDecreaseGini, y = reorder(Predictor, MeanDecreaseGini)), colour = "grey", shape = 21, size = 5, fill = "white", stroke = 1) +
  theme_minimal() +
  labs(x = "Mean decrease in Gini", y = "Predictor") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey")) +
  labs(subtitle = "Importance of predictors") +
  NULL
  
#### Predict with model ####

predTrain <- predict(RF, Training, type = "class")               # Predicting 70% training dataset (no excuse to be wrong)
T_confusion <- table(predTrain, Training$Sed_class) %>%          # Create a confusion table
 data.frame()                                                     

predValid <- predict(RF, Validation, type = "class")             # Predicting on 30% validation set

##**## Because some classess are missed in the training data, the errors odn't match up. I've commented out this code so batch processing will work for now.
# error <- mean(predValid == Validation$Sed_class)                 # Proportion of times right (estimate of error rate)                  
# V_confusion <- table(predValid, Validation$Sed_class) %>%        # Create a confusion table
#   data.frame()
# 
# #### Plot performance ####
# 
# Valid_plot <- mutate(V_confusion, Freq_Training = T_confusion$Freq) %>% # Make plottable 
#   rename(Predicted = predValid, Actual = Var2, Freq_Validation = Freq) %>% 
#   pivot_longer(cols = starts_with("Freq"), names_to = "Set", values_to = "Freq") %>% # Gather columns
#   mutate(Set = str_sub(Set, start = 6))                          # Drop factor prefix
# 
# Acc_plot <- ggplot() + 
#   geom_raster(data = Valid_plot, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
#   scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
#   facet_grid(cols = vars(Set)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(title = "Random Forest of sediments performance",
#        subtitle = paste0("Accuracy = ", round(error*100, digits = 2),"%")) +
#   NULL
# 
# comb <- Acc_plot / Imp_plot + plot_layout(heights = c(1, 0.5))
# ggsave("./Figures/sediment/performance.png", plot = comb, scale = 1, width = 16, height = 15, units = "cm", dpi = 500)
# 
# #### Tuning ####
# 
# # How many variables should we check at each node?
# 
# #a=c()
# #for (i in 2:7) {
# #  model1 <- randomForest(Sed_class ~ ., data = Training, ntree = 1000, mtry = i, importance = TRUE)
# #  predValid <- predict(model1, Validation, type = "class")
# #  a[i-1] = mean(predValid == Validation$Sed_class)
# #}
# #a
# #plot(2:7,a)
# 
# # optimal solution isn't stable at 1000 ntree - causes variation in accuacy of 1%         

#### errors by hard, gravel, sand, silt ? ####

Validation <- mutate(Validation, Hard_class = Sed_class, Grav_class = Sed_class, Sand_class = Sed_class, Silt_class = Sed_class)

predHard <- predValid ; predGrav <- predValid ; predSand <- predValid ; predSilt <- predValid


Hard_levels <- list(Hard = c(1, 175, 180, 185, 300), 
                    Soft = c(10, 20, 40, 80, 100, 115, 120, 130, 150, 160, 170))

Grav_levels <- list("0" = c(175, 180, 185, 206, 300, 1),
                    "<2" = c(10, 20, 40, 80, 100),
                    "<10" = 160,
                    "2-30" = c(115, 120, 130),
                    "30-80" = 150,
                    ">80" = 170)

Sand_levels <- list("0" = c(170, 175, 180, 185, 206, 300, 1),
                    "<10" = c(10, 20),
                    ">22.5" = 150,
                    "<45" = 115,
                    ">45" = 120,
                    "<50" = c(40, 80),
                    ">76" = 130,
                    "<80" = 160,
                    ">90" = 100)

Silt_levels <- list("0" = c(170, 175, 180, 185, 206, 300, 1),
                    "<9" = 130,
                    "<10" = c(100,160),
                    "<22.5" = 150,
                    "<45" = 120,
                    ">45" = 115,
                    "<50" = 80,
                    ">50" = 40,
                    ">90" = c(10, 20))

levels(predHard) <- Hard_levels ; levels(Validation$Hard_class) <- Hard_levels
levels(predGrav) <- Grav_levels ; levels(Validation$Grav_class) <- Grav_levels
levels(predSand) <- Sand_levels ; levels(Validation$Sand_class) <- Sand_levels
levels(predSilt) <- Silt_levels ; levels(Validation$Silt_class) <- Silt_levels

H_confusion <- table(predHard, Validation$Hard_class) %>%        # Create a confusion table
  data.frame() %>%
  rename(Predicted = predHard, Actual = Var2) %>% 
  mutate(Frac = "Hard")

G_confusion <- table(predGrav, Validation$Grav_class) %>%        # Create a confusion table
  data.frame() %>%
  rename(Predicted = predGrav, Actual = Var2) %>% 
  mutate(Frac = "Gravel")

Sa_confusion <- table(predSand, Validation$Sand_class) %>%       # Create a confusion table
  data.frame() %>%
  rename(Predicted = predSand, Actual = Var2) %>% 
  mutate(Frac = "Sand")

Si_confusion <- table(predSilt, Validation$Silt_class) %>%       # Create a confusion table
  data.frame() %>%
  rename(Predicted = predSilt, Actual = Var2) %>% 
  mutate(Frac = "Silt")

error_hard <- mean(predHard == Validation$Hard_class)            # Proportion of times right (estimate of error rate)                  
error_grav <- mean(predGrav == Validation$Grav_class)            # Proportion of times right (estimate of error rate)                  
error_sand <- mean(predSand == Validation$Sand_class)            # Proportion of times right (estimate of error rate)                  
error_silt <- mean(predSilt == Validation$Silt_class)            # Proportion of times right (estimate of error rate)                  

P_h <- ggplot() + 
  geom_raster(data = H_confusion, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
  scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
  labs(subtitle = paste0("Accuracy - ", "Hard = ", round(error_hard*100, digits = 1),"%"), x = NULL) +
  theme_minimal() +
  annotate("segment", x = 0.5, y = 0.5, xend = 2.5, yend = 2.5, colour = "white") +
  NULL

P_g <- ggplot() + 
  geom_raster(data = G_confusion, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
  scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
  labs(subtitle = paste0("Accuracy - ", "Gravel = ", round(error_grav*100, digits = 1),"%"), y = NULL, x = NULL) +
  theme_minimal() +
  annotate("segment", x = 0.5, y = 0.5, xend = 6.5, yend = 6.5, colour = "white") +
  NULL

P_sa <- ggplot() + 
  geom_raster(data = Sa_confusion, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
  scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
  labs(subtitle = paste0("Accuracy - ", "Sand = ", round(error_sand*100, digits = 1),"%")) +
  theme_minimal() +
  annotate("segment", x = 0.5, y = 0.5, xend = 9.5, yend = 9.5, colour = "white") +
  NULL

P_si <- ggplot() + 
  geom_raster(data = Si_confusion, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
  scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
  labs(subtitle = paste0("Accuracy - ", "Silt = ", round(error_silt*100, digits = 1),"%"), y = NULL) +
  theme_minimal() +
  annotate("segment", x = 0.5, y = 0.5, xend = 9.5, yend = 9.5, colour = "white") +
  NULL

Imp_plot2 <- Imp_plot + coord_flip() + 
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"), axis.text.x = element_text(angle = 90))

Imp_plot2 + ((P_h + P_g) / (P_sa + P_si)) +
  plot_layout(widths = c(0.3, 1)) +
  plot_annotation(title = 'Random Forest performance: split by 4 sediment classes') &
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
    
 ggsave("./Figures/sediment/4 classess.png", plot = last_plot(), scale = 1, width = 16, height = 15, units = "cm", dpi = 500)

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

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%
  mutate(Sed_class = as.factor(Sed_class))

Full <- select(To_predict, -c(Shore, area, Elevation)) %>%
  rbind(Obs) %>%
  left_join(Translate)

ggplot() + geom_sf(data = Full, aes(fill = Sed_class), lwd = 0) + 
  scale_fill_viridis(name = 'Sediment\nclass (NGU)',  discrete = TRUE) +
  # labs(title = "Sediment predictions") +
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

#### Aggregation ####
 
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

