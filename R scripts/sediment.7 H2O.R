
# Create a random forest model to predict NGU sediment classes from bathymetry and shear stress

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "h2o", "viridis", "patchwork", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

h2o.init()                                                       # Start running h2o in the background

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

Data <- readRDS("./Objects/RF_sediment_observations.rds") %>%    # Read in data
  drop_na() %>%                                                  # Drop point without all estimates ** might get more if you don't filter land out of bathymetry?
  mutate(Sed_class = as.factor(Sed_class))

#### Split into training and validation ####

Training <- Data %>% st_drop_geometry %>% group_by(Sed_class) %>% sample_frac(0.7) %>% ungroup
Validation <- Data %>% st_drop_geometry %>% anti_join(Training)

#### Points to predict for ####

To_predict <- readRDS("./Objects/RF_sediment_observations.rds") %>% # Read in data
  filter(is.na(Sed_class)) %>%                                  # Limit to points we don't know about sediment
  select(-Sed_class) %>%                                        # Drop the sediment columns so we can drop NAs
  st_join(., domains) %>%                                        # Locate points in model boxes
  drop_na() 

#### Build model ####

tic()
RF <- h2o.randomForest(y = "Sed_class",                         # Predict Sediments
                       training_frame = as.h2o(Training),                  
                       validation_frame = as.h2o(Validation),
                       ntrees = 500,
                       seed = 5678)                             # Set seed to ensure reproducibility
toc()

h2o.saveModel(RF, "./Objects/Sediment modelh2o")

#### Predict at unkown locations ####

tic()
prediction <- h2o.predict(RF, as.h2o(st_drop_geometry(To_predict))) %>%  # Predict in h2o
  as.data.frame() %>%                                                    # Copy results into R
  bind_cols(select(To_predict, Shore)) %>%                               # Add point geometries
  st_as_sf() %>%                                                         # Reinstate SF class
  rename(Sed_class = predict)
toc()

#### Importance of predictors ####

Importance <- h2o.varimp(RF)

Imp_plot <- ggplot() + 
  geom_point(data = Importance, aes(y = scaled_importance, x = reorder(variable, scaled_importance)), colour = "grey", shape = 21, size = 5, fill = "white", stroke = 1) +
  theme_minimal() +
  labs(y = "Scaled Importance", x = "Predictor") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"), axis.text.x = element_text(angle = 90)) +
  labs(subtitle = "Importance of predictors") +
  NULL

#### Condense sediment classes ####

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

#### Condensed errors ####

Fraction_error <- function(Confusion, coding, Fraction) {
  
  Condensed_table <- Confusion %>% 
    select(-c(Error, Rate)) %>% 
    rownames_to_column(var = "Actual") %>% 
    filter(Actual != "Totals") %>% 
    pivot_longer(-Actual, names_to = "Predicted", values_to = "Count") %>%
    mutate(Actual = as.factor(Actual), 
           Predicted = as.factor(Predicted))

  levels(Condensed_table$Actual) <- coding 
  levels(Condensed_table$Predicted) <- coding

  Condensed_table %>% 
    group_by(Actual, Predicted) %>% 
    summarise(count = sum(Count)) %>% 
    ungroup() -> Condensed_table
    
  Fraction_error <- uncount(Condensed_table, count) %>% 
    mutate(Match = Actual == Predicted)              # Proportion of times right (estimate of error rate)                  
    
  
  Condensed_table <- mutate(Condensed_table,
                            Fraction = Fraction,
                            Error = mean(Fraction_error$Match))
    } # Extract the sediment fraction error from a confusion matrix

Fraction_error_plot <- function(data) {
    ggplot(data, aes(x= Actual, y = Predicted, fill = log(count+1))) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
    labs(subtitle = str_glue("Accuracy - {data$Fraction[1]} = {round(data$Error[1]*100, digits = 1)} %"), x = NULL) +
    theme_minimal() +
    annotate("segment", x = 0.5, y = 0.5, xend = length(unique(data$Predicted)) + 0.5, 
             yend = length(unique(data$Predicted)) + 0.5, colour = "white") +
    NULL
}                   # Create a plot of fit for a sediment fraction
  
errors <- map2_dfr(list(Hard_levels, Grav_levels, Sand_levels, Silt_levels),# For each set of levels contributing to a fraction
               c("Hard", "Gravel", "Sand", "Silt"), Fraction_error,         # Calculate the condensed error
               Confusion = h2o.confusionMatrix(RF, valid = TRUE))           # From the random forest output

plots <- split(errors, f = list(errors$Fraction)) %>%                       # Split errors by sediment fraction
  map(Fraction_error_plot)                                                  # Create a plot for each fraction

Imp_plot + ((plots[["Hard"]] + plots[["Gravel"]]) / 
            (plots[["Sand"]] + plots[["Silt"]])) +                          # Bind in a facet 
  plot_layout(widths = c(0.3, 1)) +                                         # Adjust column widths
  plot_annotation(title = 'Random Forest performance: split by 4 sediment classes') & # Common title
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")

ggsave("./Figures/sediment/4 classess.png", plot = last_plot(), scale = 1, width = 16, height = 15, units = "cm", dpi = 500)

#### Full sediment map ####

Sediment <- rbind(                                # Bind
  select(Data, Sed_class),                        # Sediment observations
  select(prediction, Sed_class))                  # To sediment predictions

saveRDS(Sediment, "./Objects/modelled_sediment.rds")

h2o.shutdown(prompt = FALSE)                                                # Close h2o, otherwise you'll hit memory limits later
