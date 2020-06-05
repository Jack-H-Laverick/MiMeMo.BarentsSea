
# Create a random forest model to predict NGU sediment classes from bathymetric variables

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "randomForest", "viridis", "patchwork", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

Data <- readRDS("./Objects/RF_sediment_observations.rds") %>%     # Read in data
  drop_na() %>%                                                  # Drop point without all estimates ** might get more if you don't filter land out of bathymetry?
  mutate(Sed_class = as.factor(SEDKORNSTR)) %>%
  select(-c(SEDKORNSTR, Hard)) %>%                            
  st_drop_geometry()                                             # Strip out spatial information (this will make the fit worse, but how can we justify this when predicting Greenland?)

#### Split into training and validation ####

Training <- Data %>% group_by(Sed_class) %>% sample_frac(0.7) %>% ungroup()
Validation <- anti_join(Data, Training)

#### Create model ####

tic()
RF <- randomForest(x = select(Training, -c(flowdir, aspect, Sed_class)), 
                   y = Training[["Sed_class"]], 
                   xtest = select(Validation, -c(flowdir, aspect, Sed_class)), 
                   ytest = Validation[["Sed_class"]], 
                   importance = TRUE,
                   keep.forest = TRUE) ; RF   # Keep the forest or we can't predict
toc()

saveRDS(RF, "./Objects/Sediment model.rds")

#### Importance of predictors ####

Importance <- varImpPlot(RF,type=2) %>%                          # Get the variables in order of importance
  data.frame() %>%
  rownames_to_column(var = "Predictor")           

Imp_plot <- ggplot() + 
  geom_point(data = Importance, aes(y = MeanDecreaseGini, x = reorder(Predictor, MeanDecreaseGini)), colour = "grey", shape = 21, size = 5, fill = "white", stroke = 1) +
  theme_minimal() +
  labs(y = "Mean decrease in Gini", x = "Predictor") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"), axis.text.x = element_text(angle = 90))
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
  
  Condensed_table <- as.data.frame(Confusion) %>% 
    select(-class.error) %>% 
    rownames_to_column(var = "Actual") %>% 
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
               Confusion = RF$test$confusion)                               # From the random forest output

plots <- split(errors, f = list(errors$Fraction)) %>%                       # Split errors by sediment fraction
  map(Fraction_error_plot)                                                  # Create a plot for each fraction

Imp_plot + ((plots[["Hard"]] + plots[["Gravel"]]) / 
            (plots[["Sand"]] + plots[["Silt"]])) +                          # Bind in a facet 
  plot_layout(widths = c(0.3, 1)) +                                         # Adjust column widths
  plot_annotation(title = 'Random Forest performance: split by 4 sediment classes') & # Common title
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")

ggsave("./Figures/sediment/4 classess.png", plot = last_plot(), scale = 1, width = 16, height = 15, units = "cm", dpi = 500)

#### Tuning ####
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