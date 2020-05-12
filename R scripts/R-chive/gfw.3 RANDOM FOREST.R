
# Create a random forest model to predict fishing activity over sediment classes

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "randomForest", "viridis", "egg") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages

Data <- readRDS("./Objects/RF_Observations.rds") %>%             # Read in data
  drop_na() %>%                                                  # Drop point without all estimaes ** might get more if you don't filter land out of bathymetry?
  mutate(Sed_class = as.factor(Sed_class)) %>%
  select(-c(Sed_hard))                                           # Drop the sediment columns

To_predict <- readRDS("./Objects/RF_Observations.rds") %>%       # Read in data
  filter(is.na(Sed_class)) %>%                                   # Limit to points we don't know about sediment
  select(-c(Sed_class, Sed_hard)) %>%                            # Drop the sediment columns so we can drop NAs
  drop_na()

ggplot() + geom_sf(data = Data, aes(colour = Bathymetry)) + ggtitle("Data")
ggplot() + geom_sf(data = To_predict, aes(colour = Bathymetry)) + ggtitle("To predict")

st_geometry(Data) <- NULL                                        # Strip out spatial information (this will make the fit worse, but how can we justify this when predicting Greenland?)


Seasonal <- readRDS("./Objects/Seasonal.Rdata")                  # Import average fishing activity by month

####

# What predictors do I want?      Ice conc, Bathymetry, Month
# Is using sediment circular? Can only do this for the Barents sea/ after predicting Greenland.
# Sediment could affect certain gears though.
# Fit a model for each gear type separately?

#### Need to grid GFW ####

Tidy_packages <- c("tidyverse", "ggfortify", "viridis")                       # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth", "nngeo")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages

domains <- readRDS("./Objects/Domains.rds") %>%                               # Load SF polygons of the MiMeMo model domains
  filter(Region == "Barents Sea") %>%                                         # We only have sediment for the Barents sea, so limit the grid
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

box <- st_bbox(domains)

grid <- st_as_stars(box, dx = 0.01, dy = 0.01, values = 0) %>%
  st_as_sf(as_points = FALSE, merge = FALSE)                                  # Convert to polygons for cells

extract <- function(data) {
  
  day <- read.csv(data, header = TRUE) %>%
    filter(between(lat_bin, box[1]*100, box[2]*100), between(lon_bin, box[3]*100, box[4]*100))
}

Data <- list.files(path = "./Data/GFW daily_csvs", pattern ="*.csv", full.names = TRUE) #%>%              # Very slow! reads in 11gb of raw data 
#         sapply(read.csv, header = TRUE, simplify = FALSE) 

GFW_day <- extract(Data[1]) %>%
  mutate(Latitude = lat_bin/100, Longitude = lon_bin/100) %>%
  select(-c(lat_bin, lon_bin)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)                   # Specify original projection (crs) 


test <- st_join(grid, GFW_day)



raster <- st_rasterize(GFW_day["fishing_hours"], deltax = 0.01, deltay = 0.01)

ggplot() +
  geom_stars(data = raster)





#### Split into training and validation ####

train <- sample(nrow(Data), 0.7*nrow(Data), replace = FALSE)     # Training:Validation, 70:30 (at random)

Training <- Data[train,]
Validation <- Data[-train,]

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
  labs(subtitle = "Importance of predictors")
  NULL
  
#### Predict with model ####

predTrain <- predict(RF, Training, type = "class")               # Predicting 70% training dataset (no excuse to be wrong)
T_confusion <- table(predTrain, Training$Sed_class) %>%          # Create a confusion table
 data.frame()                                                    

predValid <- predict(RF, Validation, type = "class")             # Predicting on 30% validation set
error <- mean(predValid == Validation$Sed_class)                 # Proportion of times right (estimate of error rate)                  
V_confusion <- table(predValid, Validation$Sed_class) %>%        # Create a confusion table
  data.frame()

#### Plot performance ####

Valid_plot <- mutate(V_confusion, Freq_Training = T_confusion$Freq) %>% # Make plottable 
  rename(Predicted = predValid, Actual = Var2, Freq_Validation = Freq) %>% 
  pivot_longer(cols = starts_with("Freq"), names_to = "Set", values_to = "Freq") %>% # Gather columns
  mutate(Set = str_sub(Set, start = 6))                          # Drop factor prefix

Acc_plot <- ggplot() + 
  geom_raster(data = Valid_plot, aes(x= Actual, y = Predicted, fill = log(Freq+1)), interpolate = TRUE) +
  scale_fill_viridis(option = "inferno", name = "(log+1)\nFrequency") +
  facet_grid(cols = vars(Set)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Random Forest of sediments performance",
       subtitle = paste0("Accuracy = ", round(error*100, digits = 2),"%")) +
  NULL

comb <- ggarrange(Acc_plot, Imp_plot, nrow = 2, heights = c(1, 0.5))
ggsave("./Figures/gfw/performance.png", plot = comb, scale = 1, width = 16, height = 15, units = "cm", dpi = 500)

#### Tuning ####

# How many variables should we check at each node?

a=c()
for (i in 2:7) {
  model1 <- randomForest(Sed_class ~ ., data = Training, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model1, Validation, type = "class")
  a[i-1] = mean(predValid == Validation$Sed_class)
}
a
plot(2:7,a)

# optimal solution isn't stable at 1000 ntree - causes variation in accuacy of 1%         

