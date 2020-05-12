
#### Set up ####
setwd("~/Data/Fish Effort GFW/fishing_effort")                                # Look in the GFW folder for fishing and ice data
rm(list=ls())                                                                 # Wipe the brain
library(tidyverse)                                                            # Enter the tidyverse
library(viridis)
library(data.table)                                                           # Speedy list binding
library(RANN)                                                                 # Cool package for nearest neighbour joining

load("Seasonal.Rdata")                                                        # Import average fishing activity by month

Seasonal <- split(Seasonal, f = list(Seasonal$geartype, Seasonal$Month)) %>%  # Get a DF of file names for subset
  .[sapply(., function(x) dim(x)[1]) > 0]                                     # Drop empty dataframes (gears in months which weren't observed but split introduces)

Base_Ice <- readRDS("Satellite_Ice_M.rds")                                    # Read in NOAA satellite observations of sea ice concentration 

#### Do fishermen go near ice? ####


Ice_dodgers <- function (data) {
  
  print(str_glue("Matching Pixels to Pings for {deparse(substitute(data))}"))   # Markers to help track lapply progress
  
  closest <- nn2(data[,1:2], Base_Ice[,1:2], k = 1, searchtype = "radius", radius = 0.01) %>% # fast nearest neighbour search
    sapply(cbind) %>% as_tibble                                                 # Format as dataframe
  
  Ice <- mutate(Base_Ice, boats = ifelse(closest$nn.idx == 0, FALSE, TRUE)) %>% # Create logical vector, does an ice pixel have a boat?
    filter(Sea_ice_avg > 0)                                                     # Remove 0s to prevent 0 inflation
  Ice_fishing <- sum(Ice$boats) / nrow(Ice) * 100                               # What's the chance of fishing in iced pixels
  
  No_Ice <- mutate(Base_Ice, boats = ifelse(closest$nn.idx == 0, FALSE, TRUE)) %>% # Create logical vector, does an ice pixel have a boat?
    filter(Sea_ice_avg == 0)                                                    # Remove 0s to prevent 0 inflation
  No_Ice_fishing <- sum(No_Ice$boats) / nrow(No_Ice) * 100                      # What's the chance of fishing in ice free pixels
  
  Ice_avoidance <- No_Ice_fishing / Ice_fishing                                 # How much fishing is in ice free vs iced regions
  
  Preference <- data.frame(Ice_avoidance, Gear = unique(data$geartype), Month = unique(data$Month))
  return(Preference)
}                                          # Match nearest ice pixel to fishing pings and perform logistic regression

# Ice_dodgers(Seasonal[1:5])                                                  # test

dodgers <- lapply(Seasonal, Ice_dodgers)  %>%                                 # Apply for each Month:Gear combination
  rbindlist() %>%
  mutate(Month = as.factor(Month)) %>%                                        # Turns months into factor and fishing on % scale
  saveRDS("Ice avoidance.rds")

#### The effect of Sea Ice Concentration ####

Ice_breakers <- function (data) {
  
  print(str_glue("Matching Pixels to Pings for {deparse(sub  stitute(data))}"))    # Markers to help track lapply progress
  
  closest <- nn2(data[,1:2], Base_Ice[,1:2], k = 1, searchtype = "radius", radius = 0.01) %>% # fast nearest neighbour search
    sapply(cbind) %>% as_tibble                                                 # Format as dataframe
  
  Ice <- mutate(Base_Ice, boats = ifelse(closest$nn.idx == 0, FALSE, TRUE)) %>% # Create logical vector, does an ice pixel have a boat?
    filter(Sea_ice_avg > 0)                                                     # Remove 0s to prevent 0 inflation
  # Begs the next question, what is the chance a fishing boat is in an ice free area?
  boat.reg = glm(boats~Sea_ice_avg, data = Ice, family = binomial(), maxit = 100)            # Logistic regression of boat presence against sea ice concentration
  #summary(boat.reg)
  
  print(str_glue("Fitting logistic regression for {deparse(substitute(data))}"))# Markers to help track lapply progress
  
  fit <- data.frame(Sea_ice_avg = seq(0,1, len = 100))  %>%                     # Create 100 Sea Ice concentrations to predict for
    mutate(boat = predict(boat.reg, newdata=., type = "response"),              # Predict the chance a fishing boat is present
           Gear = unique(data$geartype),                                        # Attach geartype for plots
           Month = unique(data$Month))                                          # Attach Month for plots
  return(fit)
}                                         # Match nearest ice pixel to fishing pings and perform logistic regression

#mini <- lapply(Seasonal[1:5], Ice_breaker)  %>%                              # Apply for each Month:Gear combination (small test)
#  rbindlist()

all <- lapply(Seasonal, Ice_breakers)  %>%                                    # Apply for each Month:Gear combination
  rbindlist() %>%
  mutate(Month = as.factor(Month), boat = boat*100) %>%                       # Turns months into factor and fishing on % scale
  saveRDS("Predicted fishing around ice.rds")


#### Plotting ####

setwd("~/Analyses/Do fishermen like ice")                                     # Change working directory for saving figures and intermediary data objects
dodgers <- readRDS("Ice avoidance.rds")                                       # Read in model predictions
breakers <- readRDS("Predicted fishing around ice.rds")                       # Read in model predictions

dodgers_plot <- ggplot(data = dodgers, aes(x= Month, y = Ice_avoidance, group = Gear, colour = Gear)) +
  geom_line() +
  theme_minimal() +
  scale_colour_viridis(discrete = TRUE) +
  labs(x = "Month", y = "Preference of fishermen for ice-free vs iced waters") +
  NULL
dodgers_plot
#ggsave("FIG_Ice limits fishing.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

levels(breakers$Gear) <- list("Drifting longlines" = "drifting_longlines", "Fixed gear" = "fixed_gear",
                              "Other gear" = "other_fishing", "Purse seines" = "purse_seines", "Trawlers" = "trawlers")

background <- expand.grid(x = seq(0, 1.25, length.out = 1000), y = seq(-0.25, 3, length.out = 1000))  # Make an icey looking background

breakers_plot <- ggplot() +
  geom_raster(data = background, aes(x=x, y=y, alpha = ifelse(x > 1, 1, x)), fill = "white", show.legend = F,
              interpolate = T) +
  geom_line(data = breakers, aes(x= Sea_ice_avg, y = boat, group = Month, colour = as.integer(Month))) +
  facet_wrap(vars(Gear)) +
  theme_minimal() +
  scale_colour_gradient2(low = "blue", mid = "red", high = "blue", midpoint = 8, # Set hot midpoint to August
                         guide = "colourbar", aesthetics = "colour", name = "Month") +
  labs(x = "Sea ice concentration", y = "Chance of fishing activity in iced area (%)") +
  theme(legend.position = c(0.73, 0.23),
        panel.background = element_rect(fill = "lightblue2", colour = "lightblue2", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightblue3"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightblue3")) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,2.75)) +
  NULL
breakers_plot
#ggsave("FIG_More Ice limits fishing More.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
#ggsave("FIG_Icebreakers_poster.png", plot = last_plot(), scale = 1, width = 20.27, height = 12.67, units = "cm", dpi = 500)


