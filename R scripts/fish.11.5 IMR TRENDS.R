
#**# Plot a time series of IMR landings by guild and effort gear to check for stability

#### Set up ####

rm(list=ls())                                                                               # Wipe the brain
library(tidyverse)  
library(tseries)

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>% 
  dplyr::select(Guild, IMR.code) 
  
gear <- read.csv("./Data/MiMeMo gears.csv") 

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';', colClasses = c(RE = "character")) %>%               # Import IMR fishing data
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Area_Norway", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>% # Set column names
  filter(Area_Norway %in% c(1:4, 10:18, 20:25)) %>%                                         # Limit to areas of interest
  left_join(gear) %>%                                                                       # Attach labels
  left_join(guild) %>%                                                                      # Attach labels
  filter(Aggregated_gear != "Dropped",
         Guild != "Duplicate",
         between(Year, 2011, 2019)) %>%                                                     # 2020 is low because it's incomplete
  mutate(Weight = Weight/1000)                                                              # Convert Kg to tonnes

#### IMR effort across gears 

time_steps <- expand.grid(Year = 2011:2019, Aggregated_gear = unique(IMR$Aggregated_gear))  # Create time steps to fill times with no fishing

effort <- IMR %>% 
  group_by(Aggregated_gear, Year) %>%                                                       # By gear per year
  summarise(Effort = sum(Fishing_time, na.rm = TRUE)) %>%                                   # Total effort
  ungroup %>% 
  drop_na() %>%                                                                             
  right_join(time_steps) %>%                                                                # Attach to time steps
  replace_na(list(Effort = 0)) %>%                                                          # Set Nas to 0
  arrange(Year) %>%                                                                         # Order by year
  split(f = .$Aggregated_gear)                                                              # Split by gear

png("./Figures/IMR trends effort.png", width = 18, height = 50, units = "cm", res = 500)    # Start an image

par(mfrow=c(length(effort),2))                                                              # A row per gear 2 columns

  map(effort, ~{                                                                            # For each gear
    data <- as.matrix(.x$Effort, nrow = 1)                                                  # Get the time series
    label <- .x$Aggregated_gear[1]                                                          # And gear ID

    acf(data, lag.max = length(data), xlab = "lag position", ylab = "Autocorellation", main = label) # Plot temporal autocorrelation
    plot(2011:2019, data, type = 'l', col = 'red', xlab = "years", ylab = "Tonnes", main = "Time series") # Plot time series
  })
  
dev.off()                                                                                   # Finish file

## Trawls increasing, Shrimp trawl may be?, seines increasing, 
## decline in nets

#### IMR landings across guilds ####

time_steps <- expand.grid(Year = 2011:2019, Guild = unique(IMR$Guild))

landings <- IMR %>% 
  group_by(Guild, Year) %>% 
  summarise(Tonnes = sum(Weight, na.rm = TRUE)) %>% 
  ungroup %>% 
  drop_na() %>% 
  right_join(time_steps) %>% 
  replace_na(list(Tonnes = 0)) %>% 
  filter(Guild != "Zooplankton omnivorous") %>%          # All 0s so breaks code
  arrange(Year) %>% 
  split(f = .$Guild)  

png("./Figures/IMR trends landings.png", width = 18, height = 50, units = "cm", res = 500)

par(mfrow=c(length(landings),2))

walk(landings, ~{
  data <- as.matrix(.x$Tonnes, nrow = 1)
  label <- .x$Guild[1]
    
  acf(data, lag.max = length(data), xlab = "lag position", ylab = "Autocorrelation", main = label)
  plot(2011:2019, data, type = 'l', col = 'red', xlab = "years", ylab = "Tonnes", main = "Time series")
  
  })

dev.off()

## Evidence of trends for cetaceans, benthic carnivores, Demersals
