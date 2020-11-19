
#**# Plot a time series of IMR landings by guild and gear to check for stability

#### Set up ####

rm(list=ls())                                                                               # Wipe the brain
packages <- c("tidyverse", "formattable", "knitr", "kableExtra")                            # List packages
lapply(packages, library, character.only = TRUE)                                            # Load packages

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
         between(Year, 2011, 2019)) %>% 
  mutate(Weight = Weight/1000)                                                              # Convert Kg to tonnes

#### Proportion IMR effort across gears and guilds ####

effort_proportion <- IMR %>% 
  group_by(Aggregated_gear, Guild) %>% 
  summarise(Effort = sum(Fishing_time, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(Effort = Effort/(sum(Effort))) %>% 
  drop_na()

ggplot(effort_proportion) + 
  geom_raster(aes(x=as.factor(Guild), y=Aggregated_gear, fill = Effort)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#### IMR average absolute activity rates by gear and IMR region ####

effort_absolute <- IMR %>% 
  group_by(Aggregated_gear, Gear_type, Guild, IMR.code) %>% 
  summarise(Effort = mean(Fishing_time, na.rm = TRUE)) %>% 
  ungroup %>% 
  drop_na()

#ggplot(effort_absolute) + 
#  geom_raster(aes(x=as.factor(Guild), y=Aggregated_gear, fill = Tonnes)) + 
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 90))

saveRDS(effort_absolute, "./Objects/IMR effort by gear and guild.rds")

## To scale Russian landings we need a barents sea wide estimate to scale against

Barents_sea <- group_by(effort_absolute, Aggregated_gear, Gear_type) %>% 
  summarise(Effort = sum(Effort, na.rm = TRUE)) %>% 
  ungroup

#### IMR landings by gear and guild ####

landings <- IMR %>% 
  group_by(Aggregated_gear, Guild) %>% 
  summarise(Tonnes = mean(Weight, na.rm = TRUE)) %>% 
  ungroup %>% 
  drop_na()

ggplot(landings) + 
  geom_raster(aes(x=as.factor(Guild), y=Aggregated_gear, fill = Tonnes)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

saveRDS(landings, "./Objects/IMR landings by gear and guild.rds")

