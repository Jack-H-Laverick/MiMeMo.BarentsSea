
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(tidyverse)
library(zoo)

Data <- readxl::read_excel("./Data/River_N/ArcticGRO Water Quality Data.xlsx", 
                   sheet = "Ob", skip = 9) %>%                              # Read in data for Ob' river  
  select(3, 21, 23) %>%                                                     # Select date, total DIN, Ammonia
  setNames(c("Date", "TDN", "NH4")) %>% 
  mutate(TDN = TDN * 1000,                                                  # Get both quantities into micrograms
         Proportion = NH4/TDN,
         Month = month.name[lubridate::month(Date)],
         Year = lubridate::year(Date)) %>%         
  drop_na() %>% 
  group_by(Month, Year) %>% 
  summarise(Proportion = mean(Proportion)) %>% 
  ungroup() %>% 
  mutate(Date = as.Date(paste0(1, Month, Year), format = "%d%B%Y"))
  
ggplot(Data) +
  geom_line(aes(x = Date, y = Proportion))

steps <- seq(min(Data$Date), by = "1 month", to = max(Data$Date))

Interp <- zoo::merge.zoo(zoo(rep(1, length.out = length(steps)), steps),
                         zoo(Data$Proportion, Data$Date)) %>% 
  setNames(c("Dummy", "Proportion")) %>% 
  .[index(.) > as.Date("2009-12-31") & index(.) < as.Date("2020-01-01")]

Interp$Proportion_INT <- na.spline(Interp$Proportion, method = "monoH.FC")

Interp_gg <- as.data.frame(Interp) %>% 
  tibble::rownames_to_column("Date") %>% 
  mutate(Date = as.Date(Date))

ggplot(Interp_gg) +
  geom_line(aes(x = Date, y = Proportion_INT), colour = "Red") +
  geom_line(data = Data, aes(x = Date, y = Proportion)) +
  geom_vline(xintercept = as.Date("2010-12-31"), linetype = "dashed") +       # Marker for the project reference period
  theme_minimal()

#### Seasonal cycle #### 

Seasonal <- filter(Interp_gg, Date > as.Date("2010-12-31")) %>% 
  mutate(Month = lubridate::month(Date)) %>% 
  group_by(Month) %>% 
  summarise(`NH4/DIN` = mean(Proportion_INT, na.rm = T)) %>%
  ungroup()

saveRDS(Seasonal, "./Objects/River DIN fix.rds")

ggplot(Seasonal) +
  geom_line(aes(x = Month, y = `NH4/DIN`)) +
  theme_minimal()


  
