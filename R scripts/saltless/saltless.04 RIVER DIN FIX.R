
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
library(zoo)

#### Import time series of river DIN and Ammonia from the river Ob ####

Data <- readxl::read_excel("./Data/River_N/ArcticGRO Water Quality Data.xlsx", 
                   sheet = "Ob", skip = 9) %>%                              # Read in data for Ob' river  
  select(3, 21, 23) %>%                                                     # Select date, total DIN, Ammonia
  setNames(c("Date", "TDN", "NH4")) %>% 
  mutate(NH4 = micro_to_milli(NH4)) %>%                                               # Get both quantities into milligrams
  mutate(Proportion = NH4/TDN,                                              # Get the proportion of DIN as ammonia
         Month = month.name[lubridate::month(Date)],            
         Year = lubridate::year(Date)) %>%         
  drop_na() %>% 
  group_by(Month, Year) %>%                                                 # Average if there were multiple samples in a month
  summarise(Proportion = mean(Proportion),
            TDN = mean(TDN)) %>% 
  ungroup() %>% 
  mutate(Date = as.Date(paste0(1, Month, Year), format = "%d%B%Y"))
  
ggplot(Data) +
  geom_line(aes(x = Date, y = Proportion))

steps <- seq(min(Data$Date), by = "1 month", to = max(Data$Date))

#### Perform temporal interpolation ####

Interp <- zoo::merge.zoo(zoo(rep(1, length.out = length(steps)), steps),
                         zoo(cbind(Data$Proportion, Data$TDN), Data$Date)) %>% # bind full time steps to observations
  setNames(c("Dummy", "Proportion", "TDN")) %>% 
  .[index(.) > as.Date("2009-12-31") & index(.) < as.Date("2020-01-01")]       # Drop poorly sampled early years

Interp$Proportion_INT <- na.spline(Interp$Proportion, method = "monoH.FC")     # Interpolate Ammonia proportion
Interp$TDN_INT <- na.spline(Interp$TDN, method = "monoH.FC")                   # Interpolate DIN

Interp_gg <- as.data.frame(Interp) %>%                                         # Convert back to a dataframe
  tibble::rownames_to_column("Date") %>% 
  mutate(Date = as.Date(Date))

ggplot(Interp_gg) +
  geom_line(aes(x = Date, y = Proportion_INT), colour = "Red") +
  geom_line(data = Data, aes(x = Date, y = Proportion)) +
  geom_vline(xintercept = as.Date("2010-12-31"), linetype = "dashed") +       # Marker for the project reference period
  theme_minimal()

ggplot(Interp_gg) +
  geom_line(aes(x = Date, y = TDN_INT), colour = "Red") +
  geom_line(data = Data, aes(x = Date, y = TDN)) +
  geom_vline(xintercept = as.Date("2010-12-31"), linetype = "dashed") +       # Marker for the project reference period
  theme_minimal()

#### Seasonal cycle #### 

Seasonal <- filter(Interp_gg, Date > as.Date("2010-12-31")) %>%               # Drop the earliest year of interpolations (used a spare year to get the spline to fit well)
  mutate(Month = lubridate::month(Date)) %>% 
  group_by(Month) %>% 
  summarise(`NH4/DIN` = mean(Proportion_INT, na.rm = T),                      # Get the average seasonal cycle
            `DIN` = mean(TDN_INT, na.rm = T)) %>%
  ungroup()

ggplot(Seasonal) +
  geom_line(aes(x = Month, y = `NH4/DIN`)) +
  theme_minimal()

#### Compare to last century ####

G_NEWs <- readRDS("./Objects/River DIN.rds")

ggplot() +
  geom_line(data = G_NEWs, aes(x = Year, y = `DIN mg.l`)) +
  geom_smooth(data = Data, aes(x = Year, y = TDN), colour = "red", method = "lm") +
  geom_point(data = Data, aes(x = Year, y = TDN), colour = "red") +
  ylim(0, 0.7)

#### Impose seasonalitiy on last century ####

G_NEWs <- filter(G_NEWs, Year == 2000)$`DIN mg.l`                             # Get the average domain DIN concentration for a year (ie constant across months)

fixed <- Seasonal %>% 
  mutate(DIN = DIN/mean(DIN)*G_NEWs) %>%                                      # Scale the average DIN from G_NEWs by the seasonal change in DIN from Ob'   
  mutate(Ammonia = `NH4/DIN` * DIN,                                           # Get proportion of new DIN as ammonia
         Nitrate = (1-`NH4/DIN`) * DIN) %>%                                   # As Nitrate
  select(Month, Ammonia, Nitrate)

saveRDS(fixed, "./Objects/River nitrate and ammonia.rds")

ggplot() +
  geom_line(data = fixed, aes(x = Month, y = Ammonia), colour = "red") +
  geom_line(data = fixed, aes(x = Month, y = Nitrate))
 