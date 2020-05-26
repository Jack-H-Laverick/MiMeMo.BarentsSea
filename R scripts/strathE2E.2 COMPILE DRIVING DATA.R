
## Overwrite example driving data (boundaries and physics)

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(StrathE2E2)
library(tidyverse)

copy_model(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Copy example model

file.rename("./StrathE2E/North_Sea", "./StrathE2E/Barents Sea" )                               # Rename model
file.rename("./StrathE2E/Barents Sea/1970-1999", "./StrathE2E/Barents Sea/All")                # Rename variant

copy_model(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Keep example model

Physics_template <- read.csv("./StrathE2E/Barents Sea/All/Driving_data/physics_drivers_NORTH_SEA_1970-1999.csv") # Read in example Physical drivers
Boundary_template <- read.csv("./StrathE2E/Barents Sea/All/Driving_data/boundary_data_NORTH_SEA_1970-1999.csv")  # Read in example boundary drivers

#### Update boundary file ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>% 
  filter(Year == 1980) %>% 
  select(-c(Depth, Shore)) %>%
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                                            labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D")),
         Measured = ifelse(Variable == "Chlorophyll", Measured * 40 / 12 * (106/16), Measured)) %>%  # g C : g Chla, convert to moles of C, Redfield ratio C to N 
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured)

My_atmosphere <- readRDS("./Objects/Atmospheric N deposition.rds") %>% 
  filter(Year == 2000) %>%   
  group_by(Month, Year, Oxidation_state, Shore) %>%
  summarise(Measured = sum(Measured)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured)

Boundary_new <- mutate(Boundary_template, 
                        #SO_nitrate = ,
                        #SO_ammonia = ,      
                       SO_phyt = My_boundary_data$SO_Chlorophyll, # Chlorophyll has been converted to N from phytoplankton
                        #SO_detritus = ,
                        #D_nitrate = ,      
                        #D_ammonia = ,
                       D_phyt = My_boundary_data$D_Chlorophyll,
                        #D_detritus = ,
                        #SI_nitrate = ,
                        #SI_ammonia = ,
                       SI_phyt = My_boundary_data$SI_Chlorophyll, 
                        #SI_detritus = ,
                       ## Rivers
                        #RIV_nitrate = ,     
                        #RIV_ammonia = ,          
                       RIV_detritus = 0,
                       ## Atmosphere, daily deposition as monthly averages
                       SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
                       SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
                       SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
                       SI_ATM_ammonia_flux = My_atmosphere$Inshore_R, 
                       SI_other_nitrate_flux = 0,   # Can be used for scenarios
                       SI_other_ammonia_flux = 0)    
                       
write.csv(Boundary_new, file = "./StrathE2E/Barents Sea/All/Driving_data/boundary_data_NORTH_SEA_1970-1999.csv", row.names = F)

#### Update physics file ####

My_light <- readRDS("./Objects/Air temp and light.rds") %>% filter(Year == 1980, grepl("Light", Type))  
My_AirTemp <- readRDS("./Objects/Air temp and light.rds") %>% filter(Year == 1980, grepl("Air", Type))  
My_scale <- readRDS("./Objects/Domains.rds") %>%                             # Calculate the volume of the three zones
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "Depth", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation)) %>% 
  select(Shore, Depth, Volume)
My_H_Flows <- readRDS("./Objects/H-Flows.rds") %>% filter(Year == 1980) %>% left_join(My_scale) %>% mutate(Flow = Flow/Volume) #Scale flows by compartment volume
My_V_Flows <- readRDS("./Objects/V-Flows.rds") %>% filter(Flow == "Eddy Diffusivity", Year == 1980)  
My_volumes <- readRDS("./Objects/TS.rds") %>% filter(Year == 2000)   
My_SPM <- readRDS("./Objects/Suspended particulate matter.rds") %>% filter(Year == 2000)   
# Rivers
# Bed shear stress
My_Waves <- readRDS("./Objects/Significant wave height.rds") %>% filter(Year == 2000)   

Phyics_new <- mutate(Physics_template, SLight = My_light$Measured,
                     ## Flows, should be proportions of volume per day
                     SO_OceanIN = filter(My_H_Flows, Depth == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     D_OceanIN = filter(My_H_Flows, Depth == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanIN = filter(My_H_Flows, Depth == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanOUT = filter(My_H_Flows, Depth == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow,
                     SO_SI_flow = filter(My_H_Flows, Depth == "S", Shore == "Inshore", Neighbour == "Offshore", Direction == "In")$Flow,
                     Upwelling = 0, # Nominal value   
                     ## log e transformed suspended particulate matter concentration in zones
                     SO_LogeSPM = filter(My_SPM, Shore == "Offshore")$SPM,  
                     SI_LogeSPM = filter(My_SPM, Shore == "Inshore")$SPM,
                     ## Temperatures in volumes for each zone
                     SO_temp = filter(My_volumes, Depth == "S", Shore == "Offshore")$Temperature_avg,
                     D_temp = filter(My_volumes, Depth == "D")$Temperature_avg,
                     SI_temp = filter(My_volumes, Depth == "S", Shore == "Inshore")$Temperature_avg ,
                     ## River inflow,
                      # Rivervol_SI = , 
                     ## Vertical diffusivity
                     log10Kvert = log(My_V_Flows$Value),
                     mixLscale = mixLscale,   # Length scale over which vertical diffusion acts, nominal
                     ## Daily proportion disturbed by natural bed shear stress
                      #habS1_pdist = , 
                      #habS2_pdist = ,
                      #habS3_pdist = ,
                      #habD1_pdist = ,
                      #habD2_pdist = ,
                      #habD3_pdist = , 
                     ## Monthly mean significant wave height inshore                     
                     Inshore_waveheight = filter(My_Waves, Shore == "Inshore")$SWH,
                     ## Cryo variables
                     SO_IceFree = 1 - filter(My_volumes, Depth == "S", Shore == "Offshore")$Ice_pres,
                     SI_IceFree = 1 - filter(My_volumes, Depth == "S", Shore == "Inshore")$Ice_pres,
                     SO_IceCover = filter(My_volumes, Depth == "S", Shore == "Offshore")$Ice_conc_avg,
                     SI_IceCover = filter(My_volumes, Depth == "S", Shore == "Inshore")$Ice_conc_avg,
                     SO_IceThickness = filter(My_volumes, Depth == "S", Shore == "Offshore")$Ice_Thickness_avg, 
                     SI_IceThickness = filter(My_volumes, Depth == "S", Shore == "Inshore")$Ice_Thickness_avg,
                     SO_SnowThickness = filter(My_volumes, Depth == "S", Shore == "Offshore")$Snow_Thickness_avg, 
                     SI_SnowThickness = filter(My_volumes, Depth == "S", Shore == "Inshore")$Snow_Thickness_avg,
                     SO_AirTemp = filter(My_AirTemp, Shore == "Offshore")$Measured,
                     SI_AirTemp = filter(My_AirTemp, Shore == "Inshore")$Measured)
                     
write.csv(Physics_new, file = "./StrathE2E/Barents Sea/All/Driving_data/physics_drivers_NORTH_SEA_1970-1999.csv", row.names = F)

#### test ####

list_models(path = "./StrathE2E")                                                              # List my models

model <- read_model("Barents Sea", "All", user.path = "./StrathE2E")                           # Import the model 
results <- StrathE2E(model,nyears=5)                                                           # Run for 5 years
plot_full_length_timeseries(model, results)                                                    # Plot

model <- read_model("North_Sea", "1970-1999", user.path = "./StrathE2E")                       # Check the results now look a bit different 
results <- StrathE2E(model,nyears=5)                                                           
plot_full_length_timeseries(model, results)                           
