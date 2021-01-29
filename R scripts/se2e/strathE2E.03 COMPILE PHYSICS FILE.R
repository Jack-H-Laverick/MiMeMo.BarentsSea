
## Overwrite example driving data (boundaries and physics)

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)

Physics_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/physics_NORTH_SEA_1970-1999.csv") # Read in example Physical drivers

#### Last minute data manipulation ####

My_scale <- readRDS("./Objects/Domains.rds") %>%                            # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "slab_layer", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation)) %>% 
  dplyr::select(Shore, slab_layer, Volume)

My_light <- readRDS("./Objects/Air temp and light.rds") %>% 
  filter(between(Year, 2011, 2019), grepl("Light", Type)) %>%               # Limit to reference period and variable
  group_by(Month) %>%                                                       # Average across months
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order to match template

My_AirTemp <- readRDS("./Objects/Air temp and light.rds") %>% 
  filter(between(Year, 2011, 2019), grepl("Air", Type)) %>%                 # Limit to reference period and variable
  group_by(Month, Shore) %>%                                                # Average across months
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order to match template

My_H_Flows <- readRDS("./Objects/H-Flows.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
  summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
  ungroup() %>% 
  left_join(My_scale) %>%                                                   # Attach compartment volumes
  mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
  mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
  arrange(Month)                                                            # Order by month to match template

My_V_Flows <- readRDS("./Objects/vertical diffusivity.rds") %>%
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Month) %>% 
  summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order by month to match template

My_volumes <- readRDS("./Objects/TS.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Compartment, Month) %>%                                          # By compartment and month
  summarise(across(Salinity_avg:Ice_conc_avg, mean, na.rm = T)) %>%         # Average across years for multiple columns
  ungroup() %>% 
  arrange(Month)                                                            # Order by month to match template

My_SPM <- readRDS("./Objects/Suspended particulate matter.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Shore, Month) %>% 
  summarise(SPM = mean(SPM, na.rm = T)) %>%                                 # Average by month across years
  ungroup() %>% 
  arrange(Month)                                                            # Order by month to match template

My_Rivers <- readRDS("./Objects/River volume input.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%                                     # Limit to reference period
  group_by(Month) %>% 
  summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across years
  ungroup() %>% 
  arrange(as.numeric(Month))                                                # Order by month to match template

My_Stress <- readRDS("./Objects/Habitat disturbance.rds") %>% 
  mutate(Month = factor(Month, levels = month.name)) %>%                    # Set month as a factor for non-alphabetical ordering
  arrange(Month)                                                            # Arrange to match template

My_Waves <- readRDS("./Objects/Significant wave height.rds") %>%  #*2000 - 2010   
  filter(Shore =="Inshore") %>%                                             # Limit to inshore
  group_by(Month) %>%                                                       # By month
  summarise(SWH = mean(SWH, na.rm = T)) %>%                                 # Average
  ungroup %>% 
  arrange(Month)                                                            # Arrange to match template

#### Create new file ####

Physics_new <- mutate(Physics_template, SLight = My_light$Measured,
                     ## Flows, should be proportions of volume per day
                     SO_OceanIN = filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     D_OceanIN = filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanIN = filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanOUT = filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow,
                     SO_SI_flow = filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow,
                     Upwelling = 0, # Nominal value   
                     ## log e transformed suspended particulate matter concentration in zones
                     SO_LogeSPM = log(filter(My_SPM, Shore == "Offshore")$SPM),  
                     SI_LogeSPM = log(filter(My_SPM, Shore == "Inshore")$SPM),
                     ## Temperatures in volumes for each zone
                     SO_temp = filter(My_volumes, Compartment == "Offshore S")$Temperature_avg,
                     D_temp = filter(My_volumes, Compartment == "Offshore D")$Temperature_avg,
                     SI_temp = filter(My_volumes, Compartment == "Inshore S")$Temperature_avg ,
                     ## River inflow,
                     Rivervol_SI = My_Rivers$Runoff / filter(My_scale, Shore == "Inshore")$Volume, # Scale as proportion of inshore volume
                     ## Vertical diffusivity
                     log10Kvert = log10(My_V_Flows$V_diff),
                     mixLscale = mixLscale, # Length scale over which vertical diffusion acts, nominal
                     ## Daily proportion disturbed by natural bed shear stress
                     habS1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance, 
                     habS2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance,
                     habS3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance,
                     habD1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance,
                     habD2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance,
                     habD3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance, 
                     ## Monthly mean significant wave height inshore                     
                     Inshore_waveheight = My_Waves$SWH,
                     ## Cryo variables
                     SO_IceFree = 1 - filter(My_volumes, Compartment == "Offshore S")$Ice_pres,
                     SI_IceFree = 1 - filter(My_volumes, Compartment == "Inshore S")$Ice_pres,
                     SO_IceCover = filter(My_volumes, Compartment == "Offshore S")$Ice_conc_avg,
                     SI_IceCover = filter(My_volumes, Compartment == "Inshore S")$Ice_conc_avg,
                     SO_IceThickness = filter(My_volumes, Compartment == "Offshore S")$Ice_Thickness_avg, 
                     SI_IceThickness = filter(My_volumes, Compartment == "Inshore S")$Ice_Thickness_avg,
                     SO_SnowThickness = filter(My_volumes, Compartment == "Offshore S")$Snow_Thickness_avg, 
                     SI_SnowThickness = filter(My_volumes, Compartment == "Inshore S")$Snow_Thickness_avg,
                     SO_AirTemp = filter(My_AirTemp, Shore == "Offshore")$Measured,
                     SI_AirTemp = filter(My_AirTemp, Shore == "Inshore")$Measured)
                     
write.csv(Physics_new, file = "./StrathE2E/Models/Barents Sea/2011-2019/Driving/physics_BARENTS_SEA_2011-2019.csv", row.names = F)
