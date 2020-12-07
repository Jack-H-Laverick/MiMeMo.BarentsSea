
## Overwrite example driving data (boundaries and physics)

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(StrathE2E2)
library(MiMeMo.tools)

e2e_copy(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Copy example model

file.rename("./StrathE2E/Models/North_Sea", "./StrathE2E/Models/Barents Sea")                # Rename model
file.rename("./StrathE2E/Models/Barents Sea/1970-1999", "./StrathE2E/Models/Barents Sea/2011-2019") # Rename variant

e2e_copy(model.name = "North_Sea", model.variant =  "1970-1999" , dest.path = "./StrathE2E") # Keep example model

Physics_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/physics_NORTH_SEA_1970-1999.csv") # Read in example Physical drivers
Boundary_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/chemistry_NORTH_SEA_1970-1999.csv")  # Read in example boundary drivers

#### Update boundary file ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  filter(between(Year, 2011, 2019)) %>%                                                      # Limit to reference period
  group_by(Month, Compartment, Variable) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%                                                                         # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D")),
         Measured = ifelse(Variable == "Chlorophyll", 
                           Measured * 40 / 12 * (16/106), # weight C : weight Chla, convert to moles of C 
                           Measured)) %>%  # weight C : weight Chla, convert to moles of C, Redfield ratio atomic N to C 
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template

My_DIN_fix <- readRDS("./Objects/Ammonia to DIN.rds")

My_river_N <- readRDS("./Objects/River nitrate and ammonia.rds") %>% 
  mutate(Ammonia = l_to_m3(Ammonia)/(1/14.006720),                                           # Convert mg/l to mmol/m^3
         Nitrate = l_to_m3(Nitrate)/(1/14.006720)) 

My_atmosphere <- readRDS("./Objects/Atmospheric N deposition.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%       #** max date 2017                              # Limit to reference period
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states 
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                             # Order months ascending
  
Boundary_new <- mutate(Boundary_template, 
                       SO_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SO_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SO_phyt = full_to_milli(My_boundary_data$SO_Chlorophyll), # Chlorophyll has been converted to N from phytoplankton
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth == "Deep")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       D_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth == "Deep")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       D_phyt = full_to_milli(My_boundary_data$D_Chlorophyll),   # convert moles to millimoles
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SI_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SI_phyt = full_to_milli(My_boundary_data$SI_Chlorophyll), 
                       SI_detritus = My_boundary_data$SI_Detritus,
                       ## Rivers
                       RIV_nitrate = My_river_N$Nitrate,     
                       RIV_ammonia = My_river_N$Ammonia,          
                       RIV_detritus = 0,
                       ## Atmosphere, daily deposition as monthly averages
                       SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
                       SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
                       SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
                       SI_ATM_ammonia_flux = My_atmosphere$Inshore_R, 
                       SI_other_nitrate_flux = 0,   # Can be used for scenarios
                       SI_other_ammonia_flux = 0)    
                       
write.csv(Boundary_new, file = "./StrathE2E/Models/Barents Sea/2011-2019/Driving/chemistry_BARENTS_SEA_2011-2019.csv", row.names = F)
unlink("./StrathE2E/Models/Barents Sea/2011-2019/Driving/chemistry_NORTH_SEA_1970-1999.csv") # delete old file

#### Update physics file ####

My_scale <- readRDS("./Objects/Domains.rds") %>%                             # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "Depth", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation)) %>% 
  select(Shore, Depth, Volume)

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
  mutate(Flow = abs(Flow * 86400)) %>%                                     # Multiply for total daily from per second, and correct sign for "out" flows
  arrange(Month)                                                            # Order by month to match template

My_V_Flows <- readRDS("./Objects/V-Flows.rds") %>% 
  filter(between(Year, 2011, 2019), Flow == "Eddy Diffusivity") %>%         # Limit to reference period
  group_by(Month) %>% 
  summarise(Value = mean(as.numeric(Value), na.rm = T)) %>%                 # Average by month across years
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
  arrange(Month)                                                            # Order by month to match template

My_Stress <- readRDS("./Objects/Habitat disturbance.rds") %>% 
  mutate(Month = factor(Month, levels = month.name)) %>%                    # Set month as a factor for non-alphabetical ordering
  arrange(Month)                                                            # Arrange to match template

My_Waves <- readRDS("./Objects/Significant wave height.rds") %>%  #*2000 - 2010   
  filter(Shore =="Inshore") %>%                                             # Limit to inshore
  group_by(Month) %>%                                                       # By month
  summarise(SWH = mean(SWH, na.rm = T)) %>%                                 # Average
  ungroup %>% 
  arrange(Month)                                                            # Arrange to match template

Physics_new <- mutate(Physics_template, SLight = My_light$Measured,
                      ## Flows, should be proportions of volume per day
                     SO_OceanIN = filter(My_H_Flows, Depth == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     D_OceanIN = filter(My_H_Flows, Depth == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanIN = filter(My_H_Flows, Depth == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow,
                     SI_OceanOUT = filter(My_H_Flows, Depth == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow,
                     SO_SI_flow = filter(My_H_Flows, Depth == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow,
                     Upwelling = 0, # Nominal value   
                      ## log e transformed suspended particulate matter concentration in zones
                     SO_LogeSPM = log(filter(My_SPM, Shore == "Offshore")$SPM),  
                     SI_LogeSPM = log(filter(My_SPM, Shore == "Inshore")$SPM),
                      ## Temperatures in volumes for each zone
                     SO_temp = filter(My_volumes, Compartment == "Offshore S")$Temperature_avg,
                     D_temp = filter(My_volumes, Compartment == "Offshore D")$Temperature_avg,
                     SI_temp = filter(My_volumes, Compartment == "Inshore S")$Temperature_avg ,
                      ## River inflow,
                     Rivervol_SI = My_Rivers$Runoff * filter(My_scale, Shore == "Inshore")$Volume, # Scale as proportion of inshore volume
                      ## Vertical diffusivity
                     log10Kvert = log10(My_V_Flows$Value),
                     mixLscale = mixLscale,   # Length scale over which vertical diffusion acts, nominal
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
unlink("./StrathE2E/Models/Barents Sea/2011-2019/Driving/physics_NORTH_SEA_1970-1999.csv") # Delete old file

#### test ####

# list_models(path = "./StrathE2E")                                                              # List my models
# 
# model <- read_model("Barents Sea", "All", user.path = "./StrathE2E")                           # Import the model 
# results <- StrathE2E(model,nyears=5)                                                           # Run for 5 years
# plot_full_length_timeseries(model, results)                                                    # Plot
# 
# model <- read_model("North_Sea", "1970-1999", user.path = "./StrathE2E")                       # Check the results now look a bit different 
# results <- StrathE2E(model,nyears=5)                                                           
# plot_full_length_timeseries(model, results)                           
