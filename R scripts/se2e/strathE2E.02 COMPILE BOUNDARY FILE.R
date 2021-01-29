
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)

Boundary_template <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Driving/chemistry_NORTH_SEA_1970-1999.csv")  # Read in example boundary drivers

#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  filter(between(Year, 2011, 2019)) %>%                                                      # Limit to reference period
  group_by(Month, Compartment, Variable) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%                                                                         # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D")),
         #Measured = ifelse(Variable == "Chlorophyll", 
         #  Redundant      Measured * (20 / 12) * (16/106), # weight C : weight Chla, convert to moles of C 
         #                 Measured)  # weight C : weight Chla, convert to moles of C, Redfield ratio atomic N to C 
  ) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template

My_DIN_fix <- readRDS("./Objects/Ammonia to DIN.rds")

My_river_N <- readRDS("./Objects/River nitrate and ammonia.rds") %>% 
  mutate(Ammonia = (Ammonia*(1/14.006720))*1e3,                                           # Convert mg/l to mmol/m^3
         Nitrate = (Nitrate*(1/14.006720))*1e3) 

My_atmosphere <- readRDS("./Objects/Atmospheric N deposition.rds") %>% 
  filter(between(Year, 2011, 2019)) %>%       #** max date 2017                              # Limit to reference period
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states 
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                             # Order months ascending

#### Create new file ####

Boundary_new <- mutate(Boundary_template, 
                       SO_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SO_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SO_phyt = My_boundary_data$SO_Phytoplankton,
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth_layer == "Deep")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       D_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth_layer == "Deep")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       D_phyt = My_boundary_data$D_Phytoplankton,
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SI_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SI_phyt = My_boundary_data$SI_Phytoplankton, 
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
