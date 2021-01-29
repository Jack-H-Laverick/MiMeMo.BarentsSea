
## Overwrite the entries in the example North Sea Physical parameters file which we have recalculated for this region

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(StrathE2E2)
library(tidyverse)

Physical_parameters <- read.csv("./StrathE2E/Models/Barents Sea/2011-2019/Param/physical_parameters_NORTH_SEA.csv") # Read in example Physical drivers

#### Update Spatial file ####

My_space <- readRDS("./Objects/Domains.rds") %>%                            # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "Depth", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Elevation[1], -60, Elevation[3] + 60)) %>% 
  mutate(Volume = area * abs(Elevation))

My_areas <- readRDS("./Objects/Sediment area proportions.rds") %>% 
  mutate(Habitat = paste0(Shore, " ", Bottom))

My_sediment <- readRDS("./Objects/Other habitat parameters.rds") %>% 
  mutate(Habitat = paste0(Shore, " ", Habitat))

Physical_parameters[1,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "S")$Elevation * -1 # Offshore_Shallow_layer_thickness_(m)
Physical_parameters[2,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "D")$Elevation * -1 # Offshore_Deep_layer_thickness_(m)
Physical_parameters[3,"Value"] <- filter(My_space, Shore == "Inshore", Depth == "S")$Elevation * -1  # Inshore_Shallow_layer_thickness_(m)

Physical_parameters[5,"Value"] <- filter(My_areas, Habitat == "Inshore Rock")$Cover      # Area_proportion_of_inshore_rock_habitat_s0_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[6,"Value"] <- filter(My_areas, Habitat == "Inshore Silt")$Cover      # Area_proportion_of_inshore_sediment_habitat_s1_(muddy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[7,"Value"] <- filter(My_areas, Habitat == "Inshore Sand")$Cover      # Area_proportion_of_inshore_sediment_habitat_s2_(sandy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[8,"Value"] <- filter(My_areas, Habitat == "Inshore Gravel")$Cover    # Area_proportion_of_inshore_sediment_habitat_s3_(gravelly)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[9,"Value"] <- filter(My_areas, Habitat == "Offshore Rock")$Cover     # Area_proportion_of_offshore_rock_habitat_d0_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[10,"Value"] <- filter(My_areas, Habitat == "Offshore Silt")$Cover    # Area_proportion_of_offshore_sediment_habitat_d1_(muddy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[11,"Value"] <- filter(My_areas, Habitat == "Offshore Sand")$Cover    # Area_proportion_of_offshore_sediment_habitat_d2_(sandy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[12,"Value"] <- filter(My_areas, Habitat == "Offshore Gravel")$Cover  # Area_proportion_of_offshore_sediment_habitat_d3_(gravelly)_(sum_of_all_8_habitat_areas_must=1)

Physical_parameters[13,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$D50    # Inshore_sediment_s1_median_grain_size_(mm). If any of these are set to 0, = Rock
Physical_parameters[14,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$D50    # Inshore_sediment_s2_median_grain_size_(mm)
Physical_parameters[15,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$D50  # Inshore_sediment_s3_median_grain_size_(mm)   
Physical_parameters[16,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$D50   # Offshore_sediment_d1_median_grain_size_(mm)
Physical_parameters[17,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$D50   # Offshore_sediment_d2_median_grain_size_(mm)
Physical_parameters[18,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$D50 # Offshore_sediment_d3_median_grain_size_(mm)

Physical_parameters[20,"Value"] <- -0.435                   # Parameter_1_for_relationship_between_porosity_and_grainsize. Values from Matt Pace's thesis
Physical_parameters[21,"Value"] <- 0.302                    # Parameter_2_for_relationship_between_porosity_and_grainsize. The values are also the defaults in the D50_to_porosity function
Physical_parameters[22,"Value"] <- -1.035                   # Parameter_3_for_relationship_between_porosity_and_grainsize
Physical_parameters[23,"Value"] <- -0.314                   # Parameter_4_for_relationship_between_porosity_and_grainsize

Physical_parameters[34,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Porosity       # Defined_porosity_of_inshore_sediment_s1_(muddy)
Physical_parameters[35,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Porosity       # Defined_porosity_of_inshore_sediment_s2_(sandy)
Physical_parameters[36,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Porosity     # Defined_porosity_of_inshore_sediment_s3_gravelly)
Physical_parameters[37,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Porosity      # Defined_porosity_of_offshore_sediment_d1_(muddy)
Physical_parameters[38,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Porosity      # Defined_porosity_of_offshore_sediment_d2_(sandy)
Physical_parameters[39,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Porosity    # Defined_porosity_of_offshore_sediment_d3_(gravelly)

Physical_parameters[41,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Permeability   # Defined_permeability_of_inshore_sediment_s1_(m-2)
Physical_parameters[42,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Permeability   # Defined_permeability_of_inshore_sediment_s2_(m-2)
Physical_parameters[43,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Permeability # Defined_permeability_of_inshore_sediment_s3_(m-2)
Physical_parameters[44,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Permeability  # Defined_permeability_of_offshore_sediment_d1_(m-2)
Physical_parameters[45,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Permeability  # Defined_permeability_of_offshore_sediment_d2_(m-2)
Physical_parameters[46,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Permeability# Defined_permeability_of_offshore_sediment_d3_(m-2)

Physical_parameters[48,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Nitrogen       # Defined_total_N%_of_inshore_sediment_s1_(%DW)
Physical_parameters[49,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Nitrogen       # Defined_total_N%_of_inshore_sediment_s2_(%DW)
Physical_parameters[50,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Nitrogen     # Defined_total_N%_of_inshore_sediment_s3_(%DW)
Physical_parameters[51,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Nitrogen      # Defined_total_N%_of_offshore_sediment_d1_(%DW)
Physical_parameters[52,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Nitrogen      # Defined_total_N%_of_offshore_sediment_d2_(%DW)
Physical_parameters[53,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Nitrogen    # Defined_total_N%_of_offshore_sediment_d3_(%DW)

write.csv(Physical_parameters, file = "./StrathE2E/Models/Barents Sea/2011-2019/Param/physical_parameters_BARENTS_SEA.csv", row.names = F) # Read in example Physical drivers
