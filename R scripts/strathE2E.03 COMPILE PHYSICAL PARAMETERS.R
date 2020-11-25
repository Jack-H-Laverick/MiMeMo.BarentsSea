
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

My_sediment <- readRDS("./Objects/Sediment area proportions.rds")

Physical_parameters[1,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "S")$Elevation * -1 # Offshore_Shallow_layer_thickness_(m)
Physical_parameters[2,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "D")$Elevation * -1 # Offshore_Deep_layer_thickness_(m)
Physical_parameters[3,"Value"] <- filter(My_space, Shore == "Inshore", Depth == "S")$Elevation * -1  # Inshore_Shallow_layer_thickness_(m)

Physical_parameters[5,"Value"] <- filter(My_sediment, Shore == "Inshore", Bottom == "Rock")$Cover    # Area_proportion_of_inshore_rock_habitat_s0_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[6,"Value"] <- filter(My_sediment, Shore == "Inshore", Bottom == "Silt")$Cover    # Area_proportion_of_inshore_sediment_habitat_s1_(muddy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[7,"Value"] <- filter(My_sediment, Shore == "Inshore", Bottom == "Sand")$Cover    # Area_proportion_of_inshore_sediment_habitat_s2_(sandy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[8,"Value"] <- filter(My_sediment, Shore == "Inshore", Bottom == "Gravel")$Cover  # Area_proportion_of_inshore_sediment_habitat_s3_(gravelly)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[9,"Value"] <- filter(My_sediment, Shore == "Offshore", Bottom == "Rock")$Cover   # Area_proportion_of_offshore_rock_habitat_d0_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[10,"Value"] <- filter(My_sediment, Shore == "Offshore", Bottom == "Silt")$Cover  # Area_proportion_of_offshore_sediment_habitat_d1_(muddy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[11,"Value"] <- filter(My_sediment, Shore == "Offshore", Bottom == "Sand")$Cover  # Area_proportion_of_offshore_sediment_habitat_d2_(sandy)_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[12,"Value"] <- filter(My_sediment, Shore == "Offshore", Bottom == "Gravel")$Cover# Area_proportion_of_offshore_sediment_habitat_d3_(gravelly)_(sum_of_all_8_habitat_areas_must=1)

Physical_parameters[13,"Value"] <- mean(c(0.0625, 0.00098)) # Inshore_sediment_s1_median_grain_size_(mm)_(muddy_but_if_set_to_0=rock) mud
Physical_parameters[14,"Value"] <- mean(c(2,0.0625))        # Inshore_sediment_s2_median_grain_size_(mm)_(sandy_but_if_set_to_0=rock) sand
Physical_parameters[15,"Value"] <- 2                        # Inshore_sediment_s3_median_grain_size_(mm)_(gravelly_but_if_set_to_0=rock) gravel
Physical_parameters[16,"Value"] <- mean(c(0.0625, 0.00098)) # Offshore_sediment_d1_median_grain_size_(mm)_(muddy_but_if_set_to_0=rock) mud
Physical_parameters[17,"Value"] <- mean(c(2,0.0625))        # Offshore_sediment_d2_median_grain_size_(mm)_(sandy_but_if_set_to_0=rock) sand
Physical_parameters[18,"Value"] <- 2                        # Offshore_sediment_d3_median_grain_size_(mm)_(gravelly_but_if_set_to_0=rock) gravel

write.csv(Physical_parameters, file = "./StrathE2E/Models/Barents Sea/2011-2019/Param/physical_parameters_BARENTS_SEA.csv", row.names = F) # Read in example Physical drivers
unlink("./StrathE2E/Models/Barents Sea/2011-2019/Param/physical_parameters_NORTH_SEA.csv") # Rename file
