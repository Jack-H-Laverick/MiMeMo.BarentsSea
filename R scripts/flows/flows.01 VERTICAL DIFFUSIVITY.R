
#### Setup ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "data.table")                                    # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

deep_convection_is <- 0.14                                                  # Threshold above which vertical diffusivity = deep convection
deep_convection_threshold <- 0.7                                            # How much deep convection means we should trigger total mixing?
deep_convection_overwrite <- 0.14                                           # What value do we want to represent total mixing in StrathE2E

#### Quantify the amount of deep convection ####

total_mixing <- list.files("./Objects/vertical boundary/", full.names = T) %>% 
  future_map(readRDS) %>% 
  rbindlist() %>% 
  group_by(Month) %>% 
  summarise(Deep_convection_proportion = mean(Vertical_diffusivity > deep_convection_is))

ggplot(total_mixing) +
  geom_line(aes(x = Month, y = Deep_convection_proportion)) +
  theme_minimal() +
  ylim(0,1) +
  labs(y = "Proportion of model domain as deep convection")

#### Mean vertical diffusivity ignoring deep convection ####

normal_mixing <- list.files("./Objects/vertical boundary/", full.names = T) %>% 
  future_map(readRDS) %>% 
  rbindlist() %>% 
  filter(Vertical_diffusivity < deep_convection_is) %>% 
  group_by(Month) %>% 
  summarise(Vertical_diffusivity = mean(Vertical_diffusivity))

ggplot(normal_mixing) +
  geom_line(aes(x = Month, y = Vertical_diffusivity)) +
  theme_minimal() +
  labs(y = "Mean vertical diffusivity (ignoring deep convection)")

#### Modified ####

modified_mixing <- left_join(total_mixing, normal_mixing) %>% 
  mutate(Vertical_diffusivity = if_else(Deep_convection_proportion > deep_convection_threshold, # Overwrite diffusivity if there's too much deep convection
                                        deep_convection_overwrite, Vertical_diffusivity))

ggplot(modified_mixing) +
  geom_line(aes(x = Month, y = Vertical_diffusivity)) +
  theme_minimal() +
  labs(y = "Modified vertical diffusivity")

