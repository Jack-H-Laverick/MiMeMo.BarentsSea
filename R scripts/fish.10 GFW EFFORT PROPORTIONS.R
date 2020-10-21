
# Calculate the proportion of fishing effort by gear and flag across habitat types according to global fishing watch data

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr")                # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

plan(multiprocess)

habitats <- readRDS("./Objects/Habitats.rds")                                 # Load habitat polygons

#### Extracting a domain wide summary ####

Proportion_effort <- c("NOR_mobile_gear", "NOR_static_gear",                  # For each variable in the GFW file
          "RUS_mobile_gear", "RUS_static_gear",
          "REST_mobile_gear", "REST_static_gear") %>% 
  future_map(~{ brick("./Objects/GFW.nc", varname = .x) %>%                   # Import a brick of all years
         exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
         mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
         cbind(st_drop_geometry(habitats))}) %>%                              # Attach habitat metadata
  rbindlist() %>% 
  pivot_longer(starts_with("sum"), names_to = "Year", values_to = "Hours") %>%# Reshape so all years are in a column
  separate(Variable, into = c("Flag", "Gear"), sep = "_") %>%                 # Split variable name into flag and gear type
  mutate(Year = as.numeric(str_remove(Year, "sum.X")) + 2011) %>%                  # Fix year column
  group_by(Year) %>%                                                          # Within a year 
  mutate(Proportion = Hours/sum(Hours)) %>%                                   # Calculate the proportion of fihing effort in each row
  ungroup()

ggplot(Proportion_effort) +
  geom_path(aes(x = Year, y = Proportion, colour = paste0(Shore, " ", Habitat))) +
  facet_grid(rows = vars(Flag), cols = vars(Gear))

## Ignoring habitat types
  
Flag <- group_by(Proportion_effort, Flag, Gear, Year) %>% 
  summarise(Proportion = sum(Proportion)) %>% 
  ungroup()

ggplot(Flag) +
  geom_col(aes(x = Flag, y = Proportion, fill = Flag), position = "dodge2") +
  geom_text(data = filter(Flag, Flag == "NOR"), aes(x = Flag, y = 0, group = Year, label = Year), 
            position = position_dodge(0.9), angle = 90,  colour = "firebrick4", fontface = "bold", hjust = 0,
            family = "AvantGarde") +
  labs(y = "Proportion of total fishing hours") +
  facet_grid(rows = vars(Gear)) +
  theme_minimal() +
  theme(legend.position = "none")


