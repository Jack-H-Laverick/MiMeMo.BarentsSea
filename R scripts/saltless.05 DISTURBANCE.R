
# Extract natural disturbance by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "tidyverse", "sf", "exactextractr", "raster")# List packages
lapply(Packages, library, character.only = TRUE)                           # Load packages
source("./R scripts/@_Region file.R")

habitats <- readRDS("./Objects/Habitats.rds") %>%                          # Import maps of sea bed habitats
  filter(Habitat != "Rock")

disturbance <- raster::brick("../Sediment/Output/Greenland_and_barents_sea_seasonal_stress.nc") # Import seasonal disturbance

#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

values <- exact_extract(disturbance, habitats, fun = 'mean')               # Mean habitat disturbance
names(values) <- month.name                                                # Put months as column names

result <- cbind(st_drop_geometry(habitats),                                # Bind to map metadata
                values) %>% 
  pivot_longer(January:December, names_to = "Month", values_to = "Disturbance") # Reshape dataframe to long format

saveRDS(result, "./Objects/Habitat disturbance.rds")

ggplot(result) +
  geom_line(aes(x = as.numeric(as.factor(Month)), y = Disturbance, colour = Shore, linetype = Habitat)) +
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  labs(y = "Proportion of time disturbed", x = "Month") +
NULL

ggsave("./Figures/sediment/Habitat disturbance.png", width = 16, height = 8, units = "cm")
