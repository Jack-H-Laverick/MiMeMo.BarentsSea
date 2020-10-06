
# Extract natural disturbance by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "tidyverse", "sf", "exactextractr", "raster")# List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

habitats <- readRDS("./Objects/Habitats.rds") %>% 
  filter(Habitat != "Rock")

disturbance <- raster::brick("../Sediment/Output/Greenland_and_barents_sea_seasonal_stress.nc") # Import full sediment grid

#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

values <- exact_extract(disturbance, habitats, fun = 'mean')
names(values) <- month.name

result <- cbind(st_drop_geometry(habitats),
                values) %>% 
  pivot_longer(January:December, names_to = "Month", values_to = "Disturbance")


saveRDS(result, "./Objects/Habitat disturbance.rds")

ggplot(result) +
  geom_line(aes(x = as.numeric(as.factor(Month)), y = Disturbance, colour = Shore, linetype = Habitat)) +
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
#  theme(panel.grid.major.x = element_blank(),
#        legend.position = "top") +
#  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Proportion of time disturbed", x = "Month") +
NULL

ggsave("./Figures/sediment/Habitat disturbance.png", width = 16, height = 8, units = "cm")

