  
#### Setup ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("MiMeMo.tools", "sf", "ncdf4", "data.table", "furrr", "raster") # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

domain <- readRDS("./Objects/Domains.rds")

#### NEMO-MEDUSA ####

file <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa/", 
                   recursive = T, full.names = TRUE, pattern = "grid_W")[1]

NM_space <- raster(file, varname = "nav_lat") %>% 
  as.data.frame(xy = T) %>% 
  cbind(Lon = as.data.frame(raster(file, varname = "nav_lon"))) %>% 
  setNames(c("x", "y", "latitude", "longitude")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_join(st_transform(domain, 4326)) %>% 
  drop_na() %>% 
  st_drop_geometry() %>% 
  dplyr::select(x,y)

setDT(NM_space, key = c("x", "y"))

Score <- NM[vertical_diffusivity != 0, .(Deep = mean(vertical_diffusivity > 0.14)), by = time_step] %>% 
  mutate(time_step = as.Date(time_step, format = "%Y%m%d"),
         month = lubridate::month(time_step)) %>%
  group_by(month) %>% 
  summarise(Deep = mean(Deep))

ggplot(Score) + geom_line(aes(x = month, y = Deep))  +
  theme_minimal() +
  labs(y = "Proportion of area subject to deep convection",
       x = "month") +
  xlim(1,12)

ggsave("./Figures/deep convection.png")
