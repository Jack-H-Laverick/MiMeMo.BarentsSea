
# Extract Permeability, Porosity, and Nitrogen content by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster")                   # List packages
lapply(Packages, library, character.only = TRUE)                           # Load packages
source("./R scripts/@_Region file.R")

habitats <- readRDS("./Objects/Habitats.rds") %>%                          # Import maps of sea bed habitats
  filter(Habitat != "Rock")

Nitrogen <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "TON") # Import organic matter content
Porosity <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "Porosity") # Import porosity
Permeability <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "Permeability") # Import permeability
Dxbar <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "Dxbar")      # Import permeability

#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

result <- st_drop_geometry(habitats) %>% 
  mutate(Nitrogen = exact_extract(Nitrogen, habitats, fun = 'mean'),       # Mean of each variable per habitat
         Porosity = exact_extract(Porosity, habitats, fun = 'mean'),
         Permeability = exact_extract(Permeability, habitats, fun = 'mean'),
         D50 = exact_extract(Dxbar, habitats, fun = 'mean')) 

saveRDS(result, "./Objects/Other habitat parameters.rds")
