
# Extract sediment characteristics at CHAOSE sampling location

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "raster", "ncdf4")                            # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

file <- "../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc"  

raw <- nc_open(file)                                                        # Open file
vars <- names(raw[["var"]]) %>%                                             # Get variable names
  .[-1]                                                                     # Drop crs as a variable
nc_close(raw)

samples <- data.frame(Site = c("B13", "B14", "B15", "B16", "B17"),
                      lon = c(30.0003, 30.287, 30.0007, 29.916, 29.5066),
                      lat = c(74.4666, 76.4994, 78.2143, 80.1521, 81.4018))

data <- map_df(vars, ~{                                                  # For each variable
  data.frame(Site = samples$Site,
             Var = .x,                                                      # The variable name
             Value = raster(file, varname = .x) %>%                          
               extract(samples[,2:3]))                                      # And the summary statistic
}) %>% 
  pivot_wider(names_from =Var, values_from = Value)                         # Switch to wide for easier reading.

