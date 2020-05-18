
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

#### Set up ####

packages <- c("tidyverse", "callr", "tictoc")           # List packages
lapply(packages, library, character.only = TRUE)        # Load packages

execute <- function(x) {
  tic(x)
  r(function(x) source(x), args = list(x), spinner = TRUE)
  toc(log = T, quiet = T)

  usethis::ui_done("{usethis::ui_field(x)} completed. {praise::praise('${Exclamation}!')}")  } # Run an R script in it's own R session and record how long it takes

#### Batch process scripts ####

scripts <- c(                                            # List scripts in the order you want to run them
    "./R scripts/bathymetry.1 DATA WRANGLING.R",
    "./R scripts/bathymetry.2 PLOTTING.R",
    "./R scripts/bathymetry.3 RAYSHADER.R",
    "./R scripts/bathymetry.4 DOMAIN CHOICES.R",
    "./R scripts/bathymetry.5 DEFINE DOMAIN.R",
   
    "./R scripts/NM.1 GRID.R",
    "./R scripts/NM.2 EXTRACTION.R",
    "./R scripts/NM.3 SPATIAL.R",
    "./R scripts/NM.4 TIME SERIES.R",
    "./R scripts/NM.5 PLOTTING.R",
    "./R scripts/NM.6 LIGHT AND TEMP.R",

   "./R scripts/detritus.1 GRID.R",
   "./R scripts/detritus.2 EXTRACTION.R",
###"./R scripts/detritus.3 SPATIAL.R",
   "./R scripts/detritus.4 TIME SERIES.R",             
   "./R scripts/detritus.5 PLOTTING.R",
  
   "./R scripts/flows.1 VC-EXTRACTION.R",
   "./R scripts/flows.2 MAKE TRANSECTS.R",
   "./R scripts/flows.3 LABEL TRANSECTS.R",
##   "./R scripts/flows.4 SAMPLE PERIMETER.R",
##   "./R scripts/flows.5 SAMPLE FLUXES.R",
#  "./R scripts/flows.6 VOLUME CHECK.R",
#   "./R scripts/flows.7 PLOT EXCHANGES.R",
  
   "./R scripts/saltless.1 ATMOSPHERE.R",

   "./R scripts/spm.1 EXTRACTION.R",

   "./R scripts/fish.1 FAO REGIONS.R",
###"./R scripts/fish.2 ICES.R",
   "./R scripts/fish.3 GFW WRANGLING.R",
###  "./R scripts/fish.4 GFW FRESH FIGURE.R",
###"./R scripts/fish.5 GFW PLOTTING.R",

 # "./R scripts/sediment.1 TIDES.R",
 # "./R scripts/sediment.2 WAVES.R",
   "./R scripts/sediment.1 GRID WATER.R",
   "./R scripts/sediment.2 CROP SINMOD.R", 
   "./R scripts/sediment.2 CROP WAVES.R",
   "./R scripts/sediment.3 WATER TS.R",
   "./R scripts/sediment.4 SHEARSTRESS.R",
   "./R scripts/sediment.4 GRID.R",
   "./R scripts/sediment.5 RANDOM FOREST.R"
) %>% 
  map(execute)

#### Plot run times ####

timings <- tic.log(format = F) %>%                                                     # Get the log of timings
  lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
  bind_rows() %>%                                                                      # Get a single dataframe
  separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
  separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
  mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
saveRDS(timings, "./Objects/Run time.rds")

source("./R scripts/@_Script runtimes.R")                                              # Plot run times

   