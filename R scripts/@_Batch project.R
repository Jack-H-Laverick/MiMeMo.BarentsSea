
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
    "./R scripts/bathymetry.01 DATA WRANGLING.R",
    "./R scripts/bathymetry.02 PLOTTING.R",
    "./R scripts/bathymetry.03 DOMAIN CHOICES.R",
    "./R scripts/bathymetry.04 DEFINE DOMAIN.R",
    
    "./R scripts/nemo-medusa.01 GRID.R",
    "./R scripts/nemo-medusa.02 EXTRACTION.R",
    "./R scripts/nemo-medusa.03 SPATIAL.R",
    "./R scripts/nemo-medusa.04 TIME SERIES.R",
    "./R scripts/nemo-medusa.05 PLOTTING.R",
    "./R scripts/nemo-medusa.06 LIGHT AND TEMP.R",
 
   # "./R scripts/flows.01 VC-EXTRACTION.R",
    "./R scripts/flows.02 MAKE TRANSECTS.R",
    "./R scripts/flows.03 LABEL TRANSECTS.R",
    "./R scripts/flows.04 SAMPLE PERIMETER.R",
   # "./R scripts/flows.05 SAMPLE FLUXES.R",
   # "./R scripts/flows.06 VOLUME CHECK.R",
    "./R scripts/flows.07 PLOT EXCHANGES.R",
    "./R scripts/flows.08 WAVES.R",
  
    "./R scripts/saltless.01 ATMOSPHERE.R",
   # "./R scripts/saltless.02 RIVERS.R",
    "./R scripts/saltless.03 SPM.R",
    "./R scripts/saltless.04 HABITAT TYPES.R",
    "./R scripts/saltless.04 DISTURBANCE.R",
   
    "./R scripts/fish.01 FAO REGIONS.R",
###"./R scripts/fish.02 ICES.R",
   "./R scripts/fish.03 GFW WRANGLING.R",
###  "./R scripts/fish.04 GFW FRESH FIGURE.R",
###"./R scripts/fish.05 GFW PLOTTING.R",
   "./R scripts/fish.06 IMR TABLES.R"
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

   