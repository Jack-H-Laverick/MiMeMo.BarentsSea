
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

library(tidyverse)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
# "./R scripts/bathymetry/bathymetry.01 DATA WRANGLING.R",
# "./R scripts/bathymetry/bathymetry.02 PLOTTING.R",
# "./R scripts/bathymetry/bathymetry.03 DEFINE DOMAIN.R",
# 
# "./R scripts/nm/nemo-medusa.01 BATH.R",
# "./R scripts/nm/nemo-medusa.02 EXTRACTION.R",
# "./R scripts/nm/nemo-medusa.03 V EXTRACTION.R",
# "./R scripts/nm/nemo-medusa.04 SPATIAL.R",
# "./R scripts/nm/nemo-medusa.05 TIME SERIES.R",
# "./R scripts/nm/nemo-medusa.06 PLOTTING.R",
# 
# "./R scripts/flows/flows.01 VERTICAL DIFFUSIVITY.R",
# "./R scripts/flows/flows.02 WAVES.R",
# "./R scripts/flows/flows.03 MAKE TRANSECTS.R",
# "./R scripts/flows/flows.04 LABEL TRANSECTS.R",
# "./R scripts/flows/flows.05 SAMPLE TRANSECTS.R",
# "./R scripts/flows/flows.06 PLOT EXCHANGES.R",
# 
# "./R scripts/saltless/saltless.01 ATMOSPHERE.R",
# "./R scripts/saltless/saltless.02 LIGHT AND TEMP.R",
# "./R scripts/saltless/saltless.03 SPM.R",
# "./R scripts/saltless/saltless.04 RIVER VOLUMES.R",
# "./R scripts/saltless/saltless.05 RIVER N CONCENTRATION.R",
# "./R scripts/saltless/saltless.06 RIVER DIN FIX.R",
# "./R scripts/saltless/saltless.07 HABITAT TYPES.R",
# "./R scripts/saltless/saltless.08 DISTURBANCE.R",
# "./R scripts/saltless/saltless.09 OTHER SEDIMENT SUMMARIES.R",

#"./R scripts/fish/fish.0 MiMeMo GUILDS.R",
#"./R scripts/fish/fish.01 FAO REGIONS.R",
#"./R scripts/fish/fish.02 ICES.R",
#"./R scripts/fish/fish.03 GFW WRANGLING.R",
#"./R scripts/fish/fish.04 GFW FRESH FIGURE.R",
#"./R scripts/fish/fish.05 GFW PLOTTING.R",
#"./R scripts/fish/fish.06 IMR TABLES.R",
#"./R scripts/fish/fish.07 IMR GUILDS.R",
#"./R scripts/fish/fish.08 GFW TO NETCDF.R",
#"./R scripts/fish/fish.09 GFW EFFORT PROPORTIONS.R",
"./R scripts/fish/fish.10 IMR EFFORT AND LANDINGS.R",
"./R scripts/fish/fish.11 EU LANDINGS.R",
"./R scripts/fish/fish.12 RAFISKLAG LANDINGS.R",
"./R scripts/fish/fish.13 ICES LANDINGS PROPORTIONS.R",
"./R scripts/fish/fish.14 INTERNATIONAL LANDINGS.R",
"./R scripts/fish/fish.15 EU EFFORT.R",
"./R scripts/fish/fish.16 INTERNATIONAL EFFORT.R",
"./R scripts/fish/fish.17 IMR HABITAT EFFORTS.R",
"./R scripts/fish/fish.18 EU HABITAT EFFORTS.R",
"./R scripts/fish/fish.19 SEAL HABITAT EFFORTS.R",
"./R scripts/fish/fish.20 INTERNATIONAL HABITAT EFFORT.R",
"./R scripts/fish/fish.21 DISCARDS EU.R",

# "./R scripts/se2e/strathE2E.01 DIN FIX.R",
# "./R scripts/se2e/strathE2E.01 INITIALISE MODEL.R",
# "./R scripts/se2e/strathE2E.02 COMPILE BOUNDARY FILE.R",
# "./R scripts/se2e/strathE2E.03 COMPILE PHYSICS FILE.R",            
# "./R scripts/se2e/strathE2E.04 COMPILE PHYSICAL PARAMETERS.R",     
 "./R scripts/se2e/strathE2E.04 COMPILE FISHING FLEET.R"    
# "./R scripts/se2e/strathE2E.05 MODEL CLEANUP.R",
# "./R scripts/se2e/strathE2E.06 UPDATE PERIODS.R"                   
) %>% 
  map(MiMeMo.tools::execute)                                                           # Run the scripts

#### Plot run times ####

timings <- tictoc::tic.log(format = F) %>%                                             # Get the log of timings
  lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
  bind_rows() %>%                                                                      # Get a single dataframe
  separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
  separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
  mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
saveRDS(timings, "./Objects/Run time.rds")

source("./R scripts/@_Script runtimes.R")                                              # Plot run times
