
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

library(tidyverse)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
# "./R scripts/bathymetry.01 DATA WRANGLING.R",                 
# "./R scripts/bathymetry.02 PLOTTING.R",                       
# "./R scripts/bathymetry.03 DOMAIN CHOICES.R",                 
# "./R scripts/bathymetry.04 DEFINE DOMAIN.R",                  
# 
# "./R scripts/nemo-medusa.01 BATH.R",                          
# "./R scripts/nemo-medusa.02 EXTRACTION.R",                    
# "./R scripts/nemo-medusa.03 SPATIAL.R",                       
# "./R scripts/nemo-medusa.04 TIME SERIES.R",                   
# "./R scripts/nemo-medusa.05 PLOTTING.R",                      
# 
# #"./R scripts/flows.01 VC-EXTRACTION.R",                       
# "./R scripts/flows.02 MAKE TRANSECTS.R",                      
# "./R scripts/flows.03 LABEL TRANSECTS.R",                     
# "./R scripts/flows.04 SAMPLE PERIMETER.R",                    
# "./R scripts/flows.05 SAMPLE FLUXES.R",                       
# #"./R scripts/flows.06 VOLUME CHECK.R",                        
# "./R scripts/flows.07 PLOT EXCHANGES.R",                      
# "./R scripts/flows.08 WAVES.R",                               
# 
# "./R scripts/saltless.01 ATMOSPHERE.R",                       
# "./R scripts/saltless.02 RIVER VOLUMES.R",                    
# "./R scripts/saltless.03 RIVER N CONCENTRATION.R",            
# "./R scripts/saltless.04 RIVER DIN FIX.R",                    
# "./R scripts/saltless.05 SPM.R",                              
# "./R scripts/saltless.06 HABITAT TYPES.R",                    
# "./R scripts/saltless.07 DISTURBANCE.R",                      
# "./R scripts/saltless.08 OTHER SEDIMENT SUMMARIES.R",         
# "./R scripts/saltless.09 LIGHT AND TEMP.R",                
# 
# #"./R scripts/fish.0 MiMeMo GUILDS.R",                         
# "./R scripts/fish.01 FAO REGIONS.R",                          
# #"./R scripts/fish.02 ICES.R",                                 
# #"./R scripts/fish.03 GFW WRANGLING.R",                        
# #"./R scripts/fish.04 GFW FRESH FIGURE.R",                     
# #"./R scripts/fish.05 GFW PLOTTING.R",                         
# #"./R scripts/fish.06 IMR TABLES.R",                           
# #"./R scripts/fish.07 IMR GUILDS.R",                           
# #"./R scripts/fish.08 GFW TO NETCDF.R",                        
 "./R scripts/fish.09 GFW EFFORT PROPORTIONS.R",               
 "./R scripts/fish.10 ICES LANDINGS PROPORTIONS.R",            
 "./R scripts/fish.11 IMR TRENDS.R",                           
 "./R scripts/fish.12 IMR EFFORT AND LANDINGS.R",              
 "./R scripts/fish.13 EU LANDINGS.R",                          
 "./R scripts/fish.14 INTERNATIONAL LANDINGS.R",               
 "./R scripts/fish.15 EU EFFORT.R",                            
 "./R scripts/fish.16 INTERNATIONAL EFFORT.R",                 
 "./R scripts/fish.17 IMR HABITAT EFFORTS.R",                  
 "./R scripts/fish.18 EU HABITAT EFFORTS.R",                   
 "./R scripts/fish.19 INTERNATIONAL HABITAT EFFORT.R"         
# 
# ##"./R scripts/strathE2E.00 QUICKSTART.R",                      
# #"./R scripts/strathE2E.01 DIN FIX.R",                         
# #"./R scripts/strathE2E.01 INITIALISE MODEL.R",                
# #"./R scripts/strathE2E.02 COMPILE BOUNDARY FILE.R",           
# #"./R scripts/strathE2E.03 COMPILE PHYSICS FILE.R",            
# #"./R scripts/strathE2E.04 COMPILE PHYSICAL PARAMETERS.R"     
# ##"./R scripts/strathE2E.05 MODEL CLEANUP.R"                   
) %>% 
  MiMeMo.tools::map(execute)                                                           # Run the scripts

#### Plot run times ####

timings <- tictoc::tic.log(format = F) %>%                                             # Get the log of timings
  lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
  bind_rows() %>%                                                                      # Get a single dataframe
  separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
  separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
  mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
saveRDS(timings, "./Objects/Run time.rds")

source("./R scripts/@_Script runtimes.R")                                              # Plot run times
