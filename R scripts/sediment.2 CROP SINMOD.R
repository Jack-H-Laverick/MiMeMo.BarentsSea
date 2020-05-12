
#### Set up ####

rm(list=ls())                                                            # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "fs", "tictoc", "furrr")      # List packages
lapply(packages, library, character.only = TRUE)                         # Load packages

plan(multiprocess)                                                       # Set paralell processing

SINMOD_mask <- readRDS("./Objects/SINMOD Targets.rds")                   # Import locations of traget pixels in SINMOD grid

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as_tibble() %>%                                                        # Turn the vector into a dataframe/tibble
  separate(value, into = c("path", "file"), sep = "_") %>%               # Extract the year and month from the file name
  separate(file, into = c("year", NA),  
           remove = FALSE, sep = 4) %>%                                  # Extract the year and month from the file name
  mutate(path = paste0(path, "_"),                                       # Replace the dropped separator
         year = as.integer(year)) %>%                                    # Set time as integers 
  filter(!file %in% c("200301.nc", "ncdump") &                           # First time step isn't accessible, and don't want the dump file
           year < 2011)                                                  # We only need data which overlaps with waves heights from ECMWF

window <- list(xmin = min(SINMOD_mask$V1),                               # Define x boundaries for crop
               xmax = max(SINMOD_mask$V1),                                                 
               ymin = min(SINMOD_mask$V2),                               # Define y boundaries for crop
               ymax = max(SINMOD_mask$V2))

out_dir <- "./Data/SINMOD/"                                              # Define location to save new files

#### Subset files ####

## Subset files
tic()
future_map(all_files$file, reshape_SINMOD, path = unique(all_files$path), out_dir = out_dir, window = window)
toc()

## Concatenate files
# Concatenate along time dimension from the start, from SINMOD_blah.nc files, into a tides file
system(str_glue("ncrcat -d time,0, ./Data/SINMOD/SINMOD_??????.nc ./Data/SINMOD/tides.nc")) 

## Delete redudant subsets
cleaning <- list.files(out_dir, full.names = TRUE) %>%    # What did we create?
  .[!str_detect(., "tides.nc")] %>%                       # Drop the composite file
  unlink()                                                # Delete all the other files
