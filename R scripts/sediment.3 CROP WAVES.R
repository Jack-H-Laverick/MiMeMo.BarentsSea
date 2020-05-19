
#### Set up ####

rm(list=ls())                                                            # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "fs", "tictoc", "furrr")      # List packages
lapply(packages, library, character.only = TRUE)                         # Load packages

plan(multiprocess)                                                       # Set paralell processing

Waves_mask <- readRDS("./Objects/ECMWF Targets.rds")                     # Import locations of traget pixels in SINMOD grid

all_files <- list.files("./Data/ECMWF_Waves", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                    # Turn the vector into a dataframe
  separate(".", into = c("path", "file"), sep = -7) %>%   
  mutate(year = as.integer(str_sub(file, 1, 4))) %>%                     # Extract the year from the file name
  filter(year > 2002)                                                    # We only need data which overlaps with tides from SINMOD

window <- list(xmin = min(Waves_mask$Longitude),                           # Define x boundaries for crop
               xmax = max(Waves_mask$Longitude),                                                 
               ymin = min(Waves_mask$Latitude),                           # Define y boundaries for crop
               ymax = max(Waves_mask$Latitude))

#### FUNCTION ####

reshape_ECMWF <- function(path, file, window) {
  
  #path <- unique(all_files$path) ; file <- all_files$file[1]               # test
  
  ff_new <- paste0(path, "crop", file)                                      # Create new file name
  
  temp_file1 <- tempfile("dummy", tmpdir = path)
  temp_file2 <- tempfile("dummy", tmpdir = path)
    
  ## Run command line functions
  # str_glue and {} allows us to programmatically build character strings to run in Konsole.
  # Clip longitude and latitude dimensions (names specified in the file), from a file, saving result as new file.
  system(stringr::str_glue("ncea -d longitude,{window$xmin},{window$xmax} -d latitude,{window$ymin},{window$ymax} {paste0(path,file)} {temp_file1}"))
  
  # Average over ensemble members (number), from a file, saving result
  system(stringr::str_glue("ncwa -a number {temp_file1} {temp_file2}"))
    
  file.rename(temp_file2, ff_new)                                       # Name sensibly
    
  unlink(temp_file1)                                                    # Delete the intermediate step
    
  usethis::ui_done("{usethis::ui_field(file)} cropped.")      # Announce finished file
  
}
#### Subset files ####

tic()
future_map(all_files$file, reshape_ECMWF, path = unique(all_files$path), window = window)
toc()

## Concatenate files
# Concatenate along time dimension from the start, from SINMOD_blah.nc files, into a tides file
system(str_glue("ncrcat -d time,0, ./Data/ECMWF_Waves/crop????.nc ./Objects/waves.nc")) 

## Delete redundant subsets
cleaning <- list.files(unique(all_files$path), full.names = TRUE) %>%    # What do we have now?
  .[str_detect(., "crop")] %>%                            # list only the cropped files
  unlink()                                                # Delete