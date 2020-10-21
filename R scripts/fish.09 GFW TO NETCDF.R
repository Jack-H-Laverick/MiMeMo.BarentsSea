
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("MiMeMo.tools", "data.table", "furrr", "tictoc", "raster")      # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

plan(multiprocess)                                                            # Choose the method to parallelise by with furrr

Files <- list.files(path = "./Data/GFW daily_csvs", pattern ="*.csv", full.names = T) # Get a list of files to import 

target <- raster(xmn = 0, xmx = 100, ymn = 65, ymx = 85, res = c(0.01, 0.01), vals = NA)

#### Limit to the obervations of interest ###

Effort <- future_map(Files, ~ {                                               # For each csv
   
  fread(.x) %>%                                                               # Read in
    mutate(lat_bin = lat_bin/100,                                             # Convert the bins to latitude and longitude
           lon_bin = lon_bin/100,                                                           
           geartype = case_when(geartype %in% c("drifting_longlines", "fixed_gear", "other_fishing") ~ "static_gear",
                                geartype %in% c("trawlers", "purse_seines") ~ "mobile_gear"), # Classify gears
           flag = ifelse(!flag %in% c("NOR", "RUS"), "REST", flag)) %>%       # Simplify flags of interest
    filter(between(lat_bin, 65, 85),                                          # Rough crop to region
           between(lon_bin, 0, 100)) %>%
    group_by(lat_bin, lon_bin, flag, geartype, date) %>% 
    summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = geartype, values_from = fishing_hours) %>% 
    as.data.frame()                                                             # Convert to data frame to play nicely with rasters
    },          # Get each fishing gear as a column
    .progress = TRUE) %>%
  rbindlist(fill = TRUE)

setDT(Effort)                                                                 # Convert to data.table for speed

Effort <- Effort[, Year := year(date)][                                      # Extract year from date column
  , lapply(.SD, sum, na.rm = TRUE),                                           # Sum values
  by = .(lon_bin, lat_bin, flag, Year),                                       # By pixel, nation, gear, and year
  .SDcols = c("mobile_gear", "static_gear")] %>%                              # For each gear 
  as.data.frame()                                                             # Convert to data frame to play nicely with rasters

#### Make rasters ####

vars <- c("mobile_gear", "static_gear")

tic()
map(c("NOR", "RUS", "REST"), ~{                                               # For each flag
  
  data <- filter(Effort, flag == .x)                                          # Limit to one in turn

  print(glue::glue("Now processing {.x}"))                                    # Keeping track
  
  new <- map(vars, ~ {                                                        # Then for each fishing variable
    dplyr::select(data, c("lon_bin", "lat_bin", "Year", all_of(.x))) %>%      # Select it along with spatial and temporal variables
    split(f = .$Year) %>%                                                     # Split by year so we get one raster per time step
    map(function(x, target, var) {                                            # For each year
    
      raster <- target                                                        # Take the target grid
      cells <- cellFromXY(target, x[,c("lon_bin", "lat_bin")])                # Find which cells on the grid our observations fall on
      raster[cells] <- x[,var]                                                # Copy over the data
      return(raster)},                                                        # Return an updated raster
    target = target, var = .x) %>%                                            # Specify the target raster
    brick() }, data = data)                                                   # And bind each year into a raster brick of one variable for one flag
  
  print(glue::glue("Now saving {.x}"))                                        # Keeping track
  
  future_map2(new, paste0(.x, "_", vars), ~ {                                 # Then save to netcdf
    writeRaster(.x, filename = paste0("./Data/GFW daily_csvs/", .y, ".nc"), overwrite = TRUE, # Building a name from flag and variable
    format = "CDF", varname= .y, varunit = "Hours", xname = "Longitude",
    yname="Latitude", zname = "Year") }, .progress = T)

})
toc()

#### Combine all files ####

netcdfs <- list.files(path = "./Data/GFW daily_csvs", pattern ="*.nc", full.names = T) # Get a list of files to import 

file.rename(from = netcdfs[1], to = "./Objects/GFW.nc")

walk(netcdfs[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW.nc"))                         # In turn bind a variable to the main file
})                                                                            # Bind together all the netcdf files

unlink(netcdfs[-1])                                                           # Delete the redundant files

#### faster merges? ###

# tic()
# system(str_glue("ncrcat {'netcdfs'}./Objects/GFW_ncrcat.nc"))                        # In turn bind a variable to the main file
# toc()
# 
# tic()
# system("ncecat './Data/GFW daily_csvs/*.nc' ./Objects/GFW_ncecat.nc")                        # In turn bind a variable to the main file
# toc()

#### Extracting a domain wide summary ####

library(exactextractr)
 
check <- brick("./Objects/GFW.nc")
plot(check)
 
domain <- readRDS("./Objects/Domains.rds")
 
exact_extract(check, domain, fun = "sum")
