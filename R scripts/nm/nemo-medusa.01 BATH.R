
# Create a spatial grid to bind extracted NM model outputs to, including distance from shore and bathymetry
# Remember to mount the idrive by typing midrive into the Konsole

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "ncdf4", "sf")                                # List handy data packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

Space <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>% 
  .[1] %>%                                                                  # Name an example NM file
  nemomedusR::get_spatial()                                                 # And pull the spatial variables

original <- readRDS("./Objects/Fixed_grid.rds")

#### Get NEMO-MEDUSA bathymetry data ####

NM_bath <- "/mnt/idrive/Science/MS/Shared/CAO/nemo/GRID/allarc_bathy_meter.nc"

raw <- nc_open(NM_bath)
bath_lat <- ncvar_get(raw, varid = "nav_lat")
bath_lon <- ncvar_get(raw, varid = "nav_lon")
bath_bath <- ncvar_get(raw, varid = "Bathymetry")
nc_close(raw)

#### Crop to the MiMeMo files extent ####

tl <- which(bath_lat == Space$nc_lat[1,1] & bath_lon == Space$nc_lon[1,1], arr.ind = TRUE)            # Cut out ALLARC columns which match MiMeMo CROP
tr <- which(bath_lat == Space$nc_lat[1,ncol(Space$nc_lat)] & bath_lon == Space$nc_lon[1,ncol(Space$nc_lon)], arr.ind = TRUE)# Where in the big grid matches each corner of MiMeMo?
bl <- which(bath_lat == Space$nc_lat[nrow(Space$nc_lat),1] & bath_lon == Space$nc_lon[nrow(Space$nc_lon),1], arr.ind = TRUE)  
br <- which(bath_lat == Space$nc_lat[nrow(Space$nc_lat), ncol(Space$nc_lat)] & bath_lon == Space$nc_lon[nrow(Space$nc_lon),ncol(Space$nc_lon)], arr.ind = TRUE)

bath <- bath_bath[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                  tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

lat <- bath_lat[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

lon <- bath_lon[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

#### Further region specific crop to speed up extraction ####

tighter <- lat > 65 & lat < 84 & lon > 6 & lon < 68

E <- min(which(tighter == TRUE, arr.ind = TRUE)[,"row"])
W <- max(which(tighter == TRUE, arr.ind = TRUE)[,"row"])
S <- min(which(tighter == TRUE, arr.ind = TRUE)[,"col"])
N <- max(which(tighter == TRUE, arr.ind = TRUE)[,"col"])

#image(bath[E:W, S:N])   # visual inspection of correct cropping

grid <- setNames(reshape2::melt(lat[E:W, S:N]), c("x", "y", "Latitude")) %>% 
  left_join(setNames(reshape2::melt(lon[E:W, S:N]), c("x", "y", "Longitude"))) %>% 
  left_join(setNames(reshape2::melt(bath[E:W, S:N]), c("x", "y", "Bathymetry"))) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Set dataframe to SF format
  st_transform(crs) 

ggplot(grid) +
  geom_raster(aes(x=x, y=y, fill = Bathymetry))

saveRDS(grid, file = "./Objects/Fixed_grid2.rds")                            # Save
