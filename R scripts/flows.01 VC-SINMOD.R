  
#### Setup ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("MiMeMo.tools", "sf", "ncdf4", "data.table", "furrr", "raster") # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

domain <- readRDS("./Objects/Domains.rds")

#### Get the names of vertical current files ####

file <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD/", 
                     recursive = F, full.names = TRUE, pattern = "[.]nc")[2]
  
SINMOD_space <- raster(file, varname = "gridLats") %>% 
  as.data.frame(xy = T) %>% 
  cbind(Lon = as.data.frame(raster(file, varname = "gridLons"))) %>% 
  setNames(c("x", "y", "latitude", "longitude")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_join(st_transform(domain, 4326)) %>% 
  drop_na() %>% 
  st_drop_geometry() %>% 
  dplyr::select(x,y)

setDT(SINMOD_space, key = c("x", "y"))
  
tictoc::tic()
SINMOD <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD/", 
                        recursive = F, full.names = TRUE, pattern = "[.]nc") %>%
  .[-1] %>% 
  future_map(~{
    raster(.x, level = 1, varname = "vertical_diffusivity") %>%  # Depth is a level (4th dimension)
    as.data.frame(na.rm = T, xy = T)}, .progress = T) %>% 
    rbindlist()

setDT(SINMOD, key = c("x", "y"))

SINMOD <- SINMOD[SINMOD_space]                                                # Drop points which don't match the space object 

tictoc::toc()

#### NEMO-MEDUSA ####

file <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa/", 
                   recursive = T, full.names = TRUE, pattern = "grid_W")[1]

NM_space <- raster(file, varname = "nav_lat") %>% 
  as.data.frame(xy = T) %>% 
  cbind(Lon = as.data.frame(raster(file, varname = "nav_lon"))) %>% 
  setNames(c("x", "y", "latitude", "longitude")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_join(st_transform(domain, 4326)) %>% 
  drop_na() %>% 
  st_drop_geometry() %>% 
  dplyr::select(x,y)

setDT(NM_space, key = c("x", "y"))

tictoc::tic()
NM <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa/", 
                        recursive = T, full.names = TRUE, pattern = "grid_W") %>%
  future_map(~{
    raster(.x, band = 1, varname = "votkeavt") %>%           # Depth is a band (3rd dimension)
      as.data.frame(na.rm = T, xy = T)}, .progress = T) %>% 
  rbindlist() %>% 
  setNames(c("x", "y", "vertical_diffusivity"))

setDT(NM, key = c("x", "y"))

NM <- NM[NM_space]                                                # Drop points which don't match the space object 
tictoc::toc()

ggplot() +
  geom_density(data = NM, aes(vertical_diffusivity), fill = "orange") +
  geom_density(data = SINMOD, aes(vertical_diffusivity), fill = "blue") +
  geom_density(data = NM, aes(vertical_diffusivity), fill = "orange", alpha = 0.6) +
  geom_density(data = SINMOD, aes(vertical_diffusivity)) +
  ggtext::geom_richtext(aes(x = 0.053, y = 47), fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"),
                        hjust = 0, vjust = 1, size = 4, 
               label = str_glue("<b style = 'color:orange'>NEMO-MEDUSA:</b> 
               mean = {round(mean(NM$vertical_diffusivity, na.rm = T), 3)}<br>
               <b style = 'color:blue'>SINMOD:</b> mean = {round(mean(SINMOD$vertical_diffusivity, na.rm = T), 3)}")) +
  theme_minimal() +
  theme(plot.caption = ggtext::element_markdown()) +
  labs(caption = "Density distribution of vertical diffusivity values for
       <b style = 'color:orange'>NEMO-MEDUSA</b> and <b style = 'color:blue'>SINMOD</i></b>
       model outputs",
       x= "Vertical diffusivity") + 
   NULL

#ggsave("./Figures/diffusivity comparison.png", )
  
#### Find high values ####

file <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa/", 
                   recursive = T, full.names = TRUE, pattern = "grid_W")[1]

check <- raster(file, band = 20, varname = "votkeavt")           # Depth is a band (3rd dimension)

check[check == max(values(check))] <- NA
check <- as.data.frame(check, xy = T, na.rm = T)

ggplot() +
  geom_raster(data = check, aes(x=x, y=y), fill = "red") +
  geom_raster(data = filter(check, vertical.eddy.diffusivity <0.05), aes(x=x, y=y, fill = vertical.eddy.diffusivity))

#### double check ####

W <- nc_open(file)                # Get the different depth vector for W files

DepthsW <- W$dim$depthw$vals ; nc_close(W) ; rm(W) 

nc_close(W)

raw <-nc_open(file)

array <- ncvar_get(raw, "votkeavt", start = c(1,1,20,1), count = c(-1,-1,1, -1))

nc_close(raw)

array[array == max(array)] <- NA
array[array == 0] <- NA

#array[array >= 0.007] <- NA

#which(array == max(array))

#quantile_plot <- data.frame(quantile = seq(0.75, 0.9, 0.01)) %>% 
quantile_plot <- data.frame(quantile = seq(0.1, 1, 0.01)) %>% 
  mutate(value = quantile(array, probs = quantile, na.rm = T))

ggplot(quantile_plot) +
  geom_line(aes(x = quantile, y = log10(value))) +
  theme_minimal()

#### Check sinmod ####

file <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD/", 
                   recursive = F, full.names = TRUE, pattern = "[.]nc")[2]

raw <-nc_open(file)

array <- ncvar_get(raw, "vertical_diffusivity", start = c(1,1,1,1), count = c(-1,-1,1, -1))

nc_close(raw)

#quantile_plot <- data.frame(quantile = seq(0.75, 0.9, 0.01)) %>% 
quantile_plot <- data.frame(quantile = seq(0.1, 1, 0.01)) %>% 
  mutate(value = quantile(array, probs = quantile, na.rm = T))

ggplot(quantile_plot) +
  geom_line(aes(x = quantile, y = value)) +
  theme_minimal()

#### Check pre crop ####

# file <- "/mnt/idrive/Science/MS/Shared/CAO/nemo/ALLARC/1980/grid_W_19800105.nc" 
# 
# check <- raster(file, band = 1, varname = "votkeavt")           # Depth is a band (3rd dimension)
# check[check == max(values(check))] <- NA
# 
# check[check == max(values(check), na.rm = T)] <- NA
# 
# check <- as.data.frame(check, xy = T, na.rm = T)
# 
# ggplot() +
#   geom_raster(data = check, aes(x=x, y=y, fill = vertical.eddy.diffusivity))
# 
# raw <-nc_open(file)
# 
# array <- ncvar_get(raw, "votkeavt", start = c(1,1,1,1), count = c(-1,-1,-1, -1))
# array[array == max(array)] <- NA
# 
# array[array == max(array, na.rm = T)] <- NA
# 
# quantile_plot <- data.frame(quantile = seq(0.1, 0.9, 0.01)) %>% 
#   mutate(value = quantile(array, probs = quantile, na.rm = T))
# 
# ggplot(quantile_plot) +
#   geom_line(aes(x = quantile, y = value)) +
#   theme_minimal()



##### 1 D#####

raw <-nc_open(file)

array <- ncvar_get(raw, "votkeavt", start = c(110,75,1,1), count = c(1,1,-1, -1))

nc_close(raw)

ggplot() +
  geom_line(aes(x = 1:75, y = array)) +
  geom_point(aes(x = 1:75, y = array)) +
  coord_flip() +
  scale_x_reverse()
