
## could speed up the rasterise calls by using st_intersects

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis", "ncdf4", "gganimate", "gifski") # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth", "nngeo")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages

domains <- readRDS("./Objects/Domains.rds") %>%                               # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

world <- ne_countries(scale = "medium", returnclass = "sf") %>%               # Get a world map
  st_transform(crs = 4326)

box <- st_bbox(domains)

cells <- st_as_stars(box, dx = 0.01, dy = 0.01, values = 0) %>%
  st_as_sf(as_points = F, merge = F)                                          # Convert to SF dataframe for modelling.

Seasonal <-readRDS("./Objects/Seasonal.rds")                                  # Import average fishing activity by month

#### Gridding fishing ####

Static <- filter(Seasonal, geartype == "trawlers", fishing > 0.5) %>%
  group_by(long, lat) %>%
  summarise(fishing = sum(fishing)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_join(cells, .) %>%
  drop_na() %>%
  st_transform(crs = 3035)

#### Get some ice ####
# 
# file <- "./Data/Daily_Arctic_Sea_Ice/2018/seaice_conc_daily_nh_f17_20180315_v03r01.nc"
# nc_raw <- nc_open(file)                                
# 
# Ice <- ncvar_get(nc_raw, "goddard_bt_seaice_conc")
# Lat <- ncvar_get(nc_raw, "latitude")                              # Pull latitude
# Lon <- ncvar_get(nc_raw, "longitude")                             # Pull longitude
# 
# Cap <- st_as_stars(Ice) %>%                                       # Pass matrix of extracted variable
#   st_as_stars(curvilinear=list(X1 = Lon, X2 = Lat)) %>%           # Pass coordinate matrices and start the grid is curved
#   st_transform(crs = 3035) %>%                                    # Reproject
#   st_as_sf(as_points = FALSE, merge = FALSE) %>%                  # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
#   rename(Ice = A1) %>%
#   filter(Ice > 0)      

Month_list <- list("January"="1","February"="2","March" = "3",       # Control frame order
                   "April"="4","May"="5","June"="6","July"="7","August"="8",
                   "September"="9","October"="10","November"="11","December"="12")

Ice_m <- readRDS("./Objects/Satellite_Ice_GFW.rds") %>% 
  mutate(Month = as.factor(Month))

levels(Ice_m$Month) <- Month_list


file <- "./Data/Daily_Arctic_Sea_Ice/2018/seaice_conc_daily_nh_f17_20180315_v03r01.nc"
nc_raw <- nc_open(file)                                

Ice <- ncvar_get(nc_raw, "goddard_bt_seaice_conc")
Lat <- ncvar_get(nc_raw, "latitude")                              # Pull latitude
Lon <- ncvar_get(nc_raw, "longitude")                             # Pull longitude

Cap <- st_as_stars(Ice) %>%                                       # Pass matrix of extracted variable
  st_as_stars(curvilinear=list(X1 = Lon, X2 = Lat)) %>%           # Pass coordinate matrices and state the grid is curved
  st_transform(crs = 3035) %>%                                    # Reproject
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                  # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  st_join(Ice_m) %>%
  drop_na() %>% 
  select(-A1) %>% 
  rename(Ice = Sea_ice_avg)

# which month has minimum ice extent
#check <- split(Cap, f = Cap$Month)
#lapply(check, nrow) # September 

#### Plotting ####

domains <- st_transform(domains, crs = 3035)

box2 <- st_bbox(domains)

ggplot() +
  geom_sf(data = filter(Cap, Month == "September"), aes(alpha = Ice), fill = "White", colour = "white", lwd = 0.01, show.legend = F) +
  geom_sf(data = Static, aes(fill = log(fishing+1)), lwd = 0) +
  scale_fill_viridis(option = "inferno", name = "Fishing\nhours (log)") +
  geom_sf(data = world, fill = "black") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue2", colour = "lightblue2", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightblue3"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightblue3"),
        legend.position = c(0.075, 0.7),
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white")) +
  coord_sf(xlim = c(box2$xmin + 500000, box2$xmax), ylim = c(box2$ymin, box2$ymax - 300000)) +
  #  annotate(geom = "text", x = box2$xmin + 1500000, y = box2$ymin, label = "Global Fishing Watch & NOAA 2012:2016", hjust = 0) +
NULL

 ggsave("./Figures/poster_Fishing.png", plot = last_plot(), scale = 1, width = 20.27, height = 12.67, units = "cm", dpi = 500)


#### Animate ####

Monthly <- filter(Seasonal, geartype == "trawlers", fishing > 0.5) %>%
  group_by(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_join(cells, .) %>%
  drop_na() %>%
  st_transform(crs = 3035)

Monthly <- mutate(Monthly, Month = as.factor(Month))
levels(Monthly$Month) <- Month_list

cycle <- ggplot() +
  geom_sf(data = Cap, aes(alpha = Ice), fill = "White", colour = "white", lwd = 0.01, show.legend = F) +
  geom_sf(data = Monthly, aes(fill = log(fishing+1)), lwd = 0) +
  scale_fill_viridis(option = "inferno", name = "Fishing\nhours (log)") +
  geom_sf(data = world, fill = "black") +
  annotate(geom = "text", x = box2$xmin + 400000, y = box2$ymin + 15000, 
           label = "Global Fishing Watch &\nNOAA 2012 - 2016", hjust = 0, fontface = "italic") +
  theme_minimal() +
    theme(panel.background = element_rect(fill = "lightblue2", colour = "lightblue2", size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightblue3"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightblue3"),
        legend.position = c(0.075, 0.7),
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        plot.caption = element_text(hjust = 0.97, vjust = 27, colour = "white",),
        axis.text = element_text(colour = "white")) +
  coord_sf(xlim = c(box2$xmin + 500000, box2$xmax), ylim = c(box2$ymin, box2$ymax - 300000)) +
  enter_fade() +
  exit_fade() +
  transition_manual(Month) +   
  labs(caption = "{current_frame}", x = NULL, y = NULL) +
  NULL

gganimate::animate(cycle, width = 21.62, height = 19.05, res = 300, units = "cm", fps = 2, nframes = 12,
                     bg = 'transparent')                                             # Run animation - user specified
anim_save("./Figures/Conference_Fishing GIF", animation = last_animation())          # Save animation

