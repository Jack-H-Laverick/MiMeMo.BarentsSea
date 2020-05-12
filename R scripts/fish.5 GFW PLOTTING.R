
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("MiMeMo.tools", "tidyverse", "ggfortify", "viridis", "gganimate", "gifski", "pbapply") # List handy data packages
Geo_packages <- c("sf", "rgdal", "rnaturalearth", "maps", "polyggon", "raster", "transformr", "rgeos", "sp", "mapproj")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
#source("./R scripts/gfw.z FUNCTIONS.R")                                     # Bring in the functions I've written
source("./R scripts/@_Region file.R")                                       # Bring in the functions I've written

## Slow to read in this large datafile, you only currently need it for the heatmap section, otherwise revisit data-wrangling script
#Arctic_fishing <- read.csv("Arctic_Global_Fishing_Watch.csv", header = TRUE) # Read in Arctic subset of GFW data

Seasonal <- readRDS("./Objects/Seasonal.rds")                               # Import average fishing activity by month
Regional_ts <- readRDS("./Objects/Regional_ts.rds")                         # Import average fishing activity in FAO regions
lines <- readRDS("./Objects/Bathymetry_lines.rds")                          # Load 200m and 1000m depth contour
Ice <- readRDS("./Objects/Satellite_Ice_M.rds")                             # Load monthly average ice cover
Greenland_in <- readRDS("./Objects/Domain_GI.rds")                          # Load Domain
Greenland_off <- readRDS("./Objects/Domain_GO.rds")                         # Load Domain
Barents_in <- readRDS("./Objects/Domain_BI.rds")                            # Load Domain
Barents_off <- readRDS("./Objects/Domain_BO.rds")                           # Load Domain

world <- fortify(map("world", fill = TRUE, col = "grey"))                   # Import map of landmasses

geartypes <- as.character(unique(Seasonal$geartype))                        # Get a list of geartypes
limit <- 0.5                                                                # Minimum average fishing hours in the month we care about

FAO <- readRDS("./Objects/FAO.rds")                                           # Read in polygons for FAO regions of interest

#### Plot map by gear type (Meractore projection) ####

lapply(geartypes, jacks_lazy_avg, limit)                                    # Create, save, and print the plot for each geartype

lapply(geartypes, jacks_lazy_animation, limit)                              # Create, save, and print the plot for each geartype

## VERY SLOW for trawlers!!! lots of points to animate. Don't re-run for the sake of it. (we're talking hours)
#jacks_lazy_animation("XXXXXXXXX", limit) %>%                               # Comment out none plot lines in function and run this for a single animation
#  gganimate::animate(width = 16, height = 10, res = 300, units = "cm", fps = 10, nframes = 100)                                   # Run animation - user specified
#anim_save("./Figures/GFW/GIF2 XXXXX", animation = last_animation())    # Save animation

#### Reproject with additional layers ####

crs <- 3035                                                                 # Polar central map projection
world_p <- ne_countries(scale = "medium", returnclass = "sf") %>%           # Get a world map
            st_transform(crs = crs)                                         # Assign polar projection

Seasonal_p <- st_as_sf(Seasonal, coords = c("long", "lat"), crs = 4326) %>% # Reproject fishing 
  st_transform(crs = crs) %>%  
  mutate(Month = as.factor(Month))

Month_list <- list("January"="1","February"="2","March" = "3",       # Control frame order
                            "April"="4","May"="5","June"="6","July"="7","August"="8",
                            "September"="9","October"="10","November"="11","December"="12")
levels(Seasonal_p$Month) <- Month_list

FAO_arctic_p <- st_as_sf(FAO_arctic, coords = c("long", "lat"), crs = 4326) %>%    # Reproject FAO regions 
  st_transform(crs = crs)  

Ice_m <- filter(Ice, between(Year,2012,2016)) %>%                           # Reproject ice
  group_by(Latitude, Longitude, Month) %>%
  summarise(Sea_ice_avg = mean(Sea_ice_avg)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% # Specify original projection (crs) 
  st_transform(points_ice, crs = crs) %>%                                   # Transform to new projection
  mutate(Month = as.factor(Month))                                          # Rename months to match Seasonal

levels(Ice_m$Month) <- Month_list

#### Polar projection plots ####

lapply(geartypes, jacks_polar_static, limit)                                # Create, save, and print the plot for each geartype

lapply(geartypes, jacks_polar_animate, limit)                             # Create, save, and print the plot for each geartype

#jacks_polar_animate("XXXXXXXXX", limit) %>%                                # Comment out none plot lines in function and run this for a single animation
#  gganimate::animate(width = 16, height = 10, res = 300, units = "cm", fps = 10, nframes = 100)  # Run animation - user specified
#anim_save("./Figures/GFW/GIF3 XXXXX", animation = last_animation())                  # Save animation

#### Spatial and seasonal bar chart ####

levels(Regional_ts$geartype) <- list( "Drifting\nlonglines" = "drifting_longlines",
                                      "Fixed gear" = "fixed_gear",
                                      "Other fishing" = "other_fishing",
                                      "Purse seines" = "purse_seines",
                                      "Trawlers" = "trawlers")              # Rename factor levels for plotting

Regional_ts$Region <- as.factor(Regional_ts$Region)
levels(Regional_ts$Region) <- list("Norwegian Sea" = "Division_27.2.a",     # Rename factor levels for plotting
                                   "Division 27.2.b" = "Division_27.2.b",
                                   "East Greenland" = "FAO2",
                                   "Barents Sea" = "Sub_area_27.1")

group.colours <- c("Barents Sea" = "Tan2", "Norwegian Sea" = "cyan4",       # Specify colour palette 
                   "East Greenland" = "springgreen3", "Division 27.2.b" = "turquoise")

Regional_gear_ts <- ggplot(Regional_ts, aes(x = Month, y = Regional_fishing, fill = Region)) +
  geom_col() +
  facet_grid(rows = vars(Region), cols = vars(geartype)) +
  labs(x = "Month", y = "Hours of fishing") +
  theme_minimal() +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(guide= guide_legend(title = 'Regions'), values = c(group.colours)) +
  theme(legend.position = "None", strip.text.y = element_text(angle = 0),
  axis.text.x = element_text(angle = 90, vjust = 0.3, size = 5)) +
  NULL
Regional_gear_ts
ggsave('./Figures/GFW/Seasonal fishing by gear and FAO.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")

#### REDUNDANT Heatmap ####

vessel <- Arctic_fishing[rep(seq(1,nrow(Arctic_fishing)), Arctic_fishing$vessel_hours),]    # Scale points by vessel hours for heatmap
fishing <- Arctic_fishing[rep(seq(1,nrow(Arctic_fishing)), Arctic_fishing$fishing_hours),]  # Scale points by fishing hours for heatmap

activity <- ggplot() +
  stat_density_2d(data = filter(fishing, Day == 1), geom = "raster", aes(x = long, y = lat, fill = stat(density)), contour = FALSE, n=1000) +
  scale_fill_gradient(name = 'Scaled density', low = "navy", high = "red") +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black") +
  geom_holygon(data = FAO_arctic, aes(long, lat, group = group), colour = "white", fill = "NA", size = 0.1) +                                                        # Plots polygons with holes
  coord_fixed(1.3, xlim = c(min(-50), max(75)),
   ylim = c(min(50), max(90))) +
  facet_wrap(~geartype) +
  theme_minimal() +
  labs(x = 'Longitude (W)', y = 'Latitude (N)', title = "'Fishing hours' in the Arctic by gear type (2012-2016)") +
#  transition_time(Month) +                                                                 # Comment out to make static
#  labs(title = "Month: {frame_time}") +                                                    # Comment out to make static
  NULL
activity

ggsave('./Figures/GFW/Fishing hours.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

## WARNING ## purse_seines won't animate as there are months without data
#gganimate::animate(activity, width = 16, height = 10,                                      # Run animation - user specified
#                   res = 300, units = "cm", fps = 10, nframes = 200)
# anim_save("./Figures/GFW/GIF_activity by month", animation = last_animation())                          # Save animation
