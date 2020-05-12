
#### Set up ####
setwd("~/Data/Sea-Ice NOAA")
rm(list=ls())                                                               # Wipe the brain
library(tidyverse)
library(mapproj)
library(rnaturalearth)
library(sf)
library(rgdal)
library(viridis)
library(ggnewscale)                                                         # Lets you specify multiple fills and colours
library(gganimate)                                                          # Animate ggplots
library(gifski)  

#load("Satellite_Ice_A.Rdata")                                               # Load annual average ice cover
load("Satellite_Ice_M.Rdata")                                               # Load monthly average ice cover
load("GEBCO contours.Rdata")                                                # Load 200m and 1000m depth contour
world <- ne_countries(scale = "medium", returnclass = "sf")                 # Get a world map

#### Reproject ####

crs <- 3035                                                                 # Polar central map projection

world <- st_transform(world, crs = crs)                                     # Assign polar projection

points <- st_as_sf(Ice_Annual, coords = c("Longitude", "Latitude"), crs = 4326) # Specify original projection (crs) 
points <- st_transform(points, crs = crs)                                   # Transform to new projection

#### Overlay ice on bathymetry ####

ggplot() +
  geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
  scale_colour_viridis(name = 'Depth (m)', discrete = TRUE,
                       guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
#  new_scale_colour() +                                               # Prepare for a second colour scale
  geom_sf(data = filter(points, Year == 2000), aes(alpha = Sea_ice_avg), 
          colour = "turquoise", stroke = 0, size = 0.75, show.legend = "point") +
  scale_alpha(range = c(0,1), name = 'Sea Ice\n Concentration', guide = guide_legend(override.aes = list(linetype = "blank", shape = 16, size = 4)))+
# scale_colour_continuous(name = 'Average Sea Ice\nConcentration', low = "blue", high = "white") +
  geom_sf(data = world) +
  coord_sf(xlim = c(7000000, 000000), ylim = c(7500000, 3500000)) +
  theme_minimal() +
  NULL

# ggsave('FIG_NOAA Sea Ice over bottom.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Seasonal animation for year 2000 ####

Ice_m <- filter(ICE, Year == 2000)
points <- st_as_sf(Ice_m, coords = c("Longitude", "Latitude"), crs = 4326) # Specify original projection (crs) 
points <- st_transform(points, crs = crs)                                   # Transform to new projection

Ice_gif <- ggplot() +
  geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
  scale_colour_viridis(name = 'Depth (m)', discrete = TRUE,
                       guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  #  new_scale_colour() +                                               # Prepare for a second colour scale
  geom_sf(data = points, aes(alpha = Sea_ice_avg), 
          colour = "turquoise", stroke = 0, size = 0.75, show.legend = "point") +
  scale_alpha(range = c(0,1), name = 'Sea Ice\n Concentration', guide = guide_legend(override.aes = list(linetype = "blank", shape = 16, size = 4)))+
  # scale_colour_continuous(name = 'Average Sea Ice\nConcentration', low = "blue", high = "white") +
  geom_sf(data = world) +
  coord_sf(xlim = c(7000000, 000000), ylim = c(7500000, 3500000)) +
  theme_minimal() +
  enter_fade() +
  exit_fade() +
  #transition_states(Month, transition_length = 1, state_length = 1) +   
  transition_manual(Month) +   
  labs(subtitle = "Month: {current_frame}") +  
  NULL

gganimate::animate(Ice_gif, width = 16, height = 10,                                     # Run animation - user specified
                   res = 300, units = "cm", fps = 10, nframes = 200)
#anim_save("GIF_Ice 2000", animation = last_animation())    # Save animation


#### Decadal facet ####

decades <- separate(Ice_Annual, Year, into = c("century", "decade", "year"), # Extract the decade
           remove = FALSE, sep = c(2, 3))  %>%                               # Seperate the string at a character position
            group_by(decade, Longitude, Latitude) %>%                        # Group by pixel and decade
             summarise(Sea_ice_avg = mean(Sea_ice_avg)) %>%                  # Average sea ice concentrations
              ungroup() %>%                                                  # Ungroup
               mutate(decade = as.factor(decade))                            # Change decade to factor

levels(decades$decade) <- list ("1970" = "7", "1980" = "8", "1990" = "9", "2000" = "0", "2010" = "1") # Rename decades
                                
decades <- st_as_sf(decades, coords = c("Longitude", "Latitude"), crs = 4326) %>% # Specify original projection (crs) 
           st_transform(crs = crs)                                           # Transform to new projection

ggplot() +
  geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
  scale_colour_viridis(name = 'Depth (m)', discrete = TRUE,
                       guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  #  new_scale_colour() +                                               # Prepare for a second colour scale
  geom_sf(data = decades, aes(alpha = Sea_ice_avg), 
          colour = "turquoise", stroke = 0, size = 0.75, show.legend = "point") +
  scale_alpha(range = c(0,1), name = 'Sea Ice\n Concentration', guide = guide_legend(override.aes = list(linetype = "blank", shape = 16, size = 4)))+
  # scale_colour_continuous(name = 'Average Sea Ice\nConcentration', low = "blue", high = "white") +
  geom_sf(data = world) +
  coord_sf(xlim = c(7000000, 000000), ylim = c(7500000, 3500000)) +
  facet_wrap(~decade) +
  theme_minimal() +
  NULL
# ggsave('FIG_NOAA Sea Ice decades.png', plot = last_plot(), scale = 1, width = 32, height = 20, units = "cm", dpi = 500)
