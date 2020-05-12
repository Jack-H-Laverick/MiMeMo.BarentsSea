
# Create a 3D MiMeMo schematic, to preserve spatial organisation, and allow 5 linkages to inshore

#### Set up ####

setwd("~/Data/Bathymetry GEBCO")
rm(list=ls())                                                   
library(tidyverse)
library(data.table)
library(egg)
library(sp)
library(raster)
library(sf)
library(lwgeom)
library(rnaturalearth)
library(pbapply)
library(threed)
library(rgl)

#devtools::install_github("coolbutuseless/threed")

colours <- c("Inshore" = "Yellow", "Offshore" = "Yellow3", "Atmosphere" = "white",
             "Sea bed" = "cornsilk", "Land" = "black", "Ocean" = "lightblue2") # Colour scale

world <- ne_countries(scale = "medium", returnclass = "sf") %>%   # Get a world map
  st_transform(crs = 3035)                                        # Assign polar projection

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")         # Load SF polygons of the MiMeMo model domains

#### Domain map ####

get_blade <- function(Longitude, Latitude) {
  first <- data.frame(Longitude, Latitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%       # Set dataframe to SF format
    st_transform(3035) %>%
    st_combine() %>%
    st_cast("LINESTRING") 
}     

BW.blade <- get_blade(c(16.23, 24.25), c(70, 70))                 # Small cut out the Norwegian shelf
W.blade <- get_blade(c(41, 42.3), c(66.8, 66.5))                  # Inshore cut for white sea
BN.blade <- get_blade(c(71.2999, 67.737355),                      # Complicated cut along the East Barents sea
                      c(76.60501, 76.997247))
BS.blade <- get_blade(c(57.492431, 61.144822),                    # Complicated cut along the East Barents sea
                      c(70.736206, 69.459661))
GN.blade <- get_blade(c(-8, -14), c(81.6, 81.6))                  # Inshore cuts for Greenland
GS.blade <- get_blade(c(-18, -24), c(70, 70))                     # Offshore cuts for Greenland

map <- ggplot() +                                                 # Look at which polygons I want
  geom_sf(data = domains, aes(fill = Shore), size=0.1) + 
  scale_fill_manual(values = colours, drop = FALSE) +
  geom_sf(data = world, size = 0.1, fill = "black") +
  geom_sf(data = BW.blade, colour = "red") + 
  geom_sf(data = BN.blade, colour = "red") + 
  geom_sf(data = BS.blade, colour = "red") + 
  geom_sf(data = GN.blade, colour = "red") + 
  geom_sf(data = GS.blade, colour = "red") + 
  geom_sf(data = W.blade, colour = "red") + 
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000), ) +
  theme_minimal() +
  theme(legend.position = "none")
map
#ggsave("FIG_Domains.png", plot = map, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### 3D Example in threed ####
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define camera position and what it's looking at.
# Use the inverse of this to transform all objects in the world
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
camera_to_world <- threed::look_at_matrix(eye = c(3, 4, 5), at = c(0, 0, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  - take a cube object
#  - position it in the camera view
#  - perform perspective projection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- threed::mesh3dobj$cube %>%
   transform_by(invert_matrix(camera_to_world)) %>%
 perspective_projection()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use ggplot to plot the obj
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(obj) + 
 geom_polygon(aes(x = x, y = y, group = zorder, fill = 0.5 * fnx + fny), colour = 'black', size = 0.2) +
 theme_minimal() +
 theme(legend.position = 'none', axis.text = element_blank()) +
 coord_equal() 


#### Make the schematic ####

build <- function(H, V) {
#H <- 2             # Scaling horizontal spacing
D <- H * 1.5             # Scaling depth spacing
#V <- 3             # Scaling vertical spacing

# Open ocean column
ocean_in <- cube3d(color="blue") %>% scale3d(1, 1, 0.1) %>% translate3d(3*H, 3*D, 0*V)
ocean_off <- cube3d(color="blue") %>% scale3d(1, 1, 0.1) %>% translate3d(-3*H, 0*D, 0*V)
ocean_deep <- cube3d(color="blue") %>% scale3d(1, 1, 0.1) %>% translate3d(-3*H, 0*D, -2*V)

# Offshore column
atmos_off <- cube3d(color="white") %>% scale3d(1, 1, 0.1) %>% translate3d(0*H, 0*D, 2*V) 
offshore <- cube3d(color="yellow") %>% scale3d(1, 1, 0.1)    # side, back, top
deep <- offshore %>% translate3d(0*H, 0*D, -2*V) 
seabed_off <- cube3d(color="brown") %>% scale3d(1, 1, 0.1) %>% translate3d(0*H, 0*D, -4*V) 

# Inshore column
atmos_in <- cube3d(color="white") %>% scale3d(1, 1, 0.1) %>% translate3d(3*H, 0*D, 2*V) 
inshore <- cube3d(color="yellow") %>% scale3d(1, 1, 0.1) %>% translate3d(3*H, 0*D, 0*V)
seabed_in <- cube3d(color="brown") %>% scale3d(1, 1, 0.1) %>% translate3d(3*H, 0*D, -2*V) 

land <- cube3d(color= "black") %>% scale3d(0.25, 1, 0.1) %>% translate3d(5*H, 0*D, 0*V)  

# # Render them, in a window where the view can be manipulated
# shade3d(atmos_off)
# shade3d(offshore)
# shade3d(deep)
# shade3d(seabed_off)
# 
# shade3d(atmos_in)
# shade3d(inshore)
# shade3d(seabed_in)
# 
# shade3d(land)
# 
# shade3d(ocean_off)
# shade3d(ocean_deep)
# shade3d(ocean_in)

boxes <- list(atmos_off, offshore, deep, seabed_off,
              atmos_in, inshore, seabed_in, 
              land, 
              ocean_off, ocean_deep, ocean_in)

names(boxes) <- c("atmos_off", "offshore", "deep", "seabed_off",
              "atmos_in", "inshore", "seabed_in", 
              "land", 
              "ocean_off", "ocean_deep", "ocean_in")
return(boxes)
}                                     # Making 3dmesh objects is controlled by rgl, not by threed
flatten <- function(obj, view) {
  
  flat <- obj %>%
    transform_by(invert_matrix(view)) %>%    # Locate relative to view
    perspective_projection() %>%                        # Instill perspective
    fortify.mesh3d() %>%                                # Convert to dataframe
    filter(!hidden)                                     # Drop lines obscured from view
  
}                              # Convert a 3d mesh object into a flat ggplot with threed 
make_linkage <- function(centers) {
  
  a <- centers[1,]                                                                      # Grab the first point
  b <- centers[2,]                                                                      # Grab the second point
  
  arrow <- rbind(data.frame(x = a$x, y = a$y,                       # Line segment from a to b 
                            xend = b$x, yend = b$y),
                 data.frame(x = b$x,  y = b$y,                       # Line segment from b to a
                            xend = a$x, yend = a$y))
  return(arrow)}                           # Segments to connect the centres of boxes
schematify <- function(H, V, side, front, above) {

## Define view
 view <- threed::look_at_matrix(eye = c(side, front, above), at = c(20*H, 0, 0))
  
## Define model boxes
 flats <- build(H, V) %>%                            # Make boxes 
   map(flatten, view) %>%                            # convert for ggplot
    rbindlist(idcol = TRUE) %>%                      # Combine as dataframe
    mutate(Compartment = as.factor(.id)) %>%         # Create column for colouring
    filter(element_id == 6)                          # filter to just the top face of a box

 levels(flats$Compartment) <- list("Atmosphere" = c("atmos_off", "atmos_in"),
                                "Ocean" = c("ocean_off", "ocean_deep", "ocean_in"),
                                "Offshore" = c("offshore", "deep"), "Inshore" = "inshore",
                                "Land" = "land", "Sea bed" = c("seabed_off", "seabed_in")) # Create factor to control fill

 ## We need to plot the geometries in different layers to ensure correct overlapping in 3D space  
 sky_flats <- filter(flats, Compartment == "Atmosphere")
 surf_flats <- filter(flats, !.id %in% c("deep", "ocean_deep") & !Compartment %in% c("Atmosphere", "Sea bed"))
 Deep_flats <- filter(flats, .id %in% c("deep", "ocean_deep"))
 Bottom_flats <- filter(flats, Compartment == "Sea bed")  
 
 ## Define midpoints for arrows to point at  
  midpoints <- group_by(flats, .id) %>% # Create arrows to show flows between model boxes
    summarise(x = mean(x), y = mean(y))
  
 ## Define arrows
  Top_arrows <-rbind(make_linkage(midpoints[c(4,9),]),           # In to Off
                     make_linkage(midpoints[c(4,9),]),           # In to Off
                     make_linkage(midpoints[c(4,1),]),           # In to Atmos
                     make_linkage(midpoints[c(4,7),]),           # In to Ocean
                     make_linkage(midpoints[c(5,4),]),           # Land to Inshore
                     make_linkage(midpoints[c(9,8),]),           # Off to Ocean
                     make_linkage(midpoints[c(9,2),]))           # Off to Atmos
                     
  Middle_arrows <- rbind(make_linkage(midpoints[c(9,3),]),       # Off to Deep
                   make_linkage(midpoints[c(3,6),]))             # Deep to Ocean
  
  Bottom_arrows <- rbind(make_linkage(midpoints[c(4,10),]),      # In to Seabed
                   make_linkage(midpoints[c(3,11),]))            # Deep to Seabed
  
  # plot boxes from the bottom up to ensure correct overlapping
  ggplot() + 
    geom_polygon(data = Bottom_flats, aes(x = x, y = y, group = .id, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
    geom_segment(data = Bottom_arrows, aes(x=x, y=y, xend=xend, yend=yend)) +
#                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_polygon(data = Deep_flats, aes(x = x, y = y, group = .id, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
    geom_segment(data = Middle_arrows, aes(x=x, y=y, xend=xend, yend=yend)) +
#                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_polygon(data = surf_flats, aes(x = x, y = y, group = .id, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
    geom_segment(data = Top_arrows, aes(x=x, y=y, xend=xend, yend=yend)) +
#                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_polygon(data = sky_flats, aes(x = x, y = y, group = .id, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
    #geom_point(aes(x=0, y=0), colour = "red") +
    geom_point(data = midpoints, aes(x = x, y = y), colour = "black") +
    theme_void() +
    scale_fill_manual(values = colours, drop = FALSE) +
    theme(legend.position = "left") +
    coord_equal()

}             # Plot the schematic in a particular view (with H and V for box spacing)

schematic <- schematify(H=1, V=2, 10, -20, 50)                           # The function handles the plotting of the schematic, the 3 inputs control your view
schematic

combined <- ggarrange(map, schematic, ncol = 2, widths = c(1,1))       # Combine map and schematic

#ggsave("MiMeMo Schematic3D.png", plot= combined, scale = 1, width = 16, height = 6, units = "cm", dpi = 500)


