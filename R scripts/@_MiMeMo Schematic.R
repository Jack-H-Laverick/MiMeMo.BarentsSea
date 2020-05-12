
# Create a 3D MiMeMo schematic, to preserve spatial organisation, and allow 5 linkages to inshore

#### Set up ####

rm(list=ls())                                                   

Data_packages <- c("tidyverse", "data.table", "pbapply", "ncdf4") # List handy data packages
Geo_packages <- c("lwgeom", "sf", "rnaturalearth", "sp", "raster", "stars") # List GIS packages
Plotting_packages <- c("rgl", "threed", "ggnewscale", "ggimage", "egg")
lapply(c(Data_packages, Geo_packages, Plotting_packages), library, character.only = TRUE) # Load packages

colours <- c("Inshore" = "Yellow", "Offshore" = "Yellow3", "Atmosphere" = "grey",
             "Sea bed" = "cornsilk", "Land" = "black", "Ocean" = "lightblue2", 
             "Ice" = "white", "Limit" = "red")                    # Colour scale

lines <- c("Unidirectional" = "dashed", "Reciprocal" = "solid", "Limit" = "solid")

boundary <- c("Limit" = "red")
  
world <- ne_countries(scale = "medium", returnclass = "sf") %>%   # Get a world map
  st_transform(crs = 3035)                                        # Assign polar projection

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")         # Load SF polygons of the MiMeMo model domains

#### Pull an example ice cap from NOAA ####

#file <- "~/Data/Sea-Ice NOAA/Daily_Arctic_Sea_Ice/2018/seaice_conc_daily_nh_f17_20181220_v03r01.nc" 
file <- "~/Data/Sea-Ice NOAA/Daily_Arctic_Sea_Ice/2018/seaice_conc_daily_nh_f17_20180315_v03r01.nc"
nc_raw <- nc_open(file)                                

Ice <- ncvar_get(nc_raw, "goddard_bt_seaice_conc")
Lat <- ncvar_get(nc_raw, "latitude")                              # Pull latitude
Lon <- ncvar_get(nc_raw, "longitude")                             # Pull longitude

Cap <- st_as_stars(Ice) %>%                                       # Pass matrix of extracted variable
  st_as_stars(curvilinear=list(X1 = Lon, X2 = Lat)) %>%           # Pass coordinate matrices and start the grid is curved
  st_transform(crs = 3035) %>%                                    # Reproject
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                  # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  rename(Ice = A1) %>%
  filter(Ice > 0)                                                 # Drop ice_free pixels

plug <- data.frame(Longitude = seq(-180, 180,1), Latitude = 89) %>% # Create a plug to cover the gap at the north pole
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%   # Set dataframe to SF format
  st_transform(3035) %>%
  st_combine() %>% 
  st_cast("LINESTRING") %>%   st_cast("POLYGON")
  
#ggplot() + geom_sf(data = Cap, aes(alpha = Ice), fill = "white", colour = NA) +
#  geom_sf(data = plug, colour = "red")                            # Plug to cover the satellite gap

Extent <- st_union(Cap) %>%                                       # Combine polygons to get a boundary of ice extent, loses "frostyness"
  st_simplify(preserveTopology=TRUE, dTolerance = 20000)          # Soften edges of contour, don't do this if filling in the polygon

#### Domain map ####

get_blade <- function(Longitude, Latitude) {
  first <- data.frame(Longitude, Latitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% # Set dataframe to SF format
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
#  geom_sf(data = Extent, fill = "white", colour = "white") +
  geom_sf(data = domains, aes(fill = Shore), size=0.1) + 
  scale_fill_manual(values = colours, drop = FALSE) +
  geom_sf(data = Extent, fill = NA, colour = "white") +
  geom_sf(data = world, size = 0.1, fill = "black") +
  geom_sf(data = BW.blade, colour = "red") + 
  geom_sf(data = BN.blade, colour = "red") + 
  geom_sf(data = BS.blade, colour = "red") + 
  geom_sf(data = GN.blade, colour = "red") + 
  geom_sf(data = GS.blade, colour = "red") + 
  geom_sf(data = W.blade, colour = "red") + 
  geom_sf(data = plug, colour = "lightblue2", fill = "lightblue2") + # Plug to cover the satellite gap
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000), ) +
  theme_minimal() +
   theme( legend.position = "none",
     panel.background = element_rect(fill = "lightblue2", colour = "lightblue2", size = 0.5, linetype = "solid"),
     panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightblue3"), 
     panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightblue3"))
map
#ggsave("FIG_Domains.png", plot = map, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Make the schematic ####

corner <- function() {
  
  corner <- cube3d(color="white")
  corner[["vb"]] <- corner[["vb"]][,1:6]              # drop corners first
  corner[["ib"]][which(corner[["ib"]] > 6)] <- NA     # drop references to those corner
  corner[["ib"]] <- corner[["ib"]][,1:5]              # drop a face (a corner has 5)
  # Looks like I have to drop the final face (rather than the one being changed) for the format to work, means I have to rotate the shape later
  corner[["ib"]][,2] <- c(3, 5, 6, 4)                 # Add missing face
  corner[["ib"]] <- corner[["ib"]][,c(1,2,5)]         
  
  corner[["it"]] <- c(2, 4, 6) %>%                    # Add two triangular faces
    cbind(c(1, 5, 3))
  # Not sure why I needed to repeat it
  corner[["it"]][, 1] <- c(2, 4, 6)                   # Add missing face
  corner[["it"]][, 2] <- c(1, 5, 3)                   # Add missing face
  
  return(corner)
}                                        # Slice a cube to form a triangle
build <- function(H, V, thick) {
#H <- 2             # Scaling horizontal spacing
#V <- 3             # Scaling vertical spacing  
#thickness <- 0.1   # Model box depth  
D <- H * 1.5        # Scaling depth spacing

  
# Open ocean column
ocean_in <- cube3d(color="blue") %>% scale3d(1, 1, thick) %>% translate3d(2*H, 1*D, 0*V)
ocean_off <- cube3d(color="blue") %>% scale3d(1, 1, thick) %>% translate3d(-1.5*H, 0, 0*V)
ocean_deep <- cube3d(color="blue") %>% scale3d(1, 1, thick) %>% translate3d(-1.5*H, 0, -2*V)

# Offshore column
atmos_off <- cube3d(color="white") %>% scale3d(1, 1, thick) %>% translate3d(0*H, 0, 2*V) 
#ice_off <- corner() %>% rotate3d(pi/2, 0, 1, 0) %>% rotate3d(pi, 0, 0, 1) %>% scale3d(1, 1, 0.1) %>% translate3d(-0.1*H, 0.1*D, 0.5*V) 
offshore <- cube3d(color="yellow") %>% scale3d(1, 1, thick)    # side, back, top
deep <- offshore %>% translate3d(0*H, 0, -2*V) 
seabed_off <- cube3d(color="brown") %>% scale3d(1, 1, thick) %>% translate3d(0*H, 0, -4*V) 
seabed_offa <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d(-0.5, 0-0.5, -4*V) 
seabed_offb <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d(-0.5, 0+0.5, -4*V) 
seabed_offc <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d(+0.5, 0-0.5, -4*V) 
seabed_offd <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d(+0.5, 0+0.5, -4*V) 

# Inshore column
atmos_in <- cube3d(color="white") %>% scale3d(1, 1, thick) %>% translate3d(2*H, 0, 2*V) 
#ice_in <- corner() %>% rotate3d(pi/2, 0, 1, 0) %>% rotate3d(pi, 0, 0, 1) %>% scale3d(1, 1, 0.1) %>% translate3d(1.9*H, 0.1*D, 0.5*V) 
inshore <- cube3d(color="yellow") %>% scale3d(1, 1, thick) %>% translate3d(2*H, 0, 0*V)
seabed_in <- cube3d(color="brown") %>% scale3d(1, 1, thick) %>% translate3d(2*H, 0, -2*V) 
seabed_ina <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d((2*H)-0.5, 0-0.5, -2*V) 
seabed_inb <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d((2*H)-0.5, 0+0.5, -2*V) 
seabed_inc <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d((2*H)+0.5, 0-0.5, -2*V) 
seabed_ind <- cube3d(color="brown") %>% scale3d(0.5, 0.5, thick) %>% translate3d((2*H)+0.5, 0+0.5, -2*V) 

land <- cube3d(color= "black") %>% scale3d(0.25, 1, thick) %>% translate3d(3.5*H, 0, 0*V)  

## Ice which sits on top of box
ice_off <- corner() %>% rotate3d(pi/2, 0, 1, 0) %>% rotate3d(pi, 0, 0, 1) %>% scale3d(0.6, 0.6, thick/2) %>% translate3d(-0.49, 0.56, 0 + (thick/2)) 
ice_in <- corner() %>% rotate3d(pi/2, 0, 1, 0) %>% rotate3d(pi, 0, 0, 1) %>% scale3d(0.6, 0.6, thick/2) %>% translate3d((2*H)-0.46, 0.56, 0 + (thick/2)) 
# # Render them, in a window where the view can be manipulated
# shade3d(atmos_off)
# shade3d(ice_off)
# shade3d(offshore)
# shade3d(deep)
# shade3d(seabed_off)
# 
# shade3d(atmos_in)
# shade3d(ice_in)
# shade3d(inshore)
# shade3d(seabed_in)
# 
# shade3d(land)
# 
# shade3d(ocean_off)
# shade3d(ocean_deep)
# shade3d(ocean_in)

boxes <- list(atmos_off, ice_off, offshore, deep, seabed_off,seabed_offa, seabed_offb, seabed_offc, seabed_offd,
              atmos_in, ice_in, inshore, seabed_in, seabed_ina, seabed_inb, seabed_inc, seabed_ind, 
              land, 
              ocean_off, ocean_deep, ocean_in)

names(boxes) <- c("atmos_off", "ice_off", "offshore", "deep", "seabed_off", "seabed_offa", "seabed_offb", "seabed_offc", "seabed_offd",
              "atmos_in", "ice_in", "inshore", "seabed_in", "seabed_ina", "seabed_inb", "seabed_inc", "seabed_ind", 
              "land", 
              "ocean_off", "ocean_deep", "ocean_in")
return(boxes)
}                              # Making 3dmesh objects is controlled by rgl, not by threed
flatten <- function(obj, view) {
  
  flat <- obj %>%
    transform_by(invert_matrix(view)) %>%    # Locate relative to view
    perspective_projection() %>%                        # Instill perspective
    fortify.mesh3d() %>%                                # Convert to dataframe
    filter(!hidden)                                     # Drop lines obscured from view
  
}                              # Convert a 3d mesh object into a flat ggplot with threed 
make_linkage <- function(centers, Dir) {
  
  a <- centers[1,]                                                                      # Grab the first point
  b <- centers[2,]                                                                      # Grab the second point
  
  if(Dir == 1) Exchange <- "Unidirectional"
  if(Dir == 2) Exchange <- "Reciprocal"
  
  arrow <- rbind(data.frame(x = a$x, y = a$y,                       # Line segment from a to b 
                            xend = b$x, yend = b$y),
                 data.frame(x = b$x,  y = b$y,                       # Line segment from b to a
                            xend = a$x, yend = a$y)) %>%
    mutate(Exchange = Exchange)

   return(arrow)}                      # Segments to connect the centres of boxes
schematify <- function(H, V, thick, side, front, above) {
# H = 1 ; V = 1 ; thick = 0.4 ; side = 10 ; front = -20 ; above = 50

## Define view
 view <- threed::look_at_matrix(eye = c(side, front, above), at = c(20*H, 0, 0))
  
## Define model boxes
 boxes <- build(H, V, thick) %>%                     # Make boxes 
   map(flatten, view) %>%                            # convert for ggplot
   rbindlist(idcol = TRUE) %>%                       # Combine as dataframe
   mutate(Compartment = as.factor(.id),              # Create column for colouring
          zorder = paste0(.id, "_", zorder))         # Create a grouping variable which allows for correct plotting of 3D boxes
          
 levels(boxes$Compartment) <- list("Atmosphere" = c("atmos_off", "atmos_in"),
                                "Ice" = c("ice_off", "ice_in"),
                                "Ocean" = c("ocean_off", "ocean_deep", "ocean_in"),
                                "Offshore" = c("offshore", "deep"), "Inshore" = "inshore",
                                "Land" = "land", "Sea bed" = c("seabed_off", "seabed_in",
                                                               "seabed_ina", "seabed_inb", "seabed_inc", "seabed_ind",
                                                               "seabed_offa", "seabed_offb", "seabed_offc", "seabed_offd")) # Create factor to control fill

 ## We need to plot the geometries in different layers to ensure correct overlapping in 3D space  
 sky_boxes <- filter(boxes, Compartment == "Atmosphere") 
 ice_boxes <- filter(boxes, Compartment == "Ice") 
 surf_boxes <- filter(boxes, .id %in% c("inshore", "offshore"))
 Deep_boxes <- filter(boxes, .id == "deep")
 Bottom_boxes_front <- filter(boxes, Compartment == "Sea bed" & !.id %in% c("seabed_in", "seabed_off")) %>%
   filter(.id %in% c("seabed_ina", "seabed_inc", "seabed_offa", "seabed_offc"))
 Bottom_boxes_back <- filter(boxes, Compartment == "Sea bed" & !.id %in% c("seabed_in", "seabed_off")) %>%
   filter(!.id %in% c("seabed_ina", "seabed_inc", "seabed_offa", "seabed_offc"))

 ## Define midpoints for arrows to point at  
 midpoints <- filter(boxes, element_id == 6 | Compartment == "Ice") %>%  # filter to just the top face of a box
   filter(element_id != 5) %>%
   group_by(.id, Compartment) %>% # Create arrows to show flows between model boxes
   filter(!.id %in% c("seabed_ina", "seabed_inb", "seabed_inc", "seabed_ind",
                       "seabed_offa", "seabed_offb", "seabed_offc", "seabed_offd")) %>%
   summarise(x = mean(x), y = mean(y))

 external <- filter(midpoints, Compartment %in% c("Atmosphere", "Ocean", "Land")) %>%
   mutate(icon = Compartment) 
 levels(external$icon) <- list("~/Downloads/_ionicons_svg_md-cloud-outline.svg" = "Atmosphere", 
                                "~/Downloads/mountain-solid.svg"= "Land", 
                                '~/Downloads/water-solid.svg' = "Ocean")
                                    
 ## Define exchanges
 Sky_arrows <- rbind(make_linkage(midpoints[c(6,1),],2),          # In to Atmos
               make_linkage(midpoints[c(4,1),],2),                # In Ice to Atmos
               make_linkage(midpoints[c(11,2),],2),               # Off to Atmos
               make_linkage(midpoints[c(5,2),],2))                # Off Ice to Atmos
                
 Surf_arrows <-rbind(make_linkage(midpoints[c(6,11),],2),         # In to Off
               make_linkage(midpoints[c(6,9),],2),                # In to Ocean
               make_linkage(midpoints[c(6,4),],2),                # In to ice
               make_linkage(midpoints[c(7,6),],1),                # Land to Inshore
               make_linkage(midpoints[c(11,10),],2),              # Off to Ocean
               make_linkage(midpoints[c(11,5),],2)) %>%           # Off to Ice
               mutate(Exchange = as.factor(Exchange))
 levels(Surf_arrows$Exchange) <- c(levels(Surf_arrows$Exchange) , "Limit") # Add a factor level to hack a red line in the lines legend for the map panel
  
 Middle_arrows <- rbind(make_linkage(midpoints[c(11,3),],2),      # Off to Deep
                  make_linkage(midpoints[c(3,8),],2))             # Deep to Ocean
  
 Bottom_arrows <- rbind(make_linkage(midpoints[c(6,12),],2),      # In to Seabed
                  make_linkage(midpoints[c(3,13),],2))            # Deep to Seabed
  
 land_arrow <- make_linkage(midpoints[c(7,6),],1) %>%             # Land to Inshore
               slice(1) %>% 
               mutate(xs = quantile(c(x, xend),0.8), xsend = quantile(c(x, xend),0.7),
                      ys = quantile(c(y, yend),0.2), ysend = quantile(c(y, yend),0.3))

# plot boxes from the bottom up to ensure correct overlapping
 ggplot() + 
   geom_polygon(data = Bottom_boxes_back, aes(x = x, y = y, group = zorder, fill = Compartment), colour = 'black', size = 0.2) +
   geom_polygon(data = Bottom_boxes_front, aes(x = x, y = y, group = zorder, fill = Compartment), colour = 'black', size = 0.2) +
   geom_segment(data = Bottom_arrows, aes(x=x, y=y, xend=xend, yend=yend, linetype = Exchange), size = 0.3) +
   geom_polygon(data = Deep_boxes, aes(x = x, y = y, group = zorder, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
   geom_segment(data = Middle_arrows, aes(x=x, y=y, xend=xend, yend=yend, linetype = Exchange), size = 0.3) +
   geom_polygon(data = surf_boxes, aes(x = x, y = y, group = zorder, fill = Compartment), alpha = 0.9, colour = 'black', size = 0.2) +
   geom_segment(data = Surf_arrows, aes(x=x, y=y, xend=xend, yend=yend, linetype = Exchange), size = 0.3) +
   geom_polygon(data = ice_boxes, aes(x = x, y = y, group = zorder, fill = Compartment), colour = 'black', size = 0.2) +
   geom_segment(data = Sky_arrows, aes(x=x, y=y, xend=xend, yend=yend, linetype = Exchange), size = 0.3) +
   scale_fill_manual(values = colours) +
   geom_point(data = midpoints, aes(x = x, y = y), colour = "black") +
   geom_image(data = external, aes(x = x, y = y+0.01, colour = Compartment, image = icon), size = 0.075) +
   scale_colour_manual(values = colours, name = "External") +
   scale_linetype_manual(values = lines, name = "Exchanges", drop = FALSE) +
   guides(linetype = guide_legend(override.aes = list(colour = c("red", "black", "black")), order=1),
          fill = guide_legend(order=2)) +
   geom_segment(data = land_arrow, aes(x=xs, y=ys, xend= xsend, yend= ysend), 
                arrow = arrow(length = unit(0.2, "cm"),type = "closed"), size = 0.3) +
   theme_void() +
   theme(legend.position = "right",
         legend.title=element_text(size=7), 
         legend.text=element_text(size=5.5)) +
   coord_equal()

}     # Plot the schematic in a particular view (with H and V for box spacing)

schematic <- schematify(H=1.5, V=3, thick=0.4, 10, -20, 50)       # The function handles the plotting of the schematic, the 3 inputs control your view
schematic

combined <- ggarrange(map, schematic, ncol = 2, widths = c(1,1))  # Combine map and schematic

# ggsave("~/Data/Bathymetry GEBCO/MiMeMo Schematic3Dice_cap.cont.png", plot= combined, scale = 1, width = 17, height = 8.5, units = "cm", dpi = 500)

