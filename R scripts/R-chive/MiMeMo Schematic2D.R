
# Create a MiMeMo schematic, doesn't include atmosphere

#### Set up ####

setwd("~/Data/Bathymetry GEBCO")
rm(list=ls())                                                   
library(tidyverse)
library(egg)
library(sp)
library(raster)
library(sf)
library(lwgeom)
library(data.table)
library(rnaturalearth)
library(pbapply)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")              # Load SF polygons of the MiMeMo model domains

colours <- c("Inshore" = "Yellow", "Offshore" = "Yellow3", 
             "Sea bed" = "cornsilk", "Land" = "black", "OOB" = "white") # Colour scale

#### Domain map ####

get_blade <- function
(Longitude, Latitude) {
  first <- data.frame(Longitude, Latitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%       # Set dataframe to SF format
    st_transform(3035) %>%
    st_combine() %>%
    st_cast("LINESTRING") 
}     

BW.blade <- get_blade(c(16.23, 24.25), c(70, 70))                      # Small cut out the Norwegian shelf
W.blade <- get_blade(c(41, 42.3), c(66.8, 66.5))                       # Inshore cut for white sea
BN.blade <- get_blade(c(71.2999, 67.737355),                           # Complicated cut along the East Barents sea
                     c(76.60501, 76.997247))
BS.blade <- get_blade(c(57.492431, 61.144822),                         # Complicated cut along the East Barents sea
                     c(70.736206, 69.459661))
GN.blade <- get_blade(c(-8, -14), c(81.6, 81.6))                       # Inshore cuts for Greenland
GS.blade <- get_blade(c(-18, -24), c(70, 70))                          # Offshore cuts for Greenland

map <- ggplot() +                                                      # Look at which polygons I want
  geom_sf(data = domains, aes(fill = Shore), size=0.1) + 
  scale_fill_manual(values = colours, drop = FALSE) +
  geom_sf(data = world, size = 0.1, fill = "black") +
  geom_sf(data = BW.blade, colour = "red") + 
  geom_sf(data = BN.blade, colour = "red") + 
  geom_sf(data = BS.blade, colour = "red") + 
  geom_sf(data = GN.blade, colour = "red") + 
  geom_sf(data = GS.blade, colour = "red") + 
  geom_sf(data = W.blade, colour = "red") + 
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
  theme_minimal() +
  theme(legend.position = "none")
map
#ggsave("FIG_Domains.png", plot = map, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Adding a schematic ####

## Make model boxes for the plot
make_boxes <- function(id, reg, depth, side, land, bed, Compartment) {
  
  if(reg == "Barents Sea") x <- c(0,0,1,1)
  if(reg == "Greenland") x <- c(0,0,1,1) - 2.6
  
  if(depth == "Shallow") y <- c(0,1,1,0)
  if(depth == "Deep") y <- c(0,1,1,0) - 1.1
  
  if(side == "L") x <- x
  if(side == "R") x <- x + 1.1
  
  if(land == TRUE & side == "L") x <- x - c(0.4,0.4,1.1,1.1)
  if(land == TRUE & side == "R") x <- x + c(1.1,1.1,0.4,0.4)
  
  if(bed == TRUE) y <- y - c(0.4,1.1,1.1, 0.4)     # Make the sea bed box shorter

  box <- data.frame(x, y, box = rep(id, times = 4), Region = rep(reg, times = 4), 
                    Compartment = rep(Compartment, times = 4), Depth = rep(depth, times = 4))
  
} # Make a box to represent a model compartment 

box_all <- rbind(make_boxes(1, "Barents Sea", "Shallow", "L", FALSE, FALSE, "Offshore"),  # Bind all the boxes as a dataframe
                 make_boxes(2, "Barents Sea", "Shallow", "R", FALSE, FALSE, "Inshore"),
                 make_boxes(3, "Barents Sea", "Deep", "L", FALSE, FALSE, "Offshore"), 
                 make_boxes(4, "Barents Sea", "Shallow", "R", FALSE, TRUE, "Sea bed"),
                 make_boxes(5, "Barents Sea", "Deep", "L", FALSE, TRUE, "Sea bed"), 
                 make_boxes(6, "Barents Sea", "Shallow", "R", TRUE, FALSE, "Land"),
                 make_boxes(7, "Greenland", "Shallow", "R", FALSE, FALSE, "Offshore"),         
                 make_boxes(8, "Greenland", "Shallow", "L", FALSE, FALSE, "Inshore"),
                 make_boxes(9, "Greenland", "Deep", "R", FALSE, FALSE, "Offshore"), 
                 make_boxes(10, "Greenland", "Shallow", "L", FALSE, TRUE, "Sea bed"),
                 make_boxes(11, "Greenland", "Deep", "R", FALSE, TRUE, "Sea bed"), 
                 make_boxes(12, "Greenland", "Shallow", "L", TRUE, FALSE, "Land"))

levels(box_all$Compartment) <- c(levels(box_all$Compartment),"OOB")    # Add in Out Of Bounds factor level for legend

## Make text labels for the plot
labels <- rbind(data.frame(x = mean(box_all$x), y = -0.05, label = "60 m"),  
                data.frame(x = mean(box_all$x), y = -1.15, label = "400 m"),
                data.frame(x = mean(filter(box_all, Region == "Barents Sea" & Depth == "Shallow" & Compartment %in% c("Inshore", "Offshore"))$x), 
                           y = 1.5, label = "Barents Sea"),
                data.frame(x = mean(filter(box_all, Region == "Greenland" & Depth == "Shallow" & Compartment %in% c("Inshore", "Offshore"))$x), 
                           y = 1.5, label = "Greenland"))

## Make arrows for flows between model compartments
box_midpoints <- group_by(box_all, box, Region, Compartment, Depth) %>% # Create arrows to show flows between model boxes
  summarise_all(mean) %>%
  mutate(Skinny = ifelse(Compartment %in% c("Land", "Sea bed"), TRUE, FALSE)) # Skinny boxes need coords calculated differently for arrows

midpoints <- filter(box_midpoints, Skinny == FALSE) %>%
  mutate(x = ifelse(Depth == "Shallow", x+1.1, x)) %>%
  rbind(box_midpoints, .)

make_arrow <- function(centers, direction, region, skinny) {
 
 a <- centers[1,]                                                                      # Grab the first point
 b <- centers[2,]                                                                      # Grab the second point

 if(direction == "V") { xadj <- c(0.05, -0.05) ; yadj <- c(0.3, -0.3) } else {
                        xadj <- c(0.3, -0.3) ; yadj <- c(0.05, -0.05) }                # Shrink lines away from midpoints, and set in and out flows slightly apart

 if(skinny == TRUE & direction == "H") xadj <- xadj + c(0, 0.3) 
 if(skinny == TRUE & direction == "V") yadj <- yadj + c(-0.3, 0.6) 

 if(region == "G") xadj <- xadj * -1 ; yadj <- yadj * -1                               # Need to reverse the adjustment as the lines are either side of 0
 
 if(direction == "V") {
 arrow <- rbind(data.frame(x = a$x + xadj[1], y = a$y + yadj[1],                       # Line segment from a to b 
                           xend = b$x + xadj[1], yend = b$y + yadj[2]),
                data.frame(x = b$x + xadj[2], y = b$y + yadj[2],                       # Line segment from b to a
                           xend = a$x + xadj[2], yend = a$y + yadj[1]))
 return(arrow)}
 if(direction == "H") {
   arrow <- rbind(data.frame(x = a$x + xadj[1], y = a$y + yadj[1],                       # Line segment from a to b 
                             xend = b$x + xadj[2], yend = b$y + yadj[1]),
                  data.frame(x = b$x + xadj[2], y = b$y + yadj[2],                       # Line segment from b to a
                             xend = a$x + xadj[1], yend = a$y + yadj[2]))
 return(arrow)}
}

adjv <- matrix(rep(c(0, 1.1), times = 4), nrow = 2, byrow = T)
adjh <- matrix(rep(c(1, 0), times = 4), nrow = 2, byrow = T)

arrows <- rbind(make_arrow(midpoints[c(1,2),], "H", "B", F),           # In to Off Barents
                make_arrow(midpoints[c(7,8),], "H", "G", F),           # In to Off Green
                make_arrow(midpoints[c(1,3),], "V", "B", F),           # Off to Off Barents 
                make_arrow(midpoints[c(7,9),], "V", "G", F),           # Off to Off Green
                make_arrow(midpoints[c(2,6),], "H", "B", T),           # Land Barents
                make_arrow(midpoints[c(8,12),], "H", "G", T),          # Land Green
                make_arrow(midpoints[c(5,3),], "V", "B", T),           # Sediment to Off Barents
                make_arrow(midpoints[c(11,9),], "V", "G", T),          # Sediment to Off Green
                make_arrow(midpoints[c(4,2),], "V", "B", T),           # Sediment to In Barents
                make_arrow(midpoints[c(10,8),], "V", "G", T)) %>%      # Sediment to In Green
  slice(-c(11,9)) %>%
  rbind(make_arrow(midpoints[c(7,9),], "V", "G", F) + adjv,            # Off to OOB Green
        make_arrow(midpoints[c(1,3),], "V", "B", F) + adjv,            # Off to OOB Barents 
        make_arrow(midpoints[c(4,2),], "V", "B", T) + adjv,            # In to OOB Barents
        make_arrow(midpoints[c(10,8),], "V", "G", T) + adjv,           # In to OOB Green
        make_arrow(midpoints[c(1,2),], "H", "B", F) - adjv - adjh,     # Deep to OOB Barents
        make_arrow(midpoints[c(7,8),], "H", "G", F) -adjv + adjh)      # Deep to OOB Green

## Combine in a plot
  schematic <- ggplot() +
  geom_polygon(data = box_all, aes (x=x, y=y, group = box, fill = Compartment), colour = "black") +
  geom_text(data = labels, aes(x=x, y=y, label = label), size = 2.5) +
 # geom_point(data = midpoints, aes(x=x, y=y), colour = "red") +
  geom_segment(data = arrows, aes(x=x, y=y, xend=xend, yend=yend),
               arrow = arrow(length = unit(0.2, "cm"))) +
  theme_void() +
  scale_fill_manual(values = colours, drop = FALSE) +
  theme(legend.position = "left") +
  coord_equal(ratio = 1)
schematic

 combined <- ggarrange(map, schematic, ncol = 2, widths = c(1,2))       # Combine map and schematic

#ggsave("MiMeMo Schematic.png", plot= combined, scale = 1, width = 18, height = 6, units = "cm", dpi = 500)


