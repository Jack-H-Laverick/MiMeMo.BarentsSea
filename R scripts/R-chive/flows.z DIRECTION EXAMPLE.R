
# Small example to prove direction works

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "sf", "egg")
lapply(Packages, library, character.only = TRUE)      # Load packages

#### Set up spatial environment ####

 NW <- st_sfc(st_point(x = c(0,0)))                                # Specify an SF point
 NE <- st_sfc(st_point(x = c(1,0)))
 SE <- st_sfc(st_point(x = c(1,1)))
 SW <- st_sfc(st_point(x = c(0,1)))
 
 corners <- st_sf(corner = c("NW", "NE", "SE", "SW"), c(NW, NE, SE, SW), crs = 4326) %>% st_transform(3035) # Create an SFC object of projected corners
 
 N <- st_union(corners[1,], corners[2,]) %>% st_cast("LINESTRING") # combine two corners to make a line (box edge)
 E <- st_union(corners[2,], corners[3,]) %>% st_cast("LINESTRING") # If there were more you could wrap this in a function
 S <- st_union(corners[3,], corners[4,]) %>% st_cast("LINESTRING")
 W <- st_union(corners[4,], corners[1,]) %>% st_cast("LINESTRING")
 
 box <- rbind(N, E, S, W) %>% mutate(current = c("Meridional", "Zonal", "Meridional", "Zonal")) # Combine into single object
 polygon <- box %>% st_union %>% st_polygonize() %>% st_sf(Name = "Model", crs = 3035) # Use lines to make a closed polygon
 
 ggplot() +                                                        # Illustrate what I built as a test
   geom_sf(data = polygon, fill = "yellow") +
   geom_sf(data = box, colour = "blue", size = 1) +
   geom_sf(data = corners, colour = "red", size = 3) + 
   theme_minimal()
 
#### testing function to detect direction of current flows (in or out of model box) ####
 
 direction_test <- function(segment) {
   
   midpoint <- st_line_sample(box[segment,], n = 1)           # Grab the midpoint of a line segment
   
   domain <- polygon
   
   if(box[segment,]$current == "Meridional") {
     minus <- c(0, -0.001)                                        # Adjust for point below the segment
     plus <-  c(0, +0.001)                                        # Adjust for point above the segment
     flow_shift <- c(+100, 0)                                     # Adjust for current indicator
     flow_plot <- geom_segment(aes(xend= min(flow[,"X"]), y = min(flow[,"Y"]), # PLotting line for current indicator 
                                   x = max(flow[,"X"]), yend = max(flow[,"Y"])), arrow = arrow())
   }            # Change the shift used for test point relative midpoint
   if(box[segment,]$current == "Zonal")      {
     minus <- c(-0.001, 0)
     plus <-  c(+0.001, 0)
     flow_shift <- c(0, +100)
     flow_plot <- geom_segment(aes(x = min(flow[,"X"]), y = min(flow[,"Y"]), 
                                   xend = max(flow[,"X"]), yend = max(flow[,"Y"])), arrow = arrow()) 
   }            # Based on current of interest
   
   coords <- midpoint %>% st_transform(4326)                      # Transform to lat-lon to ensure test points are perpendicular to the line
   
   mid_minus <- coords + minus                                    # Shift from the mid point
   st_crs(mid_minus) <- 4326                                      # set crs
   mid_minus <- st_transform(mid_minus, 3035) %>%                 # change crs for plotting
     st_cast("POINT")
   
   mid_plus <- coords + plus                                      # Shift from the mid point
   st_crs(mid_plus) <- 4326                                       # set crs
   mid_plus <- st_transform(mid_plus, 3035) %>%                   # change crs for plotting
     st_cast("POINT")
   
   flow <- st_union(x = mid_plus, y = mid_minus) %>%              # Link and shift points to make an arrow to illustrate flow
     st_cast("LINESTRING") %>% 
     + flow_shift
   st_crs(flow) <- 3035 
   flow <- st_coordinates(flow)
   
   test <- st_sf(geometry = c(mid_plus, mid_minus), side = c("plus", "minus")) %>% # Create a full SF object to allow point in polygon analysis
     st_join(domain, join = st_intersects) %>%                    # Are the points either side of the boundary in a domain polygon?
     mutate(Contained = if_else(is.na(Name), "out", "in")) %>%  # Label the points 
     mutate(Flip = if_else(side == "plus" & Contained == "out" |  # Determine whether positive currents flow in or out of the domain and so if they need to be flipped
                             side == "minus" & Contained == "in", TRUE, FALSE))
   
   window <- st_bbox(polygon)                           # Get a zoom which fits the segment of interest
   
   ggplot() + 
     geom_sf(data = domain, fill = "yellow") +
     geom_sf(data = box[segment,]) + 
     geom_sf(data = midpoint, colour = "red") + 
     #geom_sf(data = test, aes(colour = Contained), show.legend = "point") + 
     flow_plot +        # The arrow still doesn't plot perpendicularly for all locations on the projection, but the tests are working fine
     theme_minimal() +
     labs(x = NULL, y = NULL, title = paste0("Current component = ", box[segment,]$current), 
          subtitle = paste0("Should currents be flipped\n(for + numbers into polygon)? = ", test$Flip[1])) +
     coord_sf(xlim = c(window$xmin, window$xmax), ylim = c(window$ymin, window$ymax))
   
   #  return(test$Flip)
 }                         # Test whether positive currents flow in or out of the model domain at a segement
 
 ex1 <-direction_test(1)
 ex2 <-direction_test(2)
 ex3 <-direction_test(3)
 ex4 <-direction_test(4)
 
 examples <- ggarrange(ex1, ex2, ex3, ex4, nrow = 2)               # Facet for weekly meeting
 
 #ggsave("./Figures/flows/curent direction example.png", plot = examples, width = 32, height = 20, units = "cm", dpi = 500)

