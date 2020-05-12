
##**## A tidy place to keep functions and reduce clutter in programmes

#### Global Fishing Watch Data Extraction ####

Arctic_boats <- function (Data)                {
  Fish <- Data %>%                                                            # Take first csv file as a test
    mutate(Date = date) %>%                                            # We need Lon-lat to be the first two columns so move date column
    mutate(date = lon_bin/100, lat_bin = lat_bin/100)                 # Move Coordinate columns, and divide by 100 because raw data misses decimal point
  
  colnames(Fish)[2] <- "Latitude"                                             # Change column names for GIS functions
  colnames(Fish)[1] <- "Longitude"
  
  coordinates(Fish) <- ~ Longitude + Latitude                                 # Converts to a shapefile
  
  proj4string(Fish) <- proj4string(FAO_arctic)                                # Set the projection of the points to match the polygons
  
  # Filter the data to rows where polygons lie over the fishing data points
  inside.Arctic <- Fish[!is.na(over(Fish, as(FAO_arctic, "SpatialPolygons"))),] %>%
    fortify(inside.Arctic)                                    # Coerce for ggplot
  return(inside.Arctic) }     # Function to clip boat pings to the FAO regions of interest
FAO_boats <- function (Data, Clip)             {
  
  colnames(Data)[2] <- "Latitude"                                             # Change column names for GIS functions
  colnames(Data)[1] <- "Longitude"
  name <- deparse(substitute(Clip))                                           # Pull object name to attach as a column on the output
  
  coordinates(Data) <- ~ Longitude + Latitude                                 # Converts to a shapefile
  
  proj4string(Data) <- proj4string(Clip)                                      # Set the projection of the points to match the polygons
  
  # Filter the data to rows where polygons lie over the fishing data points
  inside.Arctic <- Data[!is.na(over(Data, as(Clip, "SpatialPolygons"))),] %>%
    fortify(inside.Arctic) %>%                                # Coerce for ggplot
    mutate(Region = name) 
  return(inside.Arctic) }     # Function to clip boat pings to specific FAO regions

#### Global fishing watch plotting ####

jacks_lazy_avg <- function (gear, limit)       {
  plot <- ggplot() +
    geom_point(data = filter(Seasonal, geartype == gear & fishing >= limit), 
               aes(x = long, y = lat), colour = "red", alpha = 0.1, size = 0.1, shape=16, stroke = 0) + # shape and stroke get the smallest point possible
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black") +
    geom_polygon(data = FAO_arctic, aes(long, lat, group = group), colour = "black", fill = "NA", size = 0.1) +                                                        # Plots polygons with holes
    coord_fixed(1.3, xlim = c(min(-50), max(75)),
                ylim = c(min(50), max(90))) +
    theme_minimal() +
    labs(x = 'Longitude (W)', y = 'Latitude (N)', 
         title = paste(gear, "activity longer than", limit, "hours in the Arctic (2012-2016)", sep = " ")) +
    theme(legend.position = "None") +
    NULL
  plot
  
  ggsave(paste("./Figures/GFW/", gear,".png"), plot = plot, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
  
  return(plot)
}     # Function to plot data by geartype and filtered to minimum fishing activity
jacks_lazy_animation <- function (gear, limit) {
  plot <- ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black", colour = NA) +
    geom_point(data = filter(Seasonal, geartype == gear & fishing >= limit), 
               aes(x = long, y = lat),  colour = "red", size = 0.1, shape=16, stroke = 0) + # shape and stroke get the smallest point possible
    geom_polygon(data = FAO_arctic, aes(long, lat, group = group), colour = "black", fill = "NA", size = 0.1) +                                                        # Plots polygons with holes
    coord_fixed(1.3, xlim = c(min(-50), max(75)),
                ylim = c(min(50), max(90))) +
    theme_minimal() +
    labs(x = 'Longitude (W)', y = 'Latitude (N)', 
         title = paste(gear, "activity longer than", limit, "hours in the Arctic (2012-2016)", sep = " ")) +
    theme(legend.position = "None") +
    transition_events(start = Month-1, enter_length = 1, exit_length = 1) +          # It fails if your just provide month. I think it's because the frame count drops off the end of the cycle
    enter_fade() +
    exit_fade() +
    labs(subtitle = "Month: {round(frame_time)+1}") +                                         # Comment out to make static
    NULL
  
  gganimate::animate(plot, width = 16, height = 10, res = 300, units = "cm", fps = 10, nframes = 100)                                   # Run animation - user specified
  anim_save(paste("./Figures/GFW/GIF2 ", gear), animation = last_animation())    # Save animation
  # return(plot) uncomment this and comment out two lines above if you only want to return a single animation
}     # Animate the map through months

jacks_polar_static <- function (gear, limit)   {
  plot <- ggplot() +
    # bathymetry
    geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
    scale_colour_viridis(name = 'Depth (m)', discrete = TRUE,
                         guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
    # boats
    geom_sf(data = filter(Seasonal_p, geartype == gear & fishing >= limit), 
            colour = "red", size = 0.1, shape=16, stroke = 0) + # shape and stroke get the smallest point possible
    # FAO
    geom_sf(data = FAO_arctic_p, colour = "black", fill = "NA", size = 0.1) +                                                        # Plots polygons with holes
    # world
    geom_sf(data = world_p) +
    
    coord_sf(xlim = c(7000000, 000000), ylim = c(7500000, 3500000)) +
    theme_minimal() +
    labs(title = paste(gear, "activity longer than", limit, "hours in the Arctic (2012-2016)", sep = " ")) +
    theme(legend.position = "None") +
    #    transition_events(start = Month-1, enter_length = 1, exit_length = 1) +          # It fails if your just provide month. I think it's because the frame count drops off the end of the cycle
    #    enter_fade() +
    #    exit_fade() +
    #    labs(subtitle = "Month: {round(frame_time)+1}") +                                         # Comment out to make static
    NULL
  ggsave(paste("./Figures/GFW/3", gear,".png"), plot = plot, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)
  return(plot)
}     # Plot polar maps
jacks_polar_animate <- function (gear, limit)  {
  plot <- ggplot() +
    # bathymetry
    #geom_sf(data = lines, aes(colour = level), stroke = 0, size = 0.2, show.legend = "line") +
    #scale_colour_viridis(name = 'Depth (m)', discrete = TRUE,
    #                     guide = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
    # ice
    geom_sf(data = filter(Ice_m, Sea_ice_avg > 0), aes(alpha = Sea_ice_avg), 
            colour = "turquoise", stroke = 0, size = 0.75, show.legend = "point") +
    scale_alpha(range = c(0,1), name = 'Sea Ice\n Concentration', guide = guide_legend(override.aes = list(linetype = "blank", shape = 16, size = 4)))+
    # boats
    geom_sf(data = filter(Seasonal_p, geartype == gear & fishing >= limit), 
            colour = "red", size = 0.1, shape=16, stroke = 0) + # shape and stroke get the smallest point possible
    # FAO
    #geom_sf(data = FAO_arctic_p, colour = "black", fill = "NA", size = 0.1) + 
    # Domains
    geom_sf(data = Greenland_off, aes(geometry = geometry), colour = "Yellow3", fill = NA, size=0.1) + 
    geom_sf(data = Greenland_in, aes(geometry = geometry), colour = "Yellow", fill = NA, size=0.1) + 
    geom_sf(data = Barents_in, aes(geometry = geometry), colour = "Yellow", fill = NA, size=0.1) + 
    geom_sf(data = Barents_off, aes(geometry = geometry), colour = "Yellow3", fill = NA, size=0.1) + 
    # world
    geom_sf(data = world_p) +
    
    coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
    theme_minimal() +
    labs(title = paste(gear, "activity longer than", limit, "hours in the Arctic (2012-2016)", sep = " ")) +
    theme(legend.position = "None") +
    enter_fade() +
    exit_fade() +
    transition_manual(Month) +   
    labs(subtitle = "Month: {current_frame}") +                                         # Comment out to make static
    NULL
  
  gganimate::animate(plot, width = 16, height = 10, res = 300, units = "cm", fps = 10, nframes = 100)                                   # Run animation - user specified
  anim_save(paste("./Figures/GFW/GIF3_domains", gear), animation = last_animation())    # Save animation
  # return(plot) uncomment this and comment out two lines above if you only want to return a single animation
}     # Animate polar maps