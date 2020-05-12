
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("MiMeMo.tools", "tidyverse", "viridis", "furrr", "sf", "rnaturalearth") # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Instruction for parallel processing

domains <- readRDS("./Objects/Domains.rds")                                 # Load SF polygons of the MiMeMo model domains

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Check how much overlap there is between domain boundaries ####

overlap <- ggplot() +                                                                  
   geom_sf(data = world, size = 0.1, fill = "black", colour = "black") +
   geom_sf(data = domains, fill = NA, colour = "white", size = 0.1) +       # overlap with coast?
   geom_sf(data = domains, fill = NA, size = 0.1) + 
   zoom +
   theme_minimal() +
   theme(axis.text = element_blank()) +
   labs(caption = "Checking polygon overlap") +
   NULL

ggsave_map("./Figures/flows/boundaries.png", overlap)                     # Overlap has been fixed!
# Decided to use the Offshore polygon, use the inshore polygon for Inshore to out of bounds inshore flows only 

#### Break up polygon ####

Edges <- st_cast(domains, "MULTILINESTRING", group_or_split = TRUE) %>%    # Simplify polygon to mutli-linestrings
  st_cast("LINESTRING", group_or_split = TRUE) %>%                         # Split line into it's own row 
  split(., f = list(.$Shore)) %>%                                          # Separate out by zone
  future_map(boundaries, crs = crs, .progress = T)                         # Break the linestrings of a domain into transects
 
 Inshore_plot <- ggplot() +                                                # Check we're getting the inshore edges correctly
  geom_sf(data = Inshore_ocean_boundaries, colour = "black", fill = "black") +                  
  geom_sf(data = Edges[["Inshore"]], colour = "red") +                  
  geom_sf(data = Edges[["Offshore"]], colour = "yellow") +                  
  theme_minimal() +
  scale_colour_viridis() +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(caption = paste0("Retain only the inshore transects at the Inshore-Ocean\n
                         boundaries (red, over black). Specify the sampling polygons in the region file")) +
  NULL
 
ggsave_map("./Figures/flows/Inshore-Ocean boundary.png", Inshore_plot)

Inshore <- st_intersects(Edges[["Inshore"]], Inshore_ocean_boundaries) %>%  # Which Inshore edges are against open ocean (Region file)
  as.data.frame()

Edges[["Inshore"]] <- Edges[["Inshore"]][Inshore$row.id,]                   # Drop inshore transects which are on land, or duplicate offshore to inshore

saveRDS(Edges, "./Objects/Split_boundary.rds")

Transects <- ggplot() +                                                     # Show the code worked
  geom_sf(data = Edges[["Offshore"]], aes(colour = Segment)) +                  
  geom_sf(data = Edges[["Inshore"]], colour = "red") +                  
  theme_minimal() +
  scale_colour_viridis() +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(caption = "Individual transects to sample boundary fluxes\n 
                  Inshore-Ocean boundaries are in red") +
  NULL

ggsave_map("./Figures/flows/Segments.png", Transects)

