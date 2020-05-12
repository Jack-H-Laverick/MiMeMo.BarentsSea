
# readRDS("./Objects/Months/")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("MiMeMo.tools", "tidyverse", "viridis", "data.table", "furrr", "tictoc", "patchwork", "ggnewscale") # List handy data packages
Geo_packages <- c("stars", "sf", "rnaturalearth")                           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

Transects <- list(S = readRDS("./Objects/Boundary_transects.rds") %>%       # Import transects to sample at
                      filter(Neighbour == "Ocean"),                         # Keep only transects on the domain perimeter
                  D = readRDS("./Objects/Boundary_transects.rds") %>%       # Import transects to sample at
                      filter(Shore == "Offshore" & Neighbour == "Ocean"))   # The deep layer of the model only has exchange between offshore and the sea

grid <- readRDS("./Objects/Fixed_grid.rds")                                 # Joining data to the grid reorders the datapoints
lat <- matrix(grid$Latitude, nrow=235, ncol=190)                            # Reshape to a matrix for stars
lon <- matrix(grid$Longitude, nrow=235, ncol=190)                                  

vars <- c("DIN", "Chlorophyll", "Temperature", "Salinity")                  # Variables to sample

#### Create a spatial object to bind data to ####

raster <- pull(grid, Bathymetry) %>%                                        # Extract the values in a current column
  matrix(nrow=235, ncol=190) %>%                                            # Convert current data to matrix for stars
  st_as_stars() %>%                                                         # Set up stars object
  st_as_stars(curvilinear=list(X1 = lon, X2 = lat)) %>%                     # Pass coordinate matrices and state the grid is curved
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                            # geom_stars doesn't like a curvilinear grid, convert each cell to an SF polygon
  st_transform(crs = crs)                                                   # Reproject

Data_small <- readRDS("./Objects/Months/NM.1.1980.rds") %>% 
  ungroup() %>%                                                             # Looks like I forgot to ungroup before saving
  select(Bathymetry, Longitude, Latitude, Depth) %>% 
  filter(Depth == "S") %>%                                                  # Seperate shallow and deep data
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Specify original projection (crs) 
  st_transform(crs = crs) 

cells <- st_join(raster, Data_small) %>%  
  drop_na() %>%                                                             # Shrink grid to NM dataset
  select(Longitude, Latitude)

#### Intersections ####

Intersections <- list(S = st_intersects(Transects[["S"]], cells),
                      D = st_intersects(Transects[["D"]], cells))

#### Extract water exchanges between horizontal compartments ####
  
tic() 
OOB <- list.files("./Objects/Months/", full.names = T) %>%                  # Get the names of all data files
   future_map(Sample_OOB, variables = vars, transects = Transects,
              intersections = Intersections, .progress = T) %>%             # Sample the currents in each file and aggregate
   rbindlist()                                                              # Bind into a dataframe

saveRDS(OOB, "./Objects/Boundary measurements.rds")                         # Save
toc()

#### Plotting ####    

ggplot(OOB) + geom_line(aes(x= Date, y = Measured, colour = Compartment, group = Compartment), alpha = 0.5) +
  facet_grid(rows = vars(Variable), scales = "free_y") +
  theme_minimal() +
  labs(y = "Measured at ocean boundary", caption = "Average NEMO-MEDUSA outputs along our model perimeter") +
  theme(legend.position = "top")

ggsave("./Figures/flows/Boundary variables.png", last_plot(), dpi = 500, width = 18, height = 10, units = "cm")
 
#### Silly plot ####

data <- OOB %>% 
  filter(Year == 1980, Month == 1) %>% 
  group_by(Variable) %>% 
  mutate(Max = max(Measured),
         font_colour = Measured == Max) %>% 
  ungroup() %>% 
  split(list(.$Variable)) 

dot_plot <- function(data) {
  
ggplot(data) +
  geom_point(aes(x = Shore, y = Depth, size = Max), colour = "grey", show.legend = F) +
  geom_vline(xintercept = 2, colour = "grey") +
  geom_hline(yintercept = 2, colour = "grey") +
  geom_point(aes(x = Shore, y = Depth, size = Max), alpha = 0.2, show.legend = F) +
  geom_point(aes(x = Shore, y = Depth, size = Measured, colour = Measured)) +
  scale_colour_viridis(name = "Measured") +
  new_scale_color() + 
  geom_text(aes(x = Shore, y = Depth, label = round(Measured, 2), colour = font_colour)) + 
  scale_colour_manual(values = c("White", "Black")) + 
  geom_text(x = 1, y = 1, aes(label = Variable)) + 
  scale_size(range = c(15, 40)) +
  guides(size = FALSE) +
  #facet_wrap(vars(Variable)) + 
  theme_minimal() +
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "None", 
        panel.grid.major = element_blank())
}

plots <- map(data, dot_plot)

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]

ggsave("./Figures/flows/Boundary dots.png", last_plot(), dpi = 500, width = 18, height = 18, units = "cm")
