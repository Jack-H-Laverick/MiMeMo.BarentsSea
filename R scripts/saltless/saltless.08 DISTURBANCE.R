
# Extract natural disturbance by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster")                   # List packages
lapply(Packages, library, character.only = TRUE)                           # Load packages
source("./R scripts/@_Region file.R")

habitats <- readRDS("./Objects/Habitats.rds") %>%                          # Import maps of sea bed habitats
  filter(Habitat != "Rock")

data <- list(gravel = brick("../Sediment/Output/Daily_disturbance_gravel.nc"), # Import daily disturbance for sediment fractions
             sand = brick("../Sediment/Output/Daily_disturbance_sand.nc"),              
             silt = brick("../Sediment/Output/Daily_disturbance_silt.nc"))              

# Import sediment fraction proportions at each pixel

weights <- map(c("Rock", "Gravel", "Sand", "Silt"), ~{
  
weights <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc",
                  varname = .x) %>% 
  projectRaster(data[["silt"]])

  weights[is.na(weights)] <- 0 

  return(weights)
})

#### Get variable to label the day of each raster layer ####

raw <- ncdf4::nc_open("../Sediment/Objects/tides.nc")                      # Open file containing all the data
time <- raw$dim$time$len                                                   # Pull the length of the time dimension
ncdf4::nc_close(raw)                                                       # Close file

days <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = time) %>%# Calculate time steps for the dataset
  format("%Y%m%d") %>% 
  unique()

#### Calculate mean disturbance per habitat area, weighting by cell coverage and sediment fraction proportion ####

scale <- exact_extract(stack(weights), habitats, fun = 'mean') %>%         # Mean habitat disturbance weighted by sediment fraction per pixel
  setNames(c("Rock", "Gravel", "Sand", "Silt")) %>% 
  cbind(st_drop_geometry(habitats)) %>% 
  pivot_longer(-c(Habitat, Shore), names_to = "Fraction", values_to = "Proportion")

mobile <- map2_df(data, weights[2:4], ~{
  
values <- exact_extract(.x, habitats, fun = 'weighted_mean', weights = .y) %>% # Mean habitat disturbance weighted by sediment fraction per pixel
  t() %>%                                                                  # Swap habitats to columns and days to rows
  as.data.frame() %>%                                         
  setNames(paste0(habitats$Habitat, " ", habitats$Shore)) %>%              # Get habitat types as column names
  rownames_to_column(var = "Day") %>%                                      # Get day as a column
  mutate(Day = days[as.numeric(str_remove(Day, "weighted_mean.X"))],       # Clean the column
         Fraction = names(.y)) %>% 
  pivot_longer(-c(Day, Fraction), names_to = "Habitat", values_to = "Disturbance") %>%  # Collect habitat types into a column
  separate(Habitat, into = c("Habitat", "Shore"))                          # Regain the habitat meta-data
  }) %>% separate(Fraction, into = c(NA,NA,NA, "Fraction")) %>% 
  mutate(Fraction = str_to_title(Fraction))

rock_summary <- filter(mobile, Fraction == "Gravel") %>% 
  mutate(Fraction = "Rock",
         Disturbance = 0)

summary <- bind_rows(mobile, rock_summary) %>%                             # Add in Rock disturbance (0)
  left_join(scale) %>%                                                     # Attach weights of sediment classes within habitats
  group_by(Habitat, Shore, Day) %>% 
  summarise(Disturbance = weighted.mean(Disturbance, Proportion)) %>%      # Get the habitat-wide average by averaging sediment specific rates by sediment proportions per day
  ungroup() %>% 
  mutate(Month = lubridate::month(as.Date(Day, format = "%Y%m%d"))) %>%    # Convert days to months
  group_by(Month, Habitat, Shore) %>% 
  summarise(Disturbance = mean(Disturbance))                               # Average over months

saveRDS(summary, "./Objects/Habitat disturbance.rds")

ggplot(summary) +
  geom_line(aes(x = as.numeric(as.factor(Month)), y = Disturbance*100, colour = Shore, linetype = Habitat)) +
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  labs(y = "Time disturbed (%)", x = "Month") +
NULL

ggsave("./Figures/saltless/Habitat disturbance.png", width = 16, height = 8, units = "cm")
