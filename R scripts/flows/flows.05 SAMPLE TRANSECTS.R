
# readRDS("./Objects/Months/.")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain
library(MiMeMo.tools)
library(furrr)
source("./R scripts/@_Region file.R")                                       # Define project region 
plan(multisession)                                                          # Choose the method to parallelise by with furrr

Transects <- readRDS("./Objects/Boundary_transects.rds")                    # Import transects to sample at

#### Summarise along transects ####

Summary <- list.files("./Objects/Months/", full.names = T) %>%              # Get the names of all data files
   future_map(NM_boundary_summary, Transects, .progress = T)                # Sample NM output along domain boundary

#### Save water exchanges between compartments ####

Flows <- map(Summary, `[[`, 1) %>%                                          # Subset the summary results
  data.table::rbindlist() %>% 
  saveRDS("./Objects/H-Flows.rds")                                          

#### Save boundary conditions ####

Boundary <- map(Summary, `[[`, 2) %>%                                       # Subset the summary results
  data.table::rbindlist()
saveRDS(Boundary, "./Objects/Boundary measurements.rds")                         

ggplot(Boundary) + geom_line(aes(x= Date, y = Measured, colour = Compartment, group = Compartment), alpha = 0.5) +
  facet_grid(rows = vars(Variable), scales = "free_y") +
  theme_minimal() +
  labs(y = "Measured at ocean boundary", caption = "Average NEMO-MEDUSA outputs along our model perimeter") +
  theme(legend.position = "top")

ggsave("./Figures/flows/Boundary variables.png", last_plot(), dpi = 500, width = 18, height = 10, units = "cm")
 