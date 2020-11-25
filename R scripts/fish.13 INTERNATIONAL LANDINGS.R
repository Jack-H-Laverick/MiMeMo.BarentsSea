
## Combine EU and Norwegian landings, then inflate by missing Russian landings to get International landings

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse")                                                  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need landings as tonnes per m^2
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()
  
Guilds <- unique(read.csv("./Data/MiMeMo fish guilds.csv")$Guild)           # Get vector of guilds

Gears <- unique(read.csv("./Data/MiMeMo gears.csv")$Aggregated_gear)        # Get vector of gears

target <- expand.grid(Guild = Guilds, Aggregated_gear = Gears)              # Get combinations of gear and guild

Inflation <- readRDS("./Objects/ICES landings inflation.rds") %>%           # Rule to convert non-russian to international landings from ICES
  right_join(data.frame(Guild = Guilds)) %>%                                # Introduce missing guilds
  replace_na(replace = list(Inflation = 1)) %>%                             # Any unrepresented guild shouldn't be inflated
  arrange(Guild)                                                            # Alphabetise to match matrices later

IMR <- readRDS("./Objects/IMR landings by gear and guild.rds")              # Import corrected IMR landings

EU <- readRDS("./Objects/EU landings by gear and guild.rds")                # Import corrected EU landings
  
#### Combine EU and IMR landings then inflate to international ####

International <- t(Inflation$Inflation * (EU + IMR))/domain_size            # Sum EU and IMR landings then inflate by Russian activity and conver to per m^2

heatmap(International)

saveRDS(International, "./Objects/International landings.rds")
