
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

Inflation <- readRDS("./Objects/ICES landings inflation.rds") %>%           # Rule to convert non-russian to international landings from ICES
  right_join(data.frame(Guild = Guilds)) %>%                                # Introduce missing guilds
  replace_na(replace = list(Inflation = 1)) %>%                             # Any unrepresented guild shouldn't be inflated
  arrange(Guild)                                                            # Alphabetise to match matrices later

IMR <- readRDS("./Objects/IMR landings by gear and guild.rds")              # Import corrected IMR landings

Nor_seals <- readRDS("./Objects/rafisklag landings by gear and guild.rds")  # Import Norwegian seal catch

EU <- readRDS("./Objects/EU landings by gear and guild.rds")                # Import corrected EU landings
  
#### Combine EU and IMR landings then inflate to international ####

International <- t(((EU + IMR) *                                            # Sum EU and IMR landings
                     Inflation$Inflation) +                                 # then inflate by Russian activity,
                     Nor_seals)/                                            # add seals
                     domain_size                                            # and convert to per m^2

International["Seines", "Macrophyte"] <- 0                                  # There's one tiny bit of seaweed we think should be removed.
#International["Recreational", "Demersal (quota-limited)"] <- ??             # Add recreational fishing activity.

heatmap(International)

saveRDS(International, "./Objects/International landings.rds")
