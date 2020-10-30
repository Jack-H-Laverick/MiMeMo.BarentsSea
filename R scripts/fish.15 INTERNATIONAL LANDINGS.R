
## Combine EU and Norwegian landings, then inflate by missing Russian landings to get International landings

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse")                                                  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

Guilds <- unique(read.csv("./Data/MiMeMo fish guilds.csv")$Guild)           # Get vector of guilds

Gears <- unique(read.csv("./Data/MiMeMo gears.csv")$Aggregated_gear)        # Get vector of gears

target <- expand.grid(Guild = Guilds, Aggregated_gear = Gears)              # Get combinations of gear and guild

Inflation <- readRDS("./Objects/ICES landings inflation.rds") %>%           # Rule to convert non-russian to international landings from ICES
  right_join(data.frame(Guild = Guilds)) %>%                                # Introduce missing guilds
  replace_na(replace = list(Inflation = 1)) %>%                             # Any unrepresented guild shouldn't be inflated
  arrange(Guild)                                                            # Alphabetise to match matrices later
         
#### Convert EU landings to a matrix by guild and gear ####

EU <- readRDS("./Objects/Landings EU.rds") %>%                              # Get landings
  group_by(Guild, Aggregated_gear) %>%                                      # Per combination of gear and guild
  summarise(landings = mean(Tonnes, na.rm= TRUE)) %>%                       # Average landings across years
  right_join(target) %>%                                                    # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the uneeded gear class  
  replace_na(replace = list(landings = 0)) %>%                              # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = landings) %>%     # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

#### Convert IMR landings to a matrix by guild and gear ####

IMR <- readRDS("./Objects/IMR landings by gear and guild.rds") %>%          # Get landings
  right_join(target) %>%                                                    # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the uneeded gear class
  replace_na(replace = list(Tonnes = 0)) %>%                                # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Tonnes) %>%       # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

#### Combine EU and IMR landings then inflate to international ####

International <- t(Inflation$Inflation * (EU + IMR))                        # Sum EU and IMR landings then inflate by Russian activity

saveRDS(Internationl, "./Objects/International landings.rds")
