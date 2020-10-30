
## Combine EU and Norwegian fishing effort, then inflate by missing Russian landings to get International effort

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

##**## Need to get guild in EU summary

EU <- readRDS("./Objects/EU effort.rds") %>%                                # Get landings
  group_by(Guild, Aggregated_gear) %>%                                      # Per combination of gear and guild
  summarise(effort = mean(Tonnes, na.rm= TRUE)) %>%                         # Average landings across years
  right_join(target) %>%                                                    # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the uneeded gear class  
  replace_na(replace = list(effort = 0)) %>%                                # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = effort) %>%       # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

#### Convert IMR landings to a matrix by guild and gear ####

IMR <- readRDS("./Objects/IMR effort by gear and guild.rds") %>%            # Get landings
  group_by(Guild, Aggregated_gear) %>%                                      # Collate IMR regions and 
  summarise(Effort = sum(Effort, na.rm = T)) %>% 
  right_join(target) %>%                                                    # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the uneeded gear class
  replace_na(replace = list(Effort = 0)) %>%                                # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Effort) %>%       # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

#### Combine EU and IMR landings then inflate to international ####

International <- t(Inflation$Inflation * (EU + IMR)) %>%                    # Sum EU and IMR landings then inflate by Russian activity
  rowSums()
  
saveRDS(Internationl, "./Objects/International effort by gear.rds")
