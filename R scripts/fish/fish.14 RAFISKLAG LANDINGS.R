
# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

library(tidyverse)

gears <- unique(read.csv("./Data/MiMeMo gears.csv")$Aggregated_gear)          # Load fishing gear classifications

guild <- unique(read.csv("./Data/MiMeMo fish guilds.csv")$Guild)              # Get guilds

landings_target <- expand.grid(Guild = guild, 
                               Aggregated_gear = gears) %>%                   # Get combinations of gear and guild
filter(Aggregated_gear != "Dropped") %>%                                      # Ditch the uneeded gear class
  mutate(Tonnes = 0) %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = Tonnes) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns

#### Extract whale, pinniped, and macroalgae landings ####

files <- list.files("./Data/rafisklag", full.names = T, pattern = ".csv")

ts <- map_df(files, ~{
  
  import <- read.csv(.x, row.names = NULL, blank.lines.skip = T, header = F, stringsAsFactors = F) %>% 
    select(2:5) %>% 
    drop_na() %>% 
    setNames(c("Flag", "Group", "Species", "Tonnes")) %>% 
    mutate(meta = .x) %>% 
    separate(meta, into = c("Region", "Year"), sep = "_") %>% 
    mutate(Year = as.numeric(str_remove(Year, ".csv")),
           Region = str_remove(Region, "./Data/rafisklag/")) %>% 
    filter(Group %in% c("Alg", "Kval og sel")) }) %>% 
  mutate(Region = ifelse(Region == "All", "All", "Barents Sea"),
         Species = case_when(Species == "" ~ "macroalgae",
                             Species == "Kval" ~ "cetaceans",
                             Species == "Sel" ~ "pinnipeds")) %>% 
  group_by(Region, Species, Year) %>% 
  summarise(Tonnes = mean(as.numeric(Tonnes))) %>% 
  ungroup() %>% 
  full_join(expand.grid(Species = unique(.$Species), 
                        Year = 2010:2019,
                        Region = unique(.$Region))) %>% 
  replace_na(list(Tonnes = 0,
                  Region = "Barents Sea"))

ggplot(ts) +
  geom_line(aes(x = Year, y = Tonnes, colour = Region)) +
  facet_grid(rows = vars(Species), scales = "free_y") +
  labs(caption = "Barents Sea = Finnmark and Troms") +
  scale_x_continuous(breaks = seq(2010, 2019, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.25))
        
#### Add the values to the landings matrix ####

summary <- filter(ts, Region == "Barents Sea") %>%                       # For the Barents Sea
  group_by(Species) %>%                                                  # By guild 
  summarise(Tonnes = mean(Tonnes))                                       # Get average annual landings

## Add birds and pinnipeds

landings_target["Macrophyte", "Kelp harvesting"] <- filter(summary, Species == "macroalgae")$Tonnes
landings_target["Pinnipeds", "Rifles"] <- filter(summary, Species == "pinnipeds")$Tonnes
#landings_target["Cetacean", "Harpoons"] <- filter(summary, Species == "cetaceans")$Tonnes # We've decided this would be double accounting with IMR data.

saveRDS(landings_target, "./Objects/rafisklag landings by gear and guild.rds")

