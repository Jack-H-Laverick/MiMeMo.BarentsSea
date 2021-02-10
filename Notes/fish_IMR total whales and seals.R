
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

gear <- read.csv("./Data/MiMeMo gears.csv")                                   # Import gear names

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                        # Import guild names
  dplyr::select(Guild, IMR.code) %>%                                          # Limit to IMR system
  drop_na() %>%                                                               # Drop those without an IMR code
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(IMR.code) %>%                                                      # 1 duplicated IMR code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%     # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

IMR_Regions <- rgdal::readOGR(dsn="./Data/IMR/Regions/") %>%                  # Import IMR regions shapefile
  st_as_sf() %>%                                                              # Convert to SF
  st_join(Domains) %>%                                                        # Which regions are in the model domain?
  drop_na() %>%                                                               # Drop those outside
  dplyr::select(Region = havomr) %>%                                          # Select and rename region column
  distinct(Region)                                                            # Keep unique shapes


IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(RE = "character", HO = "character")) %>% # Overwriting default column types  
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Region", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>% # Set column names
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region, IMR.code, "Weight") %>% # Ditch unnecessary columns
  left_join(gear) %>%                                                         # Attach gear labels
  left_join(guild) %>%                                                        # Attach guild labels
  filter(Aggregated_gear != "Dropped",            # Limited to gears and regions of interest
         between(Year, 2011, 2019)) %>%                                       # To when the electronic reporting system started to the last complete year
  mutate(Weight = Weight/1000)

#### Get each group ####

seals <- read.csv("./Data/IMR/IMR species list_2.csv") %>% 
  filter(str_detect(Engelsk.navn, "seal|Walrus")) %>% 
  dplyr::select(IMR.code = Tall, Common.name = Engelsk.navn) %>% 
  mutate(Guild = "Pinnipeds") %>% 
  left_join(IMR, by = "IMR.code")

whales <- read.csv("./Data/IMR/IMR species list_2.csv") %>% 
  filter(str_detect(Engelsk.navn, "whale|dolphin|porpoise")) %>% 
  dplyr::select(IMR.code = Tall, Common.name = Engelsk.navn) %>% 
  mutate(Guild = "Cetacean") %>% 
  left_join(IMR, by = "IMR.code")

macroalgae <- read.csv("./Data/IMR/IMR species list_2.csv") %>% 
  filter(str_detect(Norsk.navn, "tang|tare|salat|alger")|
         str_detect(Engelsk.navn, "weed")) %>% 
  dplyr::select(IMR.code = Tall, Common.name = Engelsk.navn) %>% 
  mutate(Guild = "Macroalgae") %>% 
  left_join(IMR, by = "IMR.code")

all <- rbind(seals, whales, macroalgae) %>% 
  drop_na(Weight) %>% 
  right_join(expand.grid(Guild.x = c("Pinnipeds", "Cetacean", "Macroalgae"),
                         Year = 2011:2019)) %>% 
  replace_na(list(Weight = 0))

check_species <-distinct(dplyr::select(all, IMR.code, Common.name))

IMR_totals <- group_by(all, Guild.x, Year) %>% 
  summarise(Weight = sum(Weight, na.rm = T))

ggplot(IMR_totals) +
  geom_line(aes(x = Year, y = Weight)) +
  facet_grid(rows = vars(Guild.x), scales = "free_y")

IMR_BS <- filter(all, Region %in% IMR_Regions$Region) %>% 
  group_by(Guild.x, Year) %>% 
  summarise(Weight = sum(Weight, na.rm = T))


#### Get other source ####

files <- list.files("./Data/rafisklag", full.names = T, pattern = ".csv")

ts <- map_df(files, ~{
  
  import <- read.csv(.x, row.names = NULL, blank.lines.skip = T, header = F, stringsAsFactors = F) %>% 
    dplyr::select(2:5) %>% 
    drop_na() %>% 
    setNames(c("Flag", "Group", "Species", "Tonnes")) %>% 
    mutate(meta = .x) %>% 
    separate(meta, into = c("Region", "Year"), sep = "_") %>% 
    mutate(Year = as.numeric(str_remove(Year, ".csv")),
           Region = str_remove(Region, "./Data/rafisklag/")) %>% 
    filter(Group %in% c("Alg", "Kval og sel")) }) %>% 
  mutate(Region = ifelse(Region == "All", "All", "Barents Sea"),
         Guild.x = case_when(Species == "" ~ "Macroalgae",
                             Species == "Kval" ~ "Cetacean",
                             Species == "Sel" ~ "Pinnipeds")) %>% 
  group_by(Region, Guild.x, Year) %>% 
  summarise(Tonnes = mean(as.numeric(Tonnes))) %>% 
  ungroup() %>% 
  full_join(expand.grid(Guild.x = unique(.$Guild.x), 
                        Year = 2010:2019,
                        Region = unique(.$Region))) %>% 
  replace_na(list(Tonnes = 0,
                  Region = "Barents Sea"))



ggplot() +
  geom_line(data = IMR_totals, aes(x = Year, y = Weight)) +
  geom_line(data = IMR_BS, aes(x = Year, y = Weight), colour = "green") +
  facet_grid(rows = vars(Guild.x), scales = "free_y") +
  geom_line(data = ts, aes(x = Year, y = Tonnes, colour = Region)) +
  scale_x_continuous(breaks = seq(2010, 2019, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.25))

## Upshot
## Seals and Algae come from rafisklag. Whales from IMR only. 