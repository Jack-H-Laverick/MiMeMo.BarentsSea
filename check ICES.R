
## Ratio of Russian landings to non-by guild

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse")                                                  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>% 
  dplyr::select(Guild, Category, Subcategory) %>% 
  distinct

#### Get an inflation factor for each guild to get from Non-Russian to International landings ####

ICES_old <-read.csv("./Data/ICES fish/ICES_1903-2017.csv", header=T) %>%        # Import combined ICES landings  
       filter(area_orig %in% c("I", "IIa", "I (not specified)", "II a (not specified)", "I a", "I and IIa (not specified)")) %>% 
       left_join(guild) %>%                                                 # Attach guilds
       filter(str_detect(Guild, "Demersal") &
              between(year, 2000, 2017)) %>% 
  group_by(year) %>% 
  summarise(tonnes_old_names = sum(tonnage))


ICES_new <-read.csv("./Data/ICES fish/ICES_1903-2017.csv", header=T) %>%        # Import combined ICES landings  
  filter(str_detect(area, "27.1|27.2.b.2|27.2.a.2|27.2.a_NK|27.2.b.NK|27.2_NK") # Limit to areas of interest
         & area != "27.14.a") %>% 
  left_join(guild) %>%                                                 # Attach guilds
  filter(str_detect(Guild, "Demersal") &
           between(year, 2000, 2017)) %>% 
  group_by(year) %>% 
  summarise(tonnes_new_names = sum(tonnage)) %>% 
  left_join(ICES_old)

write.csv(ICES_new, "./ICES check.csv")

