
## Ratio of Russian landings to non-by guild

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse")                                                  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>% 
  dplyr::select(Guild, Category, Subcategory) %>% 
  distinct

#### Get an inflation factor for each guild to get from Non-Russian to International landings ####

##**## Scale the proportiong of landings from each ICES region contributing to the model domain. Could do by area
##**## Could also use GFW

ICES <-read.csv("./Data/ICES fish/ICES_1903-2017.csv", header=T) %>%        # Import combined ICES landings  
       filter(str_detect(area, "27.1|27.2.b.2|27.2.a.2|27.2.a_NK|27.2.b.NK|27.2_NK") # Limit to areas of interest
              & area != "27.14.a") %>% 
       mutate(Russia = str_detect(cname, "Russia")) %>%                     # Identify Russian activity
       left_join(guild) %>%                                                 # Attach guilds
      ## Add weighting here then keep weights column 
      dplyr::select(Russia, Guild, tonnage) %>%                            # Limit to columns of interest
       group_by(Guild, Russia) %>%                                          # Per guild and group of flags 
       summarise(tonnage = sum(tonnage, na.rm = T)) %>%                     # Total the landings
       ungroup() %>%                            
       group_by(Guild) %>%                                                  # Now for each guild
       mutate(total_guild_tonnage = sum(tonnage)) %>%                       # Total landings
       ungroup() %>% 
       filter(Russia == FALSE) %>%                                          # Don't need Russian data anymore
       mutate(Inflation = total_guild_tonnage/tonnage) %>%                  # How do we get from non-Russian landings per guild to our known total?
       dplyr::select(Guild, Inflation)

saveRDS(ICES, "./Objects/ICES landings inflation.rds")

# ICES2 <- filter(ICES, cname == "Norway") %>% 
#   group_by(year) %>% 
#   summarise(landings = sum(tonnage, na.rm = T)) %>% 
#   filter(year > 1999)
# 
# ggplot(ICES2) +
#   geom_line(aes(x = year, y = landings)) +
#   ylim(0, max(ICES2$landings))

