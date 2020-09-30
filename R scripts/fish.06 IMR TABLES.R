
#**# Produce ranked tables of Landings by gear and species in IMR fishing logbook data

#### Set up ####

rm(list=ls())                                                                               # Wipe the brain
packages <- c("tidyverse", "formattable", "knitr", "kableExtra")                            # List packages
lapply(packages, library, character.only = TRUE)                                            # Load packages

species <- data.table::fread("./Data/IMR/IMR species list.csv", sep = ',') %>%              # Get species look up table
  rename(Species = Tall)

gear <- data.table::fread("./Data/IMR/codelist_gear.csv", sep = ';') %>%                    # Get gear look up table
  rename(Gear = CODE)

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';') %>%               # Import IMR fishing data
  `colnames<-`(c("Year", "Month", "Day", "Gear", "Fishing_time", "Area_code", "Economic_zone", 
                 "Area_Norway", "Location_Norway", "Vessel_length", "Species", "Weight")) %>% # Set column names
  filter(Area_Norway %in% c(1:4, 10:18, 20:25))                                             # Limit to areas of interest

IMR <- left_join(IMR, gear) %>%                                                             # Attach labels
  left_join(species) %>%                                                                    # Attach labels
  mutate(Weight = round(Weight/1000))                                                       # Convert Kg to tonnes

#### Summarise and rank landings by gear ####

ranked <- group_by(IMR, Species, `Engelsk navn`, ENGLISH) %>%                               # Summarise landings over all time
  summarise(Landings_tonnes = sum(as.numeric(Weight))) %>% 
  ungroup %>% 
  group_by(ENGLISH) %>%                                                                     # By gear
  mutate(Rank = order(order(Landings_tonnes, decreasing=TRUE))) %>%                         # Order by landings and assign ranks
  .[order(-.$Landings_tonnes),] %>% 
  ungroup()

#### Gear table ####  

gears_table <- ranked %>% 
  rename(Gear = ENGLISH) %>%                                                                # Rename columns
  group_by(Gear) %>% 
  summarise(Landings_tonnes = sum(Landings_tonnes)) %>%                                     # Total all landings within a gear type
  ungroup() %>% 
  .[order(-.$Landings_tonnes),] %>%                                                         # Order by landings
  mutate(Landings_tonnes = color_bar("lightblue")(Landings_tonnes)) %>%                     # Create cool colourbar column
  kable(escape = F,                                                                         # Pass to html table format
    col.names = c("Gear Type", "Landings\n(tonnes)")) %>%                                   # Set column headings
  kable_styling("hover", full_width = F) %>%                                                # Let the mouse highlight rows
  column_spec(2, width = "10cm") %>%                                                        # Specify column width
  save_kable("./Figures/IMR_gear_list.html")                                                # Save

#### Species table ####  

species_table <- select(ranked, -Species) %>% 
  rename(Species = `Engelsk navn`) %>%                                                                # Rename columns
  group_by(Species) %>% 
  summarise(Landings_tonnes = sum(Landings_tonnes)) %>%                                     # Total all landings within a gear type
  ungroup() %>% 
  .[order(-.$Landings_tonnes),] %>%                                                         # Order by landings
  mutate(Rank = order(order(Landings_tonnes, decreasing=TRUE))) %>%                         # Order by landings and assign ranks
  mutate(Landings_tonnes = color_bar("lightblue")(Landings_tonnes)) %>%                     # Create cool colourbar column
  kable(escape = F,                                                                         # Pass to html table format
        col.names = c("Species", "Landings\n(tonnes)", "Rank")) %>%                                   # Set column headings
  kable_styling("hover", full_width = F) %>%                                                # Let the mouse highlight rows
  column_spec(3, width = "10cm") %>%                                                        # Specify column width
  save_kable("./Figures/IMR_species_list.html")                                             # Save

#### Wide table landings by gear ####

wide_table <- select(ranked, -Species) %>% 
  .[order(.$ENGLISH),] %>%                                                                  # Order by gear type 
  pivot_wider(names_from = ENGLISH, values_from = c(`Engelsk navn`, Landings_tonnes)) %>%   # Spread landings and species names by gear
  select(Rank, ends_with(unique(ranked$ENGLISH))) %>%                                       # Reorder columns
  mutate_at(.vars = vars(starts_with("Landings")),                                          # Convert all landings columns
            .funs = list(~ color_bar(if_else(is.na(.), "white", "lightblue"))(.))) %>%      # To cool colour bars
  kable(escape = F,
        col.names = c("Rank", rep(c("Species", "Landings\n(tonnes)"), times = 20))) %>%     # Specify column names in HTML table
  kable_styling("hover", full_width = F) %>%                                                # Allow row highlighting
  column_spec(41, width = "10cm") %>%                                                       # Specify column width
  add_header_above(c(" ", setNames(rep(2, length(unique(ranked$ENGLISH))-1), unique(ranked$ENGLISH)[!is.na(unique(ranked$ENGLISH))]))) %>% # Set group headings
  save_kable("./Figures/IMR_wide_table.html")                                               # Save

#### Long table ####

gear_list <- str_sort(unique(IMR$ENGLISH))                                                  # Alphabetised gear list

long_table <- select(ranked, -Species) %>% 
  rename(Species = "Engelsk navn", Gear = ENGLISH) %>%                                      # Rename columns
  mutate(Landings_tonnes =  color_bar(case_when(                                            # Set landings as cool colour bar
    Gear == gear_list[1] ~ rainbow(28)[1],                                                  # Specifying unique colour for
    Gear == gear_list[2] ~ rainbow(28)[2],                                                  # each gear type
    Gear == gear_list[3] ~ rainbow(28)[3],
    Gear == gear_list[4] ~ rainbow(28)[4],
    Gear == gear_list[5] ~ rainbow(28)[5],
    Gear == gear_list[6] ~ rainbow(28)[6],
    Gear == gear_list[7] ~ rainbow(28)[7],
    Gear == gear_list[8] ~ rainbow(28)[8],
    Gear == gear_list[9] ~ rainbow(28)[9],
    Gear == gear_list[10] ~ rainbow(28)[10],
    Gear == gear_list[11] ~ rainbow(28)[11],
    Gear == gear_list[12] ~ rainbow(28)[12],
    Gear == gear_list[13] ~ rainbow(28)[13],
    Gear == gear_list[14] ~ rainbow(28)[14],
    Gear == gear_list[15] ~ rainbow(28)[15],
    Gear == gear_list[16] ~ rainbow(28)[16],
    Gear == gear_list[17] ~ rainbow(28)[17],
    Gear == gear_list[18] ~ rainbow(28)[18],
    Gear == gear_list[19] ~ rainbow(28)[19],
    Gear == gear_list[20] ~ rainbow(28)[20],
    Gear == gear_list[21] ~ rainbow(28)[21],
    Gear == gear_list[22] ~ rainbow(28)[22],
    Gear == gear_list[23] ~ rainbow(28)[23],
    Gear == gear_list[24] ~ rainbow(28)[24],
    Gear == gear_list[25] ~ rainbow(28)[25],
    Gear == gear_list[26] ~ rainbow(28)[26],
    Gear == gear_list[27] ~ rainbow(28)[27],
    Gear == gear_list[28] ~ rainbow(28)[28]
  ))(Landings_tonnes)) 

## Landings ordered 

long_table %>%  
  kable(escape = F,
        col.names = c("Species", "Gear Type", "Landings\n(tonnes)", "Rank in\ngear")) %>%   # Set column names
  kable_styling("hover", full_width = F) %>%                                                # Allow row highlighting
  column_spec(4, width = "10cm") %>%                                                        # Set column width
  save_kable("./Figures/IMR_landings_table.html")                                           # Save

## Species ordered 

long_table %>% 
  .[order(.$Species),] %>%                                                                  # Order by species names
  kable(escape = F,
      col.names = c("Species", "Gear Type", "Landings\n(tonnes)", "Rank in\ngear")) %>%     # Set column headings
  kable_styling("hover", full_width = F) %>%                                                # Allow row highlighting
  column_spec(4, width = "10cm") %>%                                                        # Set column width
  save_kable("./Figures/IMR_alphabetical_table.html")                                       # Save 

