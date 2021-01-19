
# Build MiMeMo fish guilds file.

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "furrr")                                     # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multiprocess)                                                            # Set up parallel processing

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

existing_guilds <- read.csv("./Old data/Fish Landings ICES-FAO/FAO_species_reference_list_v9.csv", header = TRUE)

IMR_species <- data.table::fread("./Data/IMR/IMR species list_2.csv", sep = ',', # Get species look up table
                                 colClasses = c(Tall = "character")) %>%      # Keep leading 0s for species codes
  rename(`IMR code` = Tall) %>% 
  select(-c(V6, V7, V8))

#### Get IMR landings of interest ####

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(FISK = "character")) %>%              # Keep the leading 0s
  `colnames<-`(c("Year", "Month", "Day", "Gear", "Fishing_time", "Area_code", "Economic_zone", 
                 "Area_Norway", "Location_Norway", "Vessel_length", "IMR code", "Weight")) %>% # Set column names
  filter(Area_Norway %in% c(1:4, 10:18, 20:25)) %>%                           # Limit to areas of interest
  left_join(IMR_species) %>%                                                  # Attach labels
  select(`Norsk navn`, `Engelsk navn`, `FAO`, `Latinsk navn`, `IMR code`) %>% # Limit to species info
  distinct %>%                                                                # Drop duplicates
  drop_na()                                                                   # Drop incomplete entries, there's one with no info which soaks up NAs later. 
                                                                              # There's also 0199 which isn't in the species list
#### Get EU landings of interest ####

EU_landings <- str_glue("./Data/EU fish/spatial_landings_{2015:2018}/") %>%   # Programmatically build folder names
  future_map(~{ rgdal::readOGR(.x) %>%                                        # Import each EU effort shapefile
      st_as_sf() %>%                                                          # Convert to SF
      select(year, species)},                                                 # Drop some columns,
      .progress = T) %>%                                    
  bind_rows() 

EU_Arctic <- st_contains(Region_mask, EU_landings, sparse = F) %>%            # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_landings[.,"species"] %>%                                                # Get the species caught
  st_drop_geometry() %>%                                                      # Drop sf formatting
  unique() %>%                                                                # Drop duplicates
  rename(FAO = "species")

#### Getting unclassified EU species ####

EU_not_in_IMR <- left_join(EU_Arctic, IMR) %>%                                # Match FAO codes represented by IMR and EU
  filter(is.na(`IMR code`)) %>%                                               # If they're represented in the IMR code they will be covered by the next section
  mutate(Common.name = case_when(FAO == "PCR" ~ "Tanner crab nei",            # Label undefined FAO codes
                                 FAO == "LCT" ~ "Arctic eelpout",
                                 FAO == "JPS" ~ "Metallic codling",
                                 FAO == "BRF" ~ "Blackbelly rosefish",
                                 FAO == "ARY" ~ "Argentine",
                                 FAO == "HYD" ~ "Ratfishes nei",
                                 FAO == "SRA" ~ "Atlantic searobins"),
         Scientific.name = case_when(FAO == "PCR" ~ "Chionoecetes spp",
                                     FAO == "LCT" ~ "Lycodes reticulatus",
                                     FAO == "JPS" ~ "Physiculus fulvus",
                                     FAO == "BRF" ~ "Helicolenus dactylopterus",
                                     FAO == "ARY" ~ "Argentina sphyraena",
                                     FAO == "HYD" ~ "Hydrolagus spp",
                                     FAO == "SRA" ~ "Prionotus spp")) %>% 
  select(Common.name, Scientific.name, FAO) 

#### Binding FAO and IMR species codes to categories and subcategories ####

# You have to join by common name and latin name in two separate steps as the combinations
# don't always match the combinations in the original guilds data file. Joining by each column independently 
# maximises the number of matches.

Classified_by_common <- IMR %>%                                               # Get info on species ID
  rename(Common.name = `Engelsk navn`) %>%                                    # Match column name of guild file
  select(-c("Norsk navn", "Latinsk navn")) %>%                                # Limit to new codes and common names 
  right_join(existing_guilds, by = "Common.name")                             # Join to old classifications

Classified_by_latin_or_common <- IMR %>%                                      # Get info on species ID
  rename(Scientific.name = `Latinsk navn`) %>%                                # Match column names to old file
  select(-c("Norsk navn", "Engelsk navn")) %>%                                # Limit to new codes and latin names
  right_join(Classified_by_common, by = "Scientific.name") %>%                # Join to old classifications
  mutate(`IMR code` = ifelse(is.na(`IMR code.x`), `IMR code.y`, `IMR code.x`),# Forcing a join by one column introduces duplicated FAO and IMR columns
         FAO = ifelse(is.na(FAO.x), FAO.y, FAO.x)) %>%                        # The NAs are introduced by coercion, we really want to splice the sets of complete values
  select(-c(FAO.y, FAO.x, `IMR code.y`, `IMR code.x`))                        # Drop the duplicated columns
  
Need_to_classify <- IMR %>% 
  rename(Common.name = `Engelsk navn`) %>%                                    
  left_join(existing_guilds) %>%                                              # Join to categories by common name
  filter(is.na(Category)) %>%                                                 # Which are missing a category?             
  select(`Norsk navn`, Common.name, `FAO`, `Latinsk navn`, `IMR code`) %>%    # Drop the attached columns so we can join again trying latin name 
  rename(Scientific.name = `Latinsk navn`,                                    # Rename latin name for join, undo naming of common name to prevent join
         `Engelsk navn` = Common.name) %>% 
  left_join(existing_guilds) %>%                                              # Try classifying again
  filter(is.na(Category)) #%>%                                                # Which are STILL missing a guild?
#  write.csv(file = "./Objects/IMR_new_guilds.csv")                           # We need to do those manually

New_assignments <- read.csv("./IMR_new_guilds.csv") %>%                       # Had a meeting to reassign names
  filter(!X.1 %in% c("check", "drop")) %>%                                    # Drop centrolophidae as not found in the northern hemisphere
  select(-X.1)                                                                # Lose column for comments 

names(New_assignments) <- str_replace(names(New_assignments), "[.]", " ")     # Update the column names to match IMR species list

# Need to put the IMR codes back because I cleaned this up after the meeting
# Also first time round trailing zeros were missed from the species codes, 
# the new "need_t0_classify" is shorter

IMR_extras <- select(Need_to_classify, `Norsk navn`:`IMR code`) %>%           # Drop duplicated columns to allow a join
           filter(Scientific.name != "Calanus finmarchicus") %>%              # Drop calanus again
           rename(`Scientific name` = "Scientific.name") %>%                  # Rename column for match
           left_join(New_assignments) %>%                                     # Join new guild classifications
  select(-c(X, `Norsk navn`)) %>%                                             # Drop excel row column
  rename(Common.name = `Engelsk navn`,
         `Scientific.name` = "Scientific name")

#### Translate categories into StrathE2E guilds ####

New_guilds <- Classified_by_latin_or_common %>% 
  full_join(EU_not_in_IMR) %>%                                                # Join to old file, introducing NAs when unclassified
  full_join(IMR_extras)                                                       # Add New classifications for IMR data

#Translate_categories <- select(New_guilds, Category, Subcategory) %>%        # Save out combinations of categories to label with guilds
#  distinct() %>% 
#  write.csv("./Data/Fish categories to guilds.csv")

MiMeMo_guilds <- left_join(New_guilds, read.csv("./Data/Fish categories to guilds.csv")) %>% # Translate categories into guilds
  mutate(FAO = case_when(Common.name == "Blackbelly rosefish" ~ "BRF",        # Because I didn't join EU to FAO some species 
                         Common.name == "Argentine" ~ "ARY",                  # Get duplicated because they lack an FAO code in
                         Common.name == "Ratfishes nei" ~ "HYD",              # FAO file but not in the EU file.
                         Common.name == "Atlantic searobins" ~ "SRA",         # This and the filter line stops double joins later
                         T ~ FAO)) %>% 
  mutate(Guild = case_when(FAO == "PCR" ~ "Benthos carnivore/scavenge feeder",# Define guilds for undefined FAO codes
                           FAO == "LCT" ~ "Demersal (non quota)",
                           FAO == "JPS" ~ "Demersal (non quota)",
                           FAO == "ARY" & is.na(Guild) ~ "Duplicate",         # Mark the ones which I didn't catch first time rounf
                           FAO == "BRF" & is.na(Guild) ~ "Duplicate",         # For removal
                           FAO == "HYD" & is.na(Guild) ~ "Duplicate",
                           FAO == "SRA" & is.na(Guild) ~ "Duplicate",
                           T ~ Guild)) %>% 
  filter(Guild != "Duplicate" ) %>% 
  write.csv("./Data/MiMeMo fish guilds.csv")
