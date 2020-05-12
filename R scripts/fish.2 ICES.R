
#### Set up ####

rm(list=ls())                                                               # Wipe the brain
setwd("~/Data/Fish Landings ICES-FAO")                                      # Set working directory
library(dplyr)                                                              # Enter the tidyverse
library(tidyr)
library(ggplot2)
library(egg)
library(rgdal)                                                              # Handle GIS data imports
library(ggfortify)                                                          # Coerce shapefiles to dataframes
library(maps)                                                               # GIS in R
library(polyggon)                                                           # Use for polygon plotting with holes 
library(raster)                                                             # Map plotting
library(gganimate)                                                          # Animate ggplots
library(gifski)                                                             # Animation renderer
library(transformr)
library(rgeos)

ICES <-read.csv("ICES_1903-2017.csv", header=T) %>%                                  # Import combined ICES landings  
        separate(area, c("area_only","sub_area", "division", "sub_division"), 
        sep = "([\\.\\_])", remove = FALSE) %>%                                      # Split code into spatial hierarchy
         unite(sub_area, c(area_only, sub_area), sep = ".", remove= FALSE) %>%       # Create a sub-area resolution column
          unite(division, c(sub_area, division), sep = ".", remove= FALSE) %>%       # Division column
           unite(sub_division, c(division, sub_division), sep = ".", remove= FALSE)  # Sub-division
 
ICES$sub_division <- gsub(".NA", "", ICES$sub_division)                              # Remove .NAs from area strings
ICES$division <- gsub(".NA", "", ICES$division)                                      # Don't put all columns in a vector or lengths mismatch
ICES$sub_area <- gsub(".NA", "", ICES$sub_area)

ICES <- mutate(ICES, sub_area = as.factor(sub_area), division = as.factor(division), # convert to factors
        sub_division = as.factor(sub_division)) %>%
         mutate(guild = paste(Category, Subcategory, sep = "\n"))                     # add in guild column
# Find missing guild classifications
# missing <- filter(ICES, is.na(Category) | is.na(Subcategory))
# unique(missing$acronym)


#### Set spatial environment ####

FAO <- readOGR(dsn="FAO_map", layer = "FAO_AREAS")                                        # Import FAO shapefile
# Don't foritfy directly or you'll never work out which IDs are which FAO areas

world <- fortify(map("world", fill = TRUE, col = "grey"))                                 # Import map of landmasses

Arctic_FIDs <- filter(FAO@data, F_CODE %in% c("27.1.a", "27.1.b", "27.2.a.1", "27.2.a.2", # Find polygons
                   "27.2.b.1", "27.2.b.2", "27.14.a")) %>% dplyr::select(FID)             # of interest

FAO1 <- FAO[FAO@data$FID == 267,]                                                         # Extract polygon
FAO2 <- FAO[FAO@data$FID == 268,]                                                         # Complex architecture
FAO3 <- FAO[FAO@data$FID == 275,]                                                         # of the FAO shapefile
FAO4 <- FAO[FAO@data$FID == 351,]                                                         # seems to inihibit %in% 
FAO5 <- FAO[FAO@data$FID == 352,]                                                         # operator
FAO6 <- FAO[FAO@data$FID == 353,]
FAO7 <- FAO[FAO@data$FID == 358,]

# ## Combine polygons to sub-area resolution
# Sub_area_27.2 <- gUnion(FAO4, FAO5) %>%                                                   # Dissolve divisions
#                   gUnion(FAO6) %>%                                                        # Joining is binary
#                    gUnion(FAO7)
# Sub_area_27.1 <- gUnion(FAO1, FAO3)
# 
# FAO_arctic <- bind(FAO2, Sub_area_27.2, Sub_area_27.1) %>%                                # Recombine for ease
#                fortify() %>%                                                              # Coerce to dataframe
#                 mutate(id = as.factor(id))
# 
# levels(FAO_arctic$id) <- list ("27.14" = "1", "27.1" = "3", "27.2" = "2")                 # Rename polygons
# 
# ## Combine polygons to division resolution
# Division_27.2.b <- gUnion(FAO5, FAO6)                                                     # Dissolve the sub-divisions
# Division_27.2.a <- gUnion(FAO4, FAO7)
# 
# FAO_arctic <- bind(FAO1, FAO2, FAO3, Division_27.2.a, Division_27.2.b) %>%                # Recombine for ease
#   fortify() %>%                                                                           # Coerce to dataframe
#   mutate(id = as.factor(id))
# 
# levels(FAO_arctic$id) <- list ("27.1.a" = "1","27.14.a" = "2","27.1.b" = "3",             # Rename polygons
#                                "27.2.a" = "4", "27.2.b" = "5")
# 
# ## Max resolution, sub-division (will drop data labelled x_NK)
# FAO_arctic <- bind(FAO1, FAO2, FAO3, FAO4, FAO5, FAO6, FAO7) %>%                          # Recombine for ease
#                fortify() %>%                                                              # Coerce to dataframe
#                 mutate(id = as.factor(id))
# 
# levels(FAO_arctic$id) <- list ("27.1.a" = "1", "27.14.a" = "2", "27.1.b" = "3",           # Rename polygons
#                                "27.2.a.1" = "4", "27.2.b.1" = "5", "27.2.b.2" = "6", 
#                                "27.2.a.2" = "7")

## Combine polygons to custom resolution
Division_27.2.b <- gUnion(FAO5, FAO6)                                                     # Dissolve the sub-divisions
Division_27.2.a <- gUnion(FAO4, FAO7)
Sub_area_27.1 <- gUnion(FAO1, FAO3)

FAO_arctic <- bind(Sub_area_27.1, FAO2, Division_27.2.a, Division_27.2.b) %>%             # Recombine for ease
  fortify() %>%                                                                           # Coerce to dataframe
   mutate(id = as.factor(id))

levels(FAO_arctic$id) <- list ("27.1" = "1","27.14.a" = "2",                              # Rename polygons
                               "27.2.a" = "3", "27.2.b" = "4")

## Plot map
group.colours <- c("27.1" = "Tan2", "27.2.a" = "cyan4", "unassigned" = 'Red', "27.14.a" = "springgreen3", "27.2.b" = "turquoise", "total" = 'Black')

Map_interest <- ggplot(FAO_arctic, aes(long, lat, group = group, fill = id)) +            # Plot areas
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black") +
  geom_holygon(colour = "white") +                                                        # Plots polygons with holes
  theme_minimal() +
  coord_fixed(1.3, xlim = c(min(-50), max(75)),
              ylim = c(min(50), max(90))) +
  labs(x = 'Longitude (W)', y = 'Latitude (N)', title = "Fishing regions of interest") +
  scale_fill_manual(guide= guide_legend(title = 'Regions'), values = c(group.colours)) +
  #annotate("text", label = "92.9% of reports", x = -48, y = 52.5, hjust = 0) +            # Sub-area data present
  #annotate("text", label = "63.5% of reports", x = -48, y = 52.5, hjust = 0) +            # Dvision data present
  annotate("text", label = "97.79% of landings", x = -48, y = 52.5, hjust = 0) +           # sub-division data present
  NULL
Map_interest                                                                              # Polygons appear correctly labelled

#ggsave('FIG_FAO custom of interest.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")

rm(FAO1, FAO2, FAO3, FAO4, FAO5, FAO6, FAO7)                                              # Clean environment

## Make the custom resolution column (Division but dissolve the Barents sea)
ICES_orig <- mutate(ICES, custom = division) %>%                                          # Make custom column
              filter(!custom %in% c("27.1.a", "27.1.b", "27.1.NK"))                       # Remove codes to change

ICES <- mutate(ICES, custom = division) %>%
         filter(custom %in% c("27.1.a", "27.1.b", "27.1.NK")) %>%                         # Select codes to change
          mutate(custom = "27.1") %>%                                                     # Overwrite with new value
           bind_rows(ICES_orig)                                                           # Add in other codes
            ICES[grep("NK", ICES$custom), "custom"] = "unassigned"                                    # Collapse NK codes
#### Create summaries ####
          
Landings <- function(...) {
 summary_guilds <- group_by(ICES, ...) %>%                                 # Specify how to sum landings, ... should be replaced by bare column names
                    summarise(Landings = sum(tonnage))                     # Sum the landings within groups
return(summary_guilds) }                                                   # Tidy summary function
            
summary_guilds <- Landings(year, guild)                                     # Landings by functional guilds over time
summary_bloc <- Landings(year, N.Blocs)                                     # Landings by bloc of nations over time
summary_bloc_guilds <- Landings(year, N.Blocs, guild)                       # Landings by functional guilds and bloc of nations over time
            
#### Initial plots ####
          
# Time series of landings in the arctic by functional guild
Time_Guilds <- ggplot(summary_guilds, aes(x=year, y = Landings, group = guild, colour = guild)) +
 geom_line() +
 theme_minimal() +
 NULL
 Time_Guilds
#ggsave('FIG_Time_Guild.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")
            
# Time series of landings in the arctic by blocks of nations
Time_Bloc <- ggplot(summary_bloc, aes(x=year, y = Landings, group = N.Blocs, colour = N.Blocs)) +
 geom_line() +
 theme_minimal() +
 NULL
 Time_Bloc
#ggsave('FIG_Time_Nations.png', plot = last_plot(), scale = 1, width = 16, height = 10, units ="cm")
            
# Time series of landings in the Arcitc by both block of nations and functional guild
Time_Bloc_Guilds <- ggplot(summary_bloc_guilds, aes(x=year, y = Landings, group = guild, colour = guild)) +
 geom_line() +
 facet_wrap(~N.Blocs, nrow = 1) +
 theme_minimal() +
 labs(x = "Year", y = "Landings (Tonnes)", 
 title = "Times series of Arctic landings by guild and bloc of nations") +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 NULL
 Time_Bloc_Guilds
#ggsave('FIG_Time_Nations_Guild.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")
            
#### Create spatial summary ####

## All landings by FAO custom 
total <- Landings(year) %>%
          mutate(custom = "total")
           
summary_space_all_custom <- Landings(year, custom) %>%                
                              bind_rows(total) 

100 -(sum(filter(summary_space_all_custom, custom == "unassigned")$Landings)/             # What proportion of landings
  sum(filter(summary_space_all_custom, custom == "total")$Landings))*100                  # are assigned?

## Time series of landings in the arctic by custom 

Time_custom <- ggplot(summary_space_all_custom, aes(x=year, y = Landings, group = custom, colour = custom)) +
  geom_line() +
  geom_point(data = filter(summary_space_all_custom, custom == "unassigned")) +
  theme_minimal() +
  scale_colour_manual(guide= guide_legend(title = 'Regions'), values = c(group.colours)) +
  labs(x = "Year", y = "Landings (Tonnes)", title = "Time series of fisheries landings in the Arctic (ICES)") +
  NULL
Time_custom

#ggsave('FIG_Landings custom of interest.png', plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm")

Landings_panels <- ggarrange(Time_custom, Map_interest, ncol = 1) 
Landings_panels

#ggsave('FIG_REPORT 1 TS_MAP.png', plot = last_plot(), scale = 1, width = 8, height = 10, units = "cm")

#### Species composition within guilds ####

# ## Top 10 species by landings in each guild
# summary_species <- Landings(guild, common) %>%                                         # Group by species keeping guild                       
#                     top_n(10, Landings)                                                # Grab top 10

## Selecting instead by species which represent top 75% of biomass
species_total <- group_by(ICES, guild) %>%                                               # Group by guild
                  summarise(Total = sum(tonnage))                                        # Total landings per guild
 
species_max <- Landings(guild, common) %>%                                               # Landings by species keeping guild
                group_by(guild) %>%                                                      # Group by guild
                 top_n(1, Landings)                                                      # Select largest (if a single species is more than 75% of biomass none will be retained)

summary_species <- Landings(guild, common) %>%                                           # Landings by species keeping guild  group_by(guild) %>%                                                 # Group by guild
                      arrange(-Landings) %>%                                             # Order species in descending landings in groups     
                       mutate(Biomass = cumsum(Landings)) %>%                            # Create cumulative landings column
                        left_join(species_total) %>%                                     # add in total column
                         filter(Biomass/Total < 0.75) %>%                                # Filter out species which tip cumulative sum over 75% of biomass
                          bind_rows(species_max) %>%                                     # Add in single species with more than 75% of landings   
                           dplyr::select(-c(Biomass, Total)) %>%                         # Drop columns missing from species_max
                            distinct()                                                   # Drop duplicated entries (species_max smaller than 75% of landings)

 species_plot <- ggplot(summary_species, aes(x =reorder(common, -Landings), y = Landings, fill = guild)) +
                 geom_col() +
                 theme_minimal() +
                 facet_grid(rows = vars(guild), scales = "free_y") +
                 labs(x = "Species", y = "Landings (tonnes)", title = "Top species comprising < 75% of total landed biomass within guilds") +
                 coord_flip() +
                 theme(legend.position = "None", strip.text.y = element_text(angle = 0)) +
                 NULL

 TS_top_species <- Landings(year, common) %>%                                           # Landings by species
                    filter(common %in% unique(summary_species$common))                  # Limit to largest contributors to each guild

 TS_top_plot <- ggplot(TS_top_species, aes(x=year, y = Landings, group = common, colour = common)) +
                geom_line() +
                theme_minimal() +
                labs(x = "Year", y = "Landings (tonnes)", title = "Time series of the top contributing species to landings in guilds (see above)") +
                NULL

species_panels <- ggarrange(species_plot, TS_top_plot, nrow = 2, heights = c(1,0.66)) 
species_panels
 
#### Species composition within guilds (spatially explicit) ####

species_total <- group_by(ICES, guild, custom) %>%                          # Group by guild
                  summarise(Total = sum(tonnage))                           # Total landings per guild

species_max <- Landings(guild, common, custom) %>%                          # Landings by species keeping guild and location
                group_by(guild, custom) %>%                                 # Group by guild and location
                 top_n(1, Landings)                                         # Select largest (if a single species is more than 75% of biomass none will be retained)

summary_reg_species <- Landings(guild, common, custom) %>%                  # Landings by species keeping guild and location
                         group_by(guild, custom) %>%                        # Group by guild
                          arrange(-Landings) %>%                            # Order species in descending landings in groups     
                           mutate(Biomass = cumsum(Landings)) %>%           # Create cumulative landings column
                            left_join(species_total) %>%                    # add in total column
                             filter(Biomass/Total < 0.75) %>%               # Filter out species which tip cumulative sum over 75% of biomass
                              bind_rows(species_max) %>%                    # Add in single species with more than 75% of landings   
                               dplyr::select(-c(Biomass, Total)) %>%        # Drop columns missing from species_max
                                distinct()                                  # Drop duplicated entries (species_max smaller than 75% of landings)

species_reg_plot <- ggplot(summary_reg_species, aes(x =reorder(common, -Landings), y = log(Landings), fill = guild)) +
  geom_col() +
  theme_minimal() +
  facet_grid(rows = vars(guild), cols = vars(custom), scales = "free") +
  labs(x = "Species", y = "Landings (log(tonnes))", title = "Top species comprising < 75% of total landed biomass within guilds by region") +
  coord_flip() +
  theme(legend.position = "None", strip.text.y = element_text(angle = 0)) +
  NULL

TS_top_reg_species <- Landings(year, common, custom) %>%                  # Sum landings
                       filter(common %in% unique(summary_species$common)) # Limit to largest contributors to each guild

TS_top_reg_plot <- ggplot(TS_top_reg_species, aes(x=year, y = Landings, group = common, colour = common)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~custom) +
  labs(x = "Year", y = "Landings (tonnes)", title = "Time series of the top contributing species to landings in guilds regionally") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL

#### Look into the sub-divisions species composition after deciding to look in a meeting ####

species_total <- group_by(ICES, guild, area) %>%            # Group by guild
  summarise(Total = sum(tonnage))                           # Total landings per guild

species_max <- Landings(guild, common, area) %>%            # Landings by species keeping guild and location
  group_by(guild, area) %>%                                 # Group by guild and location
  top_n(1, Landings)                                        # Select largest (if a single species is more than 75% of biomass none will be retained)

summary_reg_species <- Landings(guild, common, area) %>%    # Landings by species keeping guild and location
  group_by(guild, area) %>%                          # Group by guild
  arrange(-Landings) %>%                            # Order species in descending landings in groups     
  mutate(Biomass = cumsum(Landings)) %>%           # Create cumulative landings column
  left_join(species_total) %>%                    # add in total column
  filter(Biomass/Total < 0.75) %>%               # Filter out species which tip cumulative sum over 75% of biomass
  bind_rows(species_max) %>%                    # Add in single species with more than 75% of landings   
  dplyr::select(-c(Biomass, Total)) %>%        # Drop columns missing from species_max
  distinct()  %>%                             # Drop duplicated entries (species_max smaller than 75% of landings)
  filter(area %in% c("27.1.a", "27.2.a.1"))  # Limit to areas which looked interesting after GFW VMS data

species_reg_plot <- ggplot(summary_reg_species, aes(x =reorder(common, -Landings), y = log(Landings), fill = guild)) +
  geom_col() +
  theme_minimal() +
  facet_grid(rows = vars(guild), cols = vars(area), scales = "free") +
  labs(x = "Species", y = "Landings (log(tonnes))", title = "Top species comprising < 75% of total landed biomass within guilds by region") +
  coord_flip() +
  theme(legend.position = "None", strip.text.y = element_text(angle = 0)) +
  NULL

TS_top_reg_species <- Landings(year, common, area) %>%        # Sum landings
  filter(common %in% unique(summary_reg_species$common)) %>%  # Limit to largest contributors to each guild
   filter(area %in% c("27.1.a", "27.2.a.1"))

TS_top_reg_plot <- ggplot(TS_top_reg_species, aes(x=year, y = Landings, group = common, colour = common)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~area) +
  labs(x = "Year", y = "Landings (tonnes)", title = "Time series of the top contributing species to landings in guilds regionally") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL

Sub_division_panels <- ggarrange(species_reg_plot, TS_top_reg_plot, nrow = 2, heights = c(1,0.66)) 
Sub_division_panels



#### How are unassigneds distributed? ####

total <- group_by(ICES, year, guild) %>%                                            # Group and summarise by guild                    
          summarise(assigned = sum(tonnage)) %>%

total_unassigned <- filter(ICES, custom == "unassigned") %>%                        # Group and summarise unassigneds by guild
                     group_by(year, guild) %>%                         
                      summarise(unassigned = sum(tonnage)) %>%
                       left_join(total) %>% 
                        mutate(percent = unassigned/assigned*100)                   # Calculate % unassigned per guild

unassigned_plot <- ggplot(total_unassigned, aes(x=year, y = percent, group = guild, colour = guild)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Unassigned landings (%)") +
  theme(legend.position = 'None') +
  NULL

unassigned_bar <- group_by(total_unassigned, guild) %>%                             # Collapse the average across years            
                   summarise(unassigned = sum(unassigned),
                             assigned = sum(assigned)) %>%
                    mutate(percent = unassigned/assigned*100)

unassigned_all <- ggplot(unassigned_bar, aes(x=guild, y = percent, colour = guild, fill = guild)) +
  geom_col() +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL

unassigned_panels <- ggarrange(unassigned_plot, unassigned_all, nrow = 1, widths = c(1, 0.5)) 
unassigned_panels

#### Animate map ####

# Change to match spatial resolution column used in
colnames(FAO_arctic)[6] <- "sub_area"                                                     # Rename column for matching

Fish_map_data <- left_join(FAO_arctic, summary_space_all_sub_area_bloc)                   # Specify the summarised data to use
#                  filter(guild == 'Pelagic.Resident')                                    # If you want to filter data do it here

Fish_map <- ggplot(Fish_map_data, aes(long, lat, group = group, fill = Landings)) +       # Plot areas
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "black") +       # Plot landmasses
  geom_holygon(colour = "white") +                                                        # Plots polygons with holes
  theme_minimal() +
  coord_fixed(1.3, xlim = c(min(-50), max(75)),
   ylim = c(min(50), max(90))) +
  labs(x = 'Longitude (W)', y = 'Latitude (N)') +
  scale_fill_gradient(name = 'Landings', low = 'Blue', high = 'Red') +
  facet_wrap(~N.Blocs) +                                                                  # specify a facetting variable
  transition_time(year) +                                                                 # Change frames through years
  labs(title = "Year: {frame_time}") +                                                    # Label for animation frames
  NULL
# Fish_map                                                                                # Run animation

gganimate::animate(Fish_map, width = 16, height = 10,                                     # Run animation - user specified
                   res = 300, units = "cm", fps = 10, nframes = 200)

# anim_save("GIF_All_sub_area_nations", animation = last_animation())                                  # Save animation
