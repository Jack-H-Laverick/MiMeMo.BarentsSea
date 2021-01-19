
#**# Produce ranked tables of Landings by gear and species in IMR fishing logbook data

#### Set up ####

rm(list=ls())                                                                               # Wipe the brain
packages <- c("tidyverse", "formattable", "knitr", "kableExtra")                            # List packages
lapply(packages, library, character.only = TRUE)                                            # Load packages

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>% 
  dplyr::select(Guild, IMR.code) 
  
gear <- read.csv("./Data/MiMeMo gears.csv") 

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';', colClasses = c(RE = "character")) %>%               # Import IMR fishing data
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Area_Norway", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>% # Set column names
  filter(Area_Norway %in% c(1:4, 10:18, 20:25)) %>%                                         # Limit to areas of interest
  left_join(gear) %>%                                                                       # Attach labels
  left_join(guild) %>%                                                                      # Attach labels
  filter(Aggregated_gear != "Dropped") %>% 
  mutate(Weight = Weight/1000)                                                              # Convert Kg to tonnes

#### And effort across gears ####

effort <- IMR %>% 
  group_by(Aggregated_gear) %>% 
  summarise(Effort = sum(Fishing_time, na.rm = TRUE)) %>% 
  ungroup

ggplot(effort) +
  geom_col(aes(y = Aggregated_gear, x = Effort)) +
  theme_minimal() +
  labs(y = "Gear", x = "Total fishing time") +
  NULL

ggsave("gearbyeffort.png", dpi = 800, units = "cm", width = 10, height = 18)

## weight

effort <- IMR %>% 
  group_by(Aggregated_gear) %>% 
  summarise(Tonnes = sum(Weight, na.rm = TRUE)) %>% 
  ungroup

ggplot(effort) +
  geom_col(aes(y = Aggregated_gear, x = Tonnes)) +
  theme_minimal() +
  labs(y = "Gear", x = "Total landings (Tonnes)") +
  NULL

ggsave("gearbytonnes.png", dpi = 800, units = "cm", width = 10, height = 18)

#### Heatmap ####

map <- group_by(IMR, Guild, Aggregated_gear) %>%                                      # Summarise landings over all time
  summarise(Landings_tonnes = sum(as.numeric(Weight))) %>% 
  ungroup

ggplot(map) + 
  geom_raster(aes(x=as.factor(Guild), y=Aggregated_gear, fill = log(Landings_tonnes))) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#### Begin grouping of guilds ####

# matrix <- rename(IMR, Geartype = ENGLISH) %>%                    # Rename columns
#   group_by(Guild, Geartype) %>%                                  # Summarise landings over all time
#   summarise(Landings_tonnes = sum(as.numeric(Weight))) %>% 
#   ungroup %>% 
#   pivot_wider(names_from = Geartype, values_from = Landings_tonnes) %>% 
#   as.matrix(byrow = FALSE)
# 
# row.names(matrix) <- matrix[,1]
# matrix <- matrix[,2:ncol(matrix)] %>% 
#   t()
# 
# labels <- rownames(matrix)
# 
# matrix[is.na(matrix)] <- 0
# matrix <- apply(matrix, 2, as.numeric)
# 
# #### Clustering ###
# 
# ccas <- vegan::cascadeKM(vegan::decostand(matrix, "hellinger"), 2, 12) # Test all possible cluster numbers
# 
# results <- as.data.frame(ccas$results) %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   separate(rowname, sep = " ", into = c("Groups", NA)) %>% 
#   mutate(Groups = as.numeric(Groups),
#          Solution = ifelse(calinski == max(calinski), "Best", NA)) 
#   
# ggplot(results) +
#   geom_line(aes(x = Groups, y = calinski)) +
#   geom_point(aes(x = Groups, y = calinski, colour = Solution)) +
#   theme_minimal()
# 
# #### Show ordination ####
# 
# dist <- vegan::vegdist(matrix) %>% 
#    cmdscale() %>% 
#    as.data.frame() %>% 
#    rownames_to_column(var = "Guild") %>% 
#    mutate(Guild = labels[as.numeric(Guild)])
#  
#  ggplot(dist) + 
#    ggrepel::geom_label_repel(aes(x = V1, y = V2, label = Guild)) +
#    theme_minimal()
# 
# 
# #### With loadings ####
# library(vegan)
# 
# matrix_no_whale <- matrix[!labels %in% "Harpoon and similar unspecified types",] 
# 
# mds <- metaMDS(matrix, distance = "bray", autotransform = FALSE, trace = FALSE)
# mds
# 
# points <- as.data.frame(mds$points) %>% 
#   rownames_to_column(var = "Gear")
# 
# loadings <- scores(mds, display = "species", scaling = 0) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("Species") %>% 
#   mutate(#Species = as.numeric(str_remove(Species, "X")),
#          Strength = sqrt(NMDS1^2 + NMDS2^2)) %>% 
#   drop_na(NMDS1) #%>% 
# #  left_join(select(species, "Species", "Engelsk navn")) %>% 
# #  top_n(10, wt = Strength)
# 
# ggplot() +
#   ggrepel::geom_label_repel(data = points, aes(x = MDS1, y = MDS2, label = Gear)) +
#   geom_segment(data = loadings, aes(x=0, y = 0, xend = NMDS1, yend = NMDS2), arrow = arrow(length = unit(0.1,"cm"))) +
#   theme_minimal()
# 
# 
# ggplot() +
#   # geom_point(data = points, aes(x=MDS1, y = MDS2)) +
# #  ggrepel::geom_label_repel(data = points, aes(x = MDS1, y = MDS2, label = Gear)) +
#   ggrepel::geom_label_repel(data = loadings, aes(x = NMDS1, y = NMDS2, label = `Engelsk navn`), arrow = arrow(length = unit(0.1,"cm"))) +
# #  geom_segment(data = loadings, aes(x=0, y = 0, xend = NMDS1, yend = NMDS2), arrow = arrow(length = unit(0.1,"cm"))) +
#   theme_minimal()
