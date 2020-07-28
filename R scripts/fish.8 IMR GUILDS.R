
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
  filter(Area_Norway %in% c(1:4, 10:18, 20:25)) %>%                                         # Limit to areas of interest
  left_join(gear) %>%                                                                       # Attach labels
  left_join(species) %>%                                                                    # Attach labels
  mutate(Weight = Weight/1000)                                                              # Convert Kg to tonnes

#### Binding guilds ####

Guilds <- read.csv("./Old data/Fish Landings ICES-FAO/FAO_species_reference_list_v9.csv", header = TRUE)

Observed_species <- select(IMR, `Norsk navn`, `Engelsk navn`, `FAO`, `Latinsk navn`) %>% 
  distinct %>% 
  rename(Scientific.name = `Latinsk navn`) %>% 
  left_join(Guilds)

reclassify <- filter(Observed_species, is.na(Category)) %>% 
  write.csv(file = "./Objects/IMR_new_guilds.csv")


unobserved_species <- select(IMR, `Norsk navn`, `Engelsk navn`, `FAO`, `Latinsk navn`) %>% 
  distinct %>% 
  rename(Scientific.name = `Latinsk navn`) %>% 
  right_join(Guilds) %>% 
  filter(is.na(`Norsk navn`))


Observed_common <- select(IMR, `Norsk navn`, `Engelsk navn`, `FAO`, `Latinsk navn`) %>% 
  distinct %>% 
  rename(Common.name = `Engelsk navn`) %>% 
  left_join(Guilds) %>% 
  filter(is.na(Category))


Observed_both <- select(IMR, `Norsk navn`, `Engelsk navn`, `FAO`, `Latinsk navn`) %>% 
  distinct %>% 
  rename(Common.name = `Engelsk navn`) %>% 
  left_join(Guilds) %>% 
  filter(is.na(Category)) %>% 
  select(`Norsk navn`, Common.name, `FAO`, `Latinsk navn`) %>% 
  rename(Scientific.name = `Latinsk navn`,
         `Engelsk navn` = Common.name) %>% 
  left_join(Guilds) %>% 
  filter(is.na(Category)) %>% 
  write.csv(file = "./Objects/IMR_new_guilds.csv")


#### And effort across gears ####

effort <- IMR %>% 
  group_by(ENGLISH) %>% 
  summarise(Effort = sum(Fishing_time, na.rm = TRUE)) %>% 
  ungroup

ggplot(effort) +
  geom_col(aes(y = ENGLISH, x = Effort)) +
  theme_minimal() +
  labs(y = "Gear", x = "Total fishing time") +
  NULL

ggsave("gearbyeffort.png", dpi = 800, units = "cm", width = 10, height = 18)


## weight

effort <- IMR %>% 
  group_by(ENGLISH) %>% 
  summarise(Effort = sum(Weight, na.rm = TRUE)) %>% 
  ungroup

ggplot(effort) +
  geom_col(aes(y = ENGLISH, x = Effort)) +
  theme_minimal() +
  labs(y = "Gear", x = "Total fishing time") +
  NULL








#### Summarise and rank landings by gear ####

matrix <- rename(IMR, Geartype = ENGLISH) %>%                    # Rename columns
  group_by(Species, Geartype) %>%                                    # Summarise landings over all time
  summarise(Landings_tonnes = sum(as.numeric(Weight))) %>% 
  ungroup %>% 
  pivot_wider(names_from = Geartype, values_from = Landings_tonnes) %>% 
  as.matrix(byrow = FALSE)

row.names(matrix) <- matrix[,1]
matrix <- matrix[,2:ncol(matrix)] %>% 
  t()

matrix[is.na(matrix)] <- 0

#### Clustering ###

ccas <- vegan::cascadeKM(vegan::decostand(matrix, "hell"), 2, 20) # Test all possible cluster numbers

results <- as.data.frame(ccas$results) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  separate(rowname, sep = " ", into = c("Groups", NA)) %>% 
  mutate(Groups = as.numeric(Groups),
         Solution = ifelse(calinski == max(calinski), "Best", NA)) 
  
ggplot(results) +
  geom_line(aes(x = Groups, y = calinski)) +
  geom_point(aes(x = Groups, y = calinski, colour = Solution)) +
  theme_minimal()

#### Show ordination ####

# dist <- vegan::vegdist(matrix) %>% 
#   cmdscale() %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "Gear")
# 
# ggplot(dist) + 
#   ggrepel::geom_label_repel(aes(x = V1, y = V2, label = Gear)) +
#   theme_minimal()

#### With loadings ####
library(vegan)

matrix_no_whale <- matrix[!rownames(matrix) %in% "Harpoon and similar unspecified types",] 

mds <- metaMDS(matrix, distance = "bray", autotransform = FALSE, trace = FALSE)
mds

points <- as.data.frame(mds$points) %>% 
  rownames_to_column(var = "Gear")

loadings <- scores(mds, display = "species", scaling = 0) %>% 
  as.data.frame() %>% 
  rownames_to_column("Species") %>% 
  mutate(Species = as.numeric(str_remove(Species, "X")),
         Strength = sqrt(NMDS1^2 + NMDS2^2)) %>% 
  drop_na(NMDS1) %>% 
  left_join(select(species, "Species", "Engelsk navn")) %>% 
  top_n(10, wt = Strength)

ggplot() +
  ggrepel::geom_label_repel(data = points, aes(x = MDS1, y = MDS2, label = Gear)) +
#  geom_segment(data = loadings, aes(x=0, y = 0, xend = NMDS1, yend = NMDS2), arrow = arrow(length = unit(0.1,"cm"))) +
  theme_minimal()


ggplot() +
  # geom_point(data = points, aes(x=MDS1, y = MDS2)) +
#  ggrepel::geom_label_repel(data = points, aes(x = MDS1, y = MDS2, label = Gear)) +
  ggrepel::geom_label_repel(data = loadings, aes(x = NMDS1, y = NMDS2, label = `Engelsk navn`), arrow = arrow(length = unit(0.1,"cm"))) +
#  geom_segment(data = loadings, aes(x=0, y = 0, xend = NMDS1, yend = NMDS2), arrow = arrow(length = unit(0.1,"cm"))) +
  theme_minimal()



#### Species focussed ####

#### With loadings ####
# library(vegan)
# 
# matrix_no_whale <- matrix[!rownames(matrix) %in% "Harpoon and similar unspecified types",] 
# 
# mds <- matrix
# mds[mds == 0] <- NA
# mds <- janitor::remove_empty(mds)
# mds[is.na(mds)] <- 0
# 
# mds <- metaMDS(t(mds), distance = "bray", autotransform = FALSE, trace = FALSE)
# mds
# 
# points <- as.data.frame(mds$points) %>% 
#   rownames_to_column(var = "Species")
# 
# loadings <- scores(mds, display = "species", scaling = 0) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("Gear") %>% 
#   mutate(Strength = sqrt(NMDS1^2 + NMDS2^2)) %>% 
#   drop_na(NMDS1) #%>% 
# #  left_join(select(species, "Species", "Engelsk navn")) %>% 
# #  top_n(10, wt = Strength)
# 
# ggplot() +
#   ggrepel::geom_label_repel(data = points, aes(x = MDS1, y = MDS2, label = Species)) +
#   theme_minimal()
# 
# ggplot() +
#   ggrepel::geom_label_repel(data = loadings, aes(x = NMDS1, y = NMDS2, label = Gear), arrow = arrow(length = unit(0.1,"cm"))) +
#   theme_minimal()
# 
#### Heatmap ####

map <- rename(IMR, Geartype = ENGLISH) %>%                    # Rename columns
  group_by(Species, Geartype) %>%                                    # Summarise landings over all time
  summarise(Landings_tonnes = sum(as.numeric(Weight))) %>% 
  ungroup

ggplot(map) + 
  geom_raster(aes(x=as.factor(Species), y=Geartype, fill = log(Landings_tonnes))) + 
  theme_minimal()
