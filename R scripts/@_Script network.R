
## Automatically trawl my R scripts to find objects being written and read, 
## and then connect these scripts for a network graph

library(tidyverse)                                                         # Enter the tidyverse
library(visNetwork)
library(viridis)

Scripts <- list.files("./R scripts",  pattern = ".R", recursive = T, full.names = T) %>%  # Read in all the Rscripts 
  as.data.frame() %>% 
  filter(!grepl('@|X_|Z_|rayshader|targets|R chive', .)) %>%                       # Ignore files labelled in these ways
  mutate(Script = as.character(.)) %>%                                    
  select(-.)

Links <- function(script) {
  
  #script <- Scripts[16,1]                                                  # Testing
  example <- readLines(script)                                              # Get each line in a script as a character vector
  
  imports <- grepl("readRDS\\(", example)                                   # Which lines use readRDS
  exports <- grepl("saveRDS\\(", example)                                   # Which lines use saveRDS
  functions <- grepl("source\\(", example)                                  # Does the script call a function file?
  
  From <- example[exports] %>%                                              # Which objects come from this script
    data.frame(Object = .) %>%                                              # A column of objects being saved
    mutate(Object = as.character(Object),                                   
           From = script) %>%                                               # Attach the script name
    separate(From, into = c(NA, "From"), sep = "./R scripts/") %>%          # Shrink the script name
    separate(Object, into = c(NA, "Object"), sep = "[.]")                   # Isolate the object name between ". and )"
  
  To <- example[imports] %>%                                                # Which objects are read into this script
    data.frame(Object = .) %>%                                              
    mutate(Object = as.character(Object),
           To = script) %>% 
    separate(To, into = c(NA, "To"), sep = "./R scripts/") %>%              # Shrink the script name
    separate(Object, into = c(NA, "Object"), sep = "[.]")                   # Isolate the file name between ". and )"
  
  Functions <- example[functions] %>% 
    data.frame(From = .) %>% 
    filter(!grepl('Region file', .)) %>%                                    # Ignore files labelled in these ways
    mutate(From = as.character(From),
           To = script) %>% 
    separate(From, into = c(NA, "From"), sep = "./R scripts/") %>%          # Shrink the script name
    separate(To, into = c(NA, "To"), sep = "./R scripts/") %>%              # Shrink the script name
    separate(From, into = c("From", NA), sep = " ") %>%                     # Shrink the script name
    mutate(From = paste(From, "FUNCTIONS.R"))
  
  Links <- bind_rows(From, To) %>% 
    bind_rows(Functions)
  
  return(Links)  
}    # Function to return the files read and saved by a script, and any function files called

#### Establish the relations file ####

Scripts2 <- filter(Scripts, !grepl('FUNCTIONS', Scripts$Script))          # Ignore functions files as relationships aren't object mediated (but keep Scripts for defining nodes later)

all <- map(Scripts2[,1], Links) %>%                                       # Check for links in all R scripts
  bind_rows()

From <- select(all, -To) %>%                                              # Grab just the files creating objects
  drop_na() 

To <- select(all, -From) %>%                                              # Grab just the files reading in objects
  drop_na()

#### Format for a network graph ####

nodes <- data.frame(id = seq(1:length(unique(Scripts$Script))),
                    label = as.character(unique(Scripts$Script))) %>% 
  separate(label, into = c(NA, "label"), sep = "./R scripts/")

Edges <- full_join(From, To) %>%                                          # Join Tos and Froms by the shared object 
  drop_na() %>%                        
  bind_rows(filter(all, is.na(Object)))  %>%                              # Add in the relationships to functions files, which aren't mediated by an object
  distinct() %>%                                                          # Remove any repeated links
  left_join(nodes, by = c("From" = "label")) %>%                          # add in numerical code for scripts
  select(-From, from = id) %>%                                            # relabel
  left_join(nodes, by = c("To" = "label")) %>%                            # repeat relabelling for to column
  select(-To, to = id) %>% 
  mutate(arrows = "middle")                                               # Add arrows in the middle of the edge when plotting

nodes <- separate(nodes, label, into = c("group", NA), remove = F, sep = "[.]") # Add a group field for colour

#### Work out node level ####

counter <- 1

start <- setdiff(Edges$from, Edges$to) %>%  na.omit()                     # Which scripts are present in the from column, but not to 
nodes <- mutate(nodes, level = ifelse(id %in% start, counter, NA))        # Label those scripts as level one

then <- filter(Edges, from %in% start)[["to"]]                            # Which are the next scripts in the chain?

while (length(then) != 0) {

counter <- counter +1

nodes <- mutate(nodes, level = ifelse(id %in% then, counter, level))
  
then <- filter(Edges, from %in% then)[["to"]]  # Which are the next scripts
  
}                                           # Until we've visited all the dependencies...

nodes <- mutate(nodes, level = ifelse(is.na(level), 1, level))            # Update this scripts with the next level label

saveRDS(list(edges = Edges, nodes = nodes), "./Objects/network.rds")      # Save an object to build the network in the website.

#### Graph it ####

v <-viridis(length(unique(nodes$group)))                                  # get viridis colours for each group

toy <- visNetwork(nodes, Edges, width = "100%", height = "1500") %>%      # Build the network
  #visLayout(randomSeed = 10) %>%                                         # Control the randomisation of the layout
  visHierarchicalLayout(direction = "UD") %>%                             # Control the randomisation of the layout
  visLegend(width = 0.15) %>%                                             # Add a legend
  visOptions(highlightNearest = list(enabled = TRUE, labelOnly = FALSE),  # Control the highlighting when you select a script
             nodesIdSelection = TRUE, clickToUse = TRUE, selectedBy = "group") %>% # Allow the user to select a script from a drop down list
  visGroups(groupname = "bathymetry", shape = "dot", color = list(background = v[1], border = v[1], # Control colouring and highlighting per group
                                                                highlight = list(background = "white", border = v[1]))) %>%
  visGroups(groupname = "nemo-medusa", shape = "dot", color = list(background = v[2], border = v[2],
                                                         highlight = list(background = "white", border = v[2]))) %>% 
  visGroups(groupname = "flows", shape = "dot", color = list(background = v[3], border = v[3],
                                                             highlight = list(background = "white", border = v[3]))) %>% 
  visGroups(groupname = "fish", shape = "dot",color = list(background = v[4], border = v[4],
                                                          highlight = list(background = "white", border = v[4]))) %>% 
  visGroups(groupname = "saltless", shape = "dot", color = list(background = v[5], border = v[5],
                                                                highlight = list(background = "white", border = v[5]))) %>% 
  visGroups(groupname = "strathE2E", shape = "dot", color = list(background = v[6], border = v[6],
                                                                 highlight = list(background = "white", border = v[6])))
toy
#visSave(toy, file = "./Project network tool.html")                       # Save as HTML file
