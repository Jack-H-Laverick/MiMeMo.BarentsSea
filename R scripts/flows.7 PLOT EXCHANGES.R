
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "tidygraph", "ggraph")                           # List GIS packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

H_Flows <- readRDS("./Objects/H-Flows.rds") %>%                             # Load flow data
  mutate(Neighbour = ifelse(Shore == "Inshore" & Neighbour == "Ocean", "Shelf", Neighbour)) %>% 
  filter(Year == 1980 & Month == 1)                                         # Use an example year

V_Flows <- readRDS("./Objects/V-Flows.rds")

#### Combine vertical and horizontal flows ####

Exchanges <- mutate(V_Flows, Value = as.numeric(Value)) %>%  
  mutate(Direction = ifelse(Value > 0, "In", "Out"),    # Introduce missing columns
         Shore = "Offshore",
         Neighbour = "Offshore (D)",
         Depth = "S") %>%
  filter(Flow != "Eddy Diffusivity") %>%                                    # Limit to velocities
  select(-c(Flow, Date)) %>%
  rename(Flow = Value) %>%                      
  filter(Year == 1980 & Month == 1) %>%                                     # Limit to example month
  bind_rows(H_Flows)                                                        # Combine to horizontal flows

#### Plot vertical currents only ####

# ggplot(TS, aes(x=Date, y= Value)) +
#   geom_line(size = 0.2) +
#   geom_smooth(span = 0.008, size = 0.2, se = FALSE) +
#   facet_grid(Flow ~., scales = "free_y") +
#   theme_minimal() +
#   labs(title = "NM time series of vertical currents") +
#   NULL
# 
#  ggsave("./Figures/flows/TS_Vertical currents.png", plot = last_plot(), width = 16, height = 10, units = "cm", dpi = 500)

#### Create network ####

flows <- mutate(Exchanges, source = ifelse(Direction == "In", paste0(Neighbour, " (", Depth, ")"), # Create compartment levels, correctly based on direction of the flow
                                           paste0(Shore, " (", Depth, ")")),
                destination = ifelse(Direction == "In", paste0(Shore, " (", Depth, ")"),
                                     paste0(Neighbour, " (", Depth, ")"))) %>%
  mutate(source = ifelse(!grepl("Offshore", source), str_sub(source, end = -5), source),                       # Remove the depth label if not an offshore label
         destination = ifelse(!grepl("Offshore", destination), str_sub(destination, end = -5), destination)) %>%
  select(source, destination, Flow) %>%                                     # Retain relevant information
  rename(weight = Flow) %>%
  mutate(source = gsub("(D) (S)", "(D)", source, fixed = T),                # Remove double depth labels when combing H and V flows
       destination = gsub("(D) (S)", "(D)", destination, fixed = T))

nodes <- tibble(label = unique(flows$source)) %>%                           # Create an index of nodes which need to be connected
  mutate(id = c(4, 1, 2, 5, 3)) %>%                                              # Specify the order you want the compartments plotted in
  arrange(id)                                                               # Set the order

edges <- flows %>%                                                          # Create the connections between nodes
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id) %>%
  select(from, to, weight) %>%
  mutate(weight = abs(weight))                                  

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)     # Set file format for plotting

#### Plot network ####

cols <- c("Inshore" = "Yellow", "Ocean" = "lightblue2", "Offshore (S)" = "Yellow3", 
          "Offshore (D)" = "Yellow3", "Shelf" = "lightblue2")    

ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight, colour = weight, ), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 4)) +
  geom_node_label(aes(label = label, fill = label), size = 3) +
  scale_fill_manual(values = cols) +
  geom_curve(aes(x = 1.7, y = .75, xend = 1.9, yend = .75), curvature = -0.5, arrow = arrow(length = unit(0.1, "cm"), type = "closed"), size = 0.25) +
  geom_curve(aes(x = 1.9, y = .55, xend = 1.7, yend = .55), curvature = - 0.5, arrow = arrow(length = unit(0.1, "cm"), type = "closed"), size = 0.25) +
  annotate("text", x = 1.8, y = 0.65, label = "Direction", size = 3) +
  labs(caption = "Water exchanges between model zones", edge_colour = "Flow") +
  theme_graph() +
  coord_cartesian(xlim = c(0.85,5.1)) +                                     # Set the plot window to avoid clipping labels
  guides(edge_width = FALSE, fill = FALSE,
         edge_colour = guide_edge_colourbar(barwidth = 0.5, barheight = 10)) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),                           # Remove padding around plot
        legend.margin=margin(c(5,2,5,-5))) +                                # Reduce padding by legend
  NULL

ggsave("./Figures/flows/flows.png", plot = last_plot(), width = 10, height = 8, units = "cm", dpi = 500)

#### Turn off flows ####

edges_needed <- edges %>%
  mutate(Balance = ifelse(from == 5 & to %in% c(3,4) | from == 3 & to %in% c(2,4) | # Identify which flows are automatically balanced by Strath E2E
                          from == 1 & to == 2 | from == 4 & to == 3 | from == 2 & to == 1, FALSE, TRUE))

routes_tidy <- tbl_graph(nodes = nodes, edges = edges_needed, directed = TRUE)     # Set file format for plotting

ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight, colour = weight, edge_linetype = Balance), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 4)) +
  geom_node_label(aes(label = label, fill = label), size = 3) +
  scale_fill_manual(values = cols) +
  geom_curve(aes(x = 1.7, y = .75, xend = 1.9, yend = .75), curvature = -0.5, arrow = arrow(length = unit(0.1, "cm"), type = "closed"), size = 0.25) +
  geom_curve(aes(x = 1.9, y = .55, xend = 1.7, yend = .55), curvature = - 0.5, arrow = arrow(length = unit(0.1, "cm"), type = "closed"), size = 0.25) +
  annotate("text", x = 1.8, y = 0.65, label = "Direction", size = 3) +
  labs(caption = "Water exchanges between model zones, dashed lines are calculated to conserve water volumes in compartments.", edge_colour = "Flow") +
  theme_graph() +
  coord_cartesian(xlim = c(0.85,5.1)) +                                     # Set the plot window to avoid clipping labels
  guides(edge_width = FALSE, fill = FALSE,
         edge_colour = guide_edge_colourbar(barwidth = 0.5, barheight = 7)) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),                           # Remove padding around plot
        legend.margin=margin(c(5,2,5,-5))) +                                # Reduce padding by legend
  NULL

ggsave("./Figures/flows/flows_balance.png", plot = last_plot(), width = 16, height = 8, units = "cm", dpi = 500)
