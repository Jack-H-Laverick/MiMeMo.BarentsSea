
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(tidyverse)
library(viridis)

timings <- readRDS("./Objects/Run time.rds")

#### Plot ####

ggplot(timings, aes(x = Script, y = Minutes, colour = Type)) +
  geom_col(fill = NA) +
  scale_colour_viridis(discrete = TRUE) +
  annotate("label", x = 8, y = 400, label = paste0("Run time:\n", round(sum(timings$Minutes)/60, digits = 2), " hours"), colour = "white", fill = "black") +
  theme_minimal() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank()) +
  labs(x = NULL) +
  NULL

#### Cool circular bar plot ####

#Create dataset
data <- data.frame(
  individual= timings$Script,
  group= timings$Type,
  value= timings$Minutes)


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# Make the plot
ring <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +   # Begin plot
  # Add axis marks
  annotate("rect", xmin = 0.5, ymin = 0, xmax = max(data$id) - 0.5, ymax = 60, colour = "white", alpha=0.1, fill = "cornflowerblue") +
  annotate("rect", xmin = 0.5, ymin = 60, xmax = max(data$id) - 0.5, ymax = 120, colour = "white", alpha=0) +
  annotate("rect", xmin = 0.5, ymin = 120, xmax = max(data$id) - 0.5, ymax = 180, colour = "white", alpha=0.1, fill = "cornflowerblue") +
  annotate("rect", xmin = 0.5, ymin = 180, xmax = max(data$id) - 0.5, ymax = 240, colour = "white", alpha=0) +
  annotate("rect", xmin = 0.5, ymin = 240, xmax = max(data$id) - 0.5, ymax = 300, colour = "white", alpha=0.1, fill = "cornflowerblue") +
  # Add text labels for axis marks
  annotate("text", x = rep(max(data$id),6), y = c(0, 60, 120, 180, 240, 300), label = c("0", "1", "2", "3", "4", "5") , color="grey", size=3 , angle=0, fontface="bold", hjust= 0.5) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity") +
  scale_fill_viridis(discrete = T, name = NULL) +
  ylim(-150,400) +        # More - creates more space in the centre, if a bar is larger than max it won't display
  annotate("text", x = max(data$id), y = 350, label = paste0("Total project run time: ", round(sum(data$value, na.rm = T)/60, digits = 2), " hours"),
           size = 3) +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(-2.5, -2.5,-3.5,-2.5), "cm")
       ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
 # labs(caption = paste0("Total project run time: ", round(sum(data$value, na.rm = T)/60, digits = 2), " hours")) +
  NULL

ring

ggsave("./Project runtime.png", plot = last_plot(), width = 16, height = 16, units = "cm", dpi = 500)
