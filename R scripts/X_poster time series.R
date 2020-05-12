

Tidy_packages <- c("tidyverse", "furrr", "tictoc", "viridis", "ggnewscale") # List handy data packages
lapply(c(Tidy_packages), library, character.only = TRUE)      # Load packages

TS <- readRDS("~/Data/MiMeMo/TS") %>%               # Read in time series
  select(date, Compartment, Region, Temperature_avg, Ice_conc_avg) %>%
  pivot_longer(c(Temperature_avg, Ice_conc_avg), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = as.factor(Variable),
         Compartment = as.factor(Compartment))

levels(TS$Variable) <- list("Sea-ice\nconcentration" = "Ice_conc_avg", "Temperature" = "Temperature_avg")

levels(TS$Compartment) <- list("Inshore Shallow" = "Inshore S", "Offshore Shallow" = "Offshore S",
                               "Offshore Deep" = "Offshore D")

ggplot(TS, aes(x=date, y= Value, colour = Compartment)) +
  geom_line(size = 0.2) +
  geom_smooth(span = 0.008, size = 0.2, se = FALSE) +
  scale_color_viridis(discrete = T) +
  facet_grid(Variable ~ Region, scales = "free_y") +
  theme_minimal() +
  labs(title = "Compartment time series",
       subtitle = "Average model output from NEMO-MEDUSA", 
       y = NULL, x = "Date") +
#  theme(legend.position = "bottom",
#        legend.margin = margin(c(-10,5,5,5))) +  # for slides
  NULL

#ggsave("FIG_TS_Poster.png", plot = last_plot(), width = 28, height = 8.46, units = "cm", dpi = 500)
ggsave("FIG_TS_slides.png", plot = last_plot(), width = 14.7, height = 8.4, units = "cm", dpi = 500)
