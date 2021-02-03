#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "gganimate") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

#### data ####

SP <- readRDS("./Objects/SPATIAL.rds") %>%                                  # Read in spatial data
  .[["2000.S"]] %>% 
  select(Ice_conc, Month, Shore) %>% 
  drop_na()

# ggplot(SP) + 
#   geom_density(aes(x = Ice_conc), fill = "steelblue2") +
#   theme_minimal() +
#   transition_states(Month) +
# #  facet_grid(cols = vars(Shore)) +
#   labs(x = "Sea Ice concentration") +
#   ggtitle("Month = {month.abb[as.integer(closest_state)]}")

names(month.abb) <- seq(1:12) # Give months numeric name to work in faect

ggplot(SP) + 
  geom_density(aes(x = Ice_conc)) +
  theme_minimal() +
  labs(y = "Cumulative density", x = "Sea Ice Concentration", 
       caption = "Mean monthly values at a pixel from 2000-2009") +
  facet_wrap(vars(Month), scales = "free_y",
             labeller = labeller(Month = month.abb[])) +
  coord_cartesian(xlim = c(0,1), expand = F) +
  theme(axis.text.x = element_text(angle = 90))

# ggplot(SP, aes(Ice_conc)) + stat_ecdf(geom = "line") +
#   theme_minimal() +
#   labs(y = "Cumulative density", x = "Sea Ice Concentration", 
#        caption = "Mean monthly values at a pixel from 2000-2009") +
#   facet_wrap(vars(Month), 
#              labeller = labeller(Month = month.abb[])) +
#   coord_cartesian(xlim = c(0,1), expand = F) +
#   theme(axis.text.x = element_text(angle = 90))

#ggsave("Ice thesholds.png", width = 18, height = 10, units = "cm", dpi = 500)

#### Single year ####

files <- paste0("./Objects/Months/NM.", seq(1:12), ".2000.rds")

year <- map_dfr(files, ~{
  readRDS(.x) %>% 
    select(Ice_conc, Month, Shore)})
 
year2 <- drop_na(year)

names(month.abb) <- seq(1:12) # Give months numeric name to work in faect

ggplot(filter(year2, Shore == "Inshore"), aes(Ice_conc)) + stat_ecdf(geom = "line") +
  theme_minimal() +
  labs(y = "Cumulative density", x = "Sea Ice Concentration", 
       title = "Inshore zone",
       caption = "Mean monthly values at a pixel in 2000") +
  facet_wrap(vars(Month), 
             labeller = labeller(Month = month.abb[])) +
  coord_cartesian(xlim = c(0,1), expand = F) +
  theme(axis.text.x = element_text(angle = 90))

#ggsave("Ice thesholds in.png", width = 18, height = 10, units = "cm", dpi = 500)

ggplot(filter(year2, Shore == "Offshore"), aes(Ice_conc)) + stat_ecdf(geom = "line") +
  theme_minimal() +
  labs(y = "Cumulative density", x = "Sea Ice Concentration", 
       title = "Offshore zone",
       caption = "Mean monthly values at a pixel in 2000") +
  facet_wrap(vars(Month), 
             labeller = labeller(Month = month.abb[])) +
  coord_cartesian(xlim = c(0,1), expand = F) +
  theme(axis.text.x = element_text(angle = 90))

#ggsave("Ice thesholds off.png", width = 18, height = 10, units = "cm", dpi = 500)
