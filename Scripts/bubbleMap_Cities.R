## ----
## bubble map with ggplot2: Cities and Population of Switzerland
##
## Feb. 2018, Mirco Heidemann
## ----

# Libraries
library(tidyverse)
library(maps)
library(viridis)

## Arbeitsverzeichnis waehlen
setwd('I:/Statistical Computing/Bubble_map_schaden/data')

# Get the world polygon and extract Switzerland
CH <- map_data("world") %>% filter(region == "Switzerland")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc == "Switzerland")

## reorder the dataset first! Big cities appear later, on top
data %>%
  arrange(pop) %>% # This reorder your data frame
  mutate(name = factor(name, unique(name))) %>% # ggplot reorder the levels of the factor
  ggplot() +
  geom_polygon(data = CH, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_point(aes(x = long, y = lat, size = pop, color = pop), alpha = 0.9) +
  scale_size_continuous(range = c(1,12)) +
  scale_color_viridis(trans = "log") +
  theme_void() + coord_map() + theme(legend.position = "none") # + ylim(50,59)

## custom a little bit for a better looking result
mybreaks = c(0.02, 0.04, 0.08, 1, 7)
data %>%
  arrange(pop) %>%
  mutate(name = factor(name, unique(name))) %>%
  mutate(pop = pop/1000000) %>%
  ggplot() +
  geom_polygon(data = CH, aes(x = long, y = lat, group = group), fill = "grey",
               alpha = 0.3) +
  geom_point(aes(x = long, y = lat, size = pop, color = pop, alpha = pop), shape = 20,
               stroke = FALSE) +
  scale_size_continuous(name = "Bevölkerung (in Mio.)", trans = "log", range = c(1, 12),
                        breaks = mybreaks) +
  scale_alpha_continuous(name = "Bevölkerung (in Mio.)", trans = "log", range = c(0.1, .9),
                         breaks = mybreaks) +
  scale_color_viridis(option = "magma", trans = "log", breaks = mybreaks,
                      name = "Bevölkerung (in Mio.)" ) +
  theme_void() + coord_map() + #ylim(50,59) + 
  guides(colour = guide_legend()) +
  ggtitle("Die 1000 grössten Städte der Schweiz") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size = 16, hjust = 0.1, color = "#4e4d47",
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )

