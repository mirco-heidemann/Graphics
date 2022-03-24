## ----
## bubble map with ggplot2: GVZ Claims of 2017
##
## Feb. 2018, Mirco Heidemann
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("./data/")
pth_port <- ("../GVZ_Portfolio/")
pth_funct <- ("../R_functions/")
pth_out <- ("./out/")

# Libraries
library(tidyverse)
library(ggswissmaps)
library(maps)
library(viridis)
source(paste0(pth_funct, 'Fun_SchadPrepAll.R'))

# Get the world polygon and extract Switzerland
CH <- map_data("world") %>% filter(region == "Switzerland")

## Extract from ggswissmaps data the districts of canton ZH
ZH <- shp_df[["g1k15"]] %>% filter(KTNR %in% 1)

# ggplot(ZH, aes(x = long, y = lat, group = group)) +
#   geom_path() +
#   coord_equal() +
#   theme_white_f()

## ---- 
## aufbereitung und indexierung der gvz schaeden
gvz_index <- paste0(pth_port, 'versicherungsindex_gvz.csv')
dat_schaden <- paste0(pth_port, 'schaeden_201801_georef.csv')
## funktion 'Fun_SchadPrep.R':
## Input: - Pfad des csv schaden files
##        - Pfad des csv index file
## Output: data.frame der aufbereiteten schaeden, inkl. indexierung

schad <- schadPrep(dat_schaden, gvz_index) %>%
  filter(schad_art == "Elementar") %>% 
  filter(format(schad_datum, "%Y") == 2016) %>% 
  arrange(schad_index) %>% 
  mutate(schad_index = schad_index/1e3)

my_breaks = c(10, 20, 40, 80, 120, 160)

ggplot() +
  geom_polygon(data = ZH, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_point(data = schad, aes(x = geox, y = geoy, size = schad_index,
                               color = schad_index), alpha = 0.9) +
  scale_color_viridis(option = "magma", trans = "log", breaks = my_breaks,
                      name = "Schadensumme\n[Tsd. CHF]" ) +
  scale_size_continuous(name = "Schadensumme\n[Tsd. CHF]", breaks = my_breaks, range = c(1, 10)) +
  guides(color = guide_legend(), size=guide_legend()) +
  scale_alpha_continuous(name = "Schadensumme\n[Tsd. CHF]", range = c(1, 10),
                         breaks = my_breaks) +
  theme_void() + coord_equal() + 
  ggtitle("Elementarschäden 2016") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size = 16, hjust = 0.1, color = "#4e4d47",
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )

