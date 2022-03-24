## ----------------------------------------------------------------------------
##  Grafik "Erdbebenfonds Entwicklung"
##  mit dem package ggPlot
##
##  Mirco Heidemann, Jan 2017
## ----------------------------------------------------------------------------

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("./data/")
pth_port <- ("../GVZ_Portfolio/")
pth_funct <- ("../R_functions/")
pth_out <- ("./presentation/")

library(dplyr)
## daten einlesen
df <- read.csv2(paste0(pth_data, "Erdbebenfonds.csv"), stringsAsFactors = FALSE) %>% 
  mutate(jahr = as.integer(Jahr),
         vers_kapital_mia = as.numeric(Versicherungskapital..Mia.),
         erdbeben_fonds_mio = as.numeric(Erdbebenfonds..Mio.),
         fonds_max_mio = as.numeric(Erdbebenfonds..Gesetzlich.max.0.6...des.VersKapital)) %>% 
  dplyr::select(5:8)

## --- Diagramm mit ggplot
library(ggplot2)
library(reshape2) ## melt

df_melt <- melt(df, id = "jahr")

theme_set(theme_bw(base_size = 12))

## Diagramm mit sekundaerer y-achse
ggplot(df_melt, aes(x = jahr, y = value, group = variable, colour = variable)) +
  geom_line(size=0.5) +
  geom_point(size=2) +
  scale_color_manual(name = "",
                     labels = c("Versicherungskapital [Mia]",
                                "Erdbebenfonds [Mio]",
                                "Gesetzliches Maximum des Erdbebenfonds: 0.6 Promille des Versicherungskapital"),
                     values=c("#003c30", "#8c510a", "#dfc27d")) +
  scale_y_continuous(name = "Versicherungskapitla in Mia CHF",
                     sec.axis = sec_axis(~ . /1, name = "Erdbebenfonds in Mio CHF")) +
  scale_x_continuous(breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 2),
                     expand = c(0.005, 0.005)) +
  
  labs(title = "Entwicklung des Erdbebenfonds seit 1990", 
       subtitle = "", 
       caption = "Quelle: GVZ") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        panel.grid = element_line(colour = "gray"),
        panel.grid.major = element_line(colour = "gray"),
        panel.grid.minor = element_line(colour = "gray"),
        legend.position = "bottom", # "none" for no legend
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")

