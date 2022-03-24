## ----------------------------------------------------------------------------
##  Grafik mit dem package ggPlot
##  Am Bsp. der Kundenumfrage 2016 - Bekanntheit der ESP Dienstleistung der GVZ 
##
##  Mirco Heidemann, Nov 2016
## ----------------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('I:/R/Grafik mit ggPlot/')

## Funktionen und Packages laden
require(maptools)
require(broom) ## funktion "tidy"
require(plyr) ## funktion "join"
require(ggplot2)
require(grid) ## Fussnote, arrangeGrob
require(gridExtra) ## Fussnote, arrangeGrob

## verschiedene ordner-Pfade definieren:
shape.path <- paste('J:/Naturgefahren/FG2_Datenanalysen/',
                    'Daten-Grundlagen GIS/Grunddaten_KantonZH/',sep='')

## shapes fuer plots einlesen
sh.seen <-readShapeSpatial(paste(shape.path,'seendet_250.shp',sep=""))
sh.schaetzkreis <- readShapeSpatial(paste(shape.path,'gvz_schaetzkreise',sep=""))

## Umfrage Ergebnisse einlesen
dat.path <- 'J:/Naturgefahren/Korrespondenz_Kommunikation/Kundenumfrage 2016/'
dat <- read.csv2(paste(dat.path,'Kundenumfrage 2016 ESP.csv',sep=''))
dat$Ja.Anteil <- as.numeric(as.character(dat$Ja.Anteil))

data <- data.frame(id=rownames(sh.schaetzkreis@data), 
                   id.bezirk=sh.schaetzkreis@data$schatzkrID)
## von factor zu character
data$id <- as.character(data$id)

## merge Ja-Anteil zu den Schaetzbezirken
data    <- merge(data,dat,by.x="id.bezirk",by.y="ID.Bezirk",all.x=T)

## konvertiere R objekt in einen data frame
df.schaetzkreis <- tidy(sh.schaetzkreis)
df.seen <- tidy(sh.seen)

## join data zum "shape-data frame"
df.schaetzkreis <- join(df.schaetzkreis, data, by="id")

## Grafik-Thema definieren
my.theme <- theme_bw()+
  theme(panel.border = element_blank(),
        #panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(b=0), size = 16),
        plot.subtitle=element_text(margin=margin(t=5, b = 10),size = 12), 
        legend.position = c(0.93, 0.05),
        axis.text=element_blank(),axis.ticks=element_blank())

## schaetzkreis-data.frame zeichnen
ggp.raw <- ggplot(data=df.schaetzkreis, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = Ja.Anteil), ## zeichne polygone
               col = "grey40", size=0.1) + ## zeichne Bezirksgrenzen
  coord_equal() + ## ohne koordinaten verzerrung
  scale_fill_gradient(low = "#ff4444", high = "#ffffcc", 
                      space = "Lab", na.value = "grey80",
                      guide = "colourbar") +
  ## Seen zeichnen
  geom_polygon(data=df.seen, aes(x=long, y=lat, group=group), fill='#b9cfdd')

ggp <- ggp.raw +
  labs(x="", y="",
       title='Bekanntheit der Elementarschadenprävention nach Schätzbezirk',
       subtitle='Kundenumfrage GVZ 2016') +
  my.theme ## Grafik-Theme verwenden (Hinergrund, Achsenbeschriftung, usw.)

## Fussnote einfuegen
grid.newpage()
footnote <- paste('Übersichtsplan Kanton Zürich © Amt für Raumentwicklung,',
                  ' bearbeitet durch die GVZ - Mirco Heidemann, November 2016',
                  sep='')
ggp <- arrangeGrob(ggp, bottom = textGrob(footnote, x = 0, hjust = -0.1,
                                        vjust=0.1, gp = gpar(fontface = "italic",
                                                             fontsize = 8)))

# ## A4-PDF speichern (ggplot rechnet in zoll)
# pdf("Kundenumfrage2016.esp.pdf", paper="a4",  width=8.5, height=11)
# grid.draw(ggp)
# dev.off()

## Grafik in RStudio-Umgebung in vereinfachter Form  anzeigen
ggp.raw

