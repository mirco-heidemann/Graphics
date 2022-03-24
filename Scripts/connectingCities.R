## Heikes Geburtstagsmap
## Woher kommen all die Gaeste
## CONNECTING ROUTES ON MAP

library(tidyverse)
library(maps)
library(geosphere)

setwd("/Users/vanBrunkhorst/Documents/Statistical Computing/GeburiHeike/")
# setwd("C:/Users/mheidemann/Desktop/GeburiHeike")

## Draw an empty map
# jpeg("Cities.jpeg", width = 5.1, height = 4, units = 'in', res = 2000)
jpeg("Cities2.jpeg", width = 270, height = 205, units = 'mm', res = 1000)
par(mar=c(0,0,0,0))
map('world',col="#d9d9d9", fill=TRUE, bg=NA,
    mar=rep(0,4),border=NA, xlim=c(-10,16.5), ylim=c(41,55))

# myCol <- "slateblue"
myCol <- "#993404"

## Add cities ----

## create a data frame with the coordinates of the cities
Berlin=c(13.4,52.52)
Hamburg=c(9.99,53.55)
Bonn=c(7.095,50.73)
Dresden=c(13.737,51.05)
Düsseldorf=c(6.776, 51.22)
Nürnberg=c(11.06, 49.46)
Greifswald=c(13.39, 54.09)
Halle=c(12, 51.5)
Forst=c(14.65, 51.74)
Kamenz=c(14.096, 51.27)
# Gröditz=c(13.44, 51.41)
Putzkau=c(14.27, 51.1)
# Steina=c(14.05, 51.2)
Rostock=c(12.14, 54.089)
Kiel=c(10.135, 54.32)
Parchim=c(11.84, 53.43)
LaFloresta=c(2.075,41.45)
Zürich=c(8.55,47.37)

df=rbind(Berlin, Hamburg, Bonn, Dresden, Düsseldorf, Nürnberg, Greifswald,
           Halle, Forst, Kamenz, Putzkau, Rostock, Kiel, Parchim,
           LaFloresta, Zürich) %>% as.data.frame()
colnames(df)=c("long","lat")

## label fuer darstellung
df$citylabel <- row.names(df)
df <- df %>%
  mutate(citylabel = replace(citylabel, citylabel == "LaFloresta", "La Floresta"))

## add it to the map using the points function
points(x=df$long, y=df$lat, col=myCol, cex=1.6, pch=20)
text(df$citylabel[-1], x=df$long[-1], y=df$lat[-1],
     col=myCol, cex=0.8, offset = 0.5, font = 2,
     pos=c(1,1,2,3,4,4,2,4,4,1,3,2,4,2,4))

# Show Connections ----

## between Hamburg and Berlin
inter <- gcIntermediate(Berlin,  Hamburg, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Bonn and Berlin
inter <- gcIntermediate(Berlin,  Bonn, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Dresden and Berlin
inter <- gcIntermediate(Berlin,  Dresden, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Duesseldorf and Berlin
inter <- gcIntermediate(Berlin,  Düsseldorf, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Nuernberg and Berlin
inter <- gcIntermediate(Berlin,  Nürnberg, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Greifswald and Berlin
inter <- gcIntermediate(Berlin,  Greifswald, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Halle and Berlin
inter <- gcIntermediate(Berlin,  Halle, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Forst and Berlin
inter <- gcIntermediate(Berlin,  Forst, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Kamenz and Berlin
inter <- gcIntermediate(Berlin,  Kamenz, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Putzkau and Berlin
inter <- gcIntermediate(Berlin,  Putzkau, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

# ## between Steina and Berlin
# inter <- gcIntermediate(Berlin,  Steina, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)

## between Rostock and Berlin
inter <- gcIntermediate(Berlin,  Rostock, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Kiel and Berlin
inter <- gcIntermediate(Berlin,  Kiel, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between Parchim and Berlin
inter <- gcIntermediate(Berlin,  Parchim, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## between LaFloresta and Berlin
inter <- gcIntermediate(Berlin,  LaFloresta, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

# Between Zuerich and Berlin
inter <- gcIntermediate(Berlin,  Zürich, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col=myCol, lwd=1)

## Berlin extra point
points(x=df$long[1], y=df$lat[1], col="#ec7014", cex=2, pch=20)
text(df$citylabel[1], x=df$long[1], y=df$lat[1],
     col="#ec7014", cex=0.7, font=2, offset = 0.5, pos=4)

dev.off()
