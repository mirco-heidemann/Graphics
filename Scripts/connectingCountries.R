## Heikes Geburtstagsmap
## Video Gruesse
## CONNECTING COUNTRIES ON MAP

library(tidyverse)
library(maps)
library(geosphere)
library(dplyr)

setwd("/Users/vanBrunkhorst/Documents/Statistical Computing/GeburiHeike/")
# setwd("C:/Users/mheidemann/Desktop/GeburiHeike")

dfText <- read.csv2("videos.csv")
dfText$Name <- as.character(dfText$Name)
dfText$Stadt <- as.character(dfText$Stadt)

## label fuer darstellung
dfText <- dfText %>%
  mutate(Stadt = replace(Stadt, Stadt == "Zurich", "Zürich")) %>%
  mutate(Stadt = replace(Stadt, Stadt == "Schoneiche", "Schöneiche")) %>%
  mutate(Stadt = replace(Stadt, Stadt == "Saarbrucken", "Saarbrücken")) %>%
  mutate(Name = replace(Name, Name == "Barbel und Lotti", "Bärbel und Lotti"))

## Draw an empty map
# jpeg("videos.jpeg", width = 5, height = 3.3, units = 'in', res = 2000)
jpeg("videos2.jpeg", width = 270, height = 205, units = 'mm', res = 2000)
par(mar=c(0,0,0,0))
map('world',col="#d9d9d9", fill=TRUE, bg=NA,
    mar=rep(0,4),border=NA, xlim=c(-16,165), ylim=c(-38,78))

# myCol <- "slateblue"
myCol <- "#993404"

## Add cities ----

## create a data frame with the coordinates of the cities
Berlin=c(13.4,52.52)
Chengdu=c(104.07,30.67)
Kiew=c(32,50.45)
ByronBay=c(153.6,-28.65)
Brisbane=c(153.03,-27.47)
Inverell=c(151.1,-29.76)
Budapest=c(19.04,47.5)
SantCugat=c(2.085,41.47)
Zürich=c(8.55,47.37)
NeuWulmstorf=c(10.2,53.47)
Coesfeld=c(7.17,51.5)
#Steina=c(14.05, 51.2)
Kamenz=c(14.09, 51.27)
#Wien=c(16.37,48.21)
Bremen=c(8.8,53.08)
#Halle=c(12, 51.5)
Saarbrücken=c(7,49.24)
#Pulsnitz=c(14.02,51)
#Schöneiche=c(13.71,52.47)
Perth=c(115.86,-31.95)

df=rbind(Berlin, Chengdu, Kiew, ByronBay, Brisbane, Inverell, Budapest,
           SantCugat, Zürich, NeuWulmstorf, Coesfeld, Kamenz, Bremen,
           Saarbrücken, Perth) %>% as.data.frame()
colnames(df)=c("long","lat")

## label fuer darstellung
df$citylabel <- row.names(df)
df <- df %>%
  mutate(citylabel = replace(citylabel, citylabel == "SantCugat", "Sant Cugat")) %>% 
  mutate(citylabel = replace(citylabel, citylabel == "Kamenz", "Kamenz/Steina")) %>% 
  mutate(citylabel = replace(citylabel, citylabel == "ByronBay", "Byron Bay")) %>% 
  mutate(citylabel = replace(citylabel, citylabel == "NeuWulmstorf", "Neu Wulmstorf"))

## add it to the map using the points function
points(x=df$long, y=df$lat, col=myCol, cex=0.9, pch=20)
text(df$citylabel, x=df$long, y=df$lat,
     col=myCol, cex=0.6, offset = 0.3,
     pos=c(4,4,4,4,3,2,4,4,2,3,2,4,2,2,4), font = 2)
# segments(df$long,df$lat,df$long,df$lat+3,col="#ec7014")

dev.off()

# ## Berlin extra point
# points(x=df$long[1], y=df$lat[1], col="#ec7014", cex=2, pch=20)
# text(rownames(df)[1], x=df$long[1], y=df$lat[1],
#      col="#ec7014", cex=0.7, font=2, offset = 0.5, pos=4)

## add a legend ----
## legend text
# p1 <- rep(letters[1:26],length=nrow(df))
# p2 <- rep(letters[17:26],length=nrow(df))
# dfText <- data.frame(p1,p2)

# legend(x=-10, y=27, legend=paste0(dfText$Name,": ",dfText$Stadt), col=myCol,
#        bty="n", pt.cex=0.8, cex=0.6, border = "black",pch=20, title="",
#        inset=0.2, x.intersp=0.8, y.intersp=0.9, text.col=myCol, title.adj=0.15)

# # Show Connections ----
# 
# ## between Hamburg and Berlin
# inter <- gcIntermediate(Berlin,  Hamburg, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Bonn and Berlin
# inter <- gcIntermediate(Berlin,  Bonn, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Dresden and Berlin
# inter <- gcIntermediate(Berlin,  Dresden, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Duesseldorf and Berlin
# inter <- gcIntermediate(Berlin,  Düsseldorf, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Nuernberg and Berlin
# inter <- gcIntermediate(Berlin,  Nürnberg, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Greifswald and Berlin
# inter <- gcIntermediate(Berlin,  Greifswald, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Halle and Berlin
# inter <- gcIntermediate(Berlin,  Halle, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Forst and Berlin
# inter <- gcIntermediate(Berlin,  Forst, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Kamenz and Berlin
# inter <- gcIntermediate(Berlin,  Kamenz, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Groeditz and Berlin
# inter <- gcIntermediate(Berlin,  Gröditz, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Steina and Berlin
# inter <- gcIntermediate(Berlin,  Steina, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Rostock and Berlin
# inter <- gcIntermediate(Berlin,  Rostock, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Kiel and Berlin
# inter <- gcIntermediate(Berlin,  Kiel, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between Parchim and Berlin
# inter <- gcIntermediate(Berlin,  Parchim, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# ## between LaFloresta and Berlin
# inter <- gcIntermediate(Berlin,  LaFloresta, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)
# 
# # Between Zuerich and Berlin
# inter <- gcIntermediate(Berlin,  Zürich, n=50, addStartEnd=TRUE, breakAtDateLine=F)
# lines(inter, col=myCol, lwd=1)

# ## Berlin extra point
# points(x=df$long[1], y=df$lat[1], col="#ec7014", cex=2, pch=20)
# text(rownames(df)[1], x=df$long[1], y=df$lat[1],
#      col="#ec7014", cex=0.7, font=2, offset = 0.5, pos=4)
