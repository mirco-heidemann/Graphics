## Heikes Geburtstagsmap
## TIME LINE

library(dplyr)
setwd("/Users/vanBrunkhorst/Documents/Statistical Computing/GeburiHeike/")
# setwd("C:/Users/mheidemann/Desktop/GeburiHeike")

dat <- read.csv2("Jahreszahlen.csv")

## nach jahr aufsteigend sortieren
dat <- arrange(dat, Jahr)

## manually order der namen
dat$ord <- c(2,3,1,4:19,22,21,20,23:56)
dat <- arrange(dat, ord)
dat$Namen <- as.character(dat$Namen)

## label fuer darstellung
dat$nameLabel <- dat$Namen
dat <- dat %>%
  mutate(nameLabel = replace(nameLabel, nameLabel == "Annegret und Gotz",
                             "Annegret\nund Götz")) %>%
  mutate(nameLabel = replace(nameLabel, nameLabel == "Andre", "André"))%>%
  mutate(nameLabel = replace(nameLabel, nameLabel == "Marina und Roland",
                             "Marina\nund Roland")) %>% 
  mutate(nameLabel = replace(nameLabel, nameLabel == "Jona", "Jona   ")) %>% 
  mutate(nameLabel = replace(nameLabel, nameLabel == "Jonathan", "   Jonathan"))

jahr <- dat$Jahr
#name <- dat$Namen
name <- dat$nameLabel

df <- data.frame(jahr, name)

df$Y <- as.Date(paste0("01", "01", df$jahr), format="%d%m%Y")
rangeY <- range(df$Y)

# myCol <- "#5B7FA3"
myCol <- "#993404"

# jpeg("timeline.jpeg", width = 10, height = 6, units = 'in', res = 2000)
jpeg("timeline2.jpeg", width = 270, height = 205, units = 'mm', res = 1000)
plot(NA,ylim=c(-1,1),xlim=rangeY,ann=FALSE,axes=FALSE)
abline(h=0,lwd=2,col=myCol)

ypts <- rep(c(0.5,0.75,1,-0.5,-0.75,-1), length.out=nrow(df))
# ypts <- rep(c(1,0.75,0.5,-0.5,-0.75,-1), length.out=nrow(df))
# ypts <- rep(c(-1,-0.5,0.5,0.75,1), length.out=nrow(df))
# ypts <- rep(c(-1,-0.75,-0.5,0.5,0.75,1), length.out=nrow(df))
# ypts <- rep(c(-1,-0.75,-0.5,0.5,0.75,1), length.out=nrow(df))

txtpts <- rep(c(3,3,3,1,1,1), length.out=nrow(df)) ## text positionierung um das symbol
segments(df$Y,0,df$Y,ypts,col="gray80")

## split axis labels
s1 <- seq(1, ceiling(length(unique(df$Y))), by =2)
s2 <- seq(2, floor(length(unique(df$Y))), by =2)

axis.Date(1, at=unique(df$Y)[s1], las=2, format="%Y", cex.axis=0.6, pos=0,
          lwd=0, lwd.tick=NA, col=myCol, font=2)
axis.Date(3, at=unique(df$Y)[s2], las=2, format="%Y", cex.axis=0.6, pos=0,
          lwd=0, lwd.tick=NA, col=myCol, font=2)

points(df$Y,y=ypts, pch="-", cex=1.5, col=myCol)
# points(df$Y,y=ypts, pch=20, cex=1, col=myCol)
par(xpd=NA)
text(df$Y, y=ypts, labels=df$name, cex=0.6, pos=txtpts, font=2)
par(xpd=FALSE)

dev.off()
