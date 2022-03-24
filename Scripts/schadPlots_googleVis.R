## ----
## googleVis Grafik:
## Schadenplots
##
## Mirco Heidemann, Oktober 2017
## ----

## Arbeitsverzeichnis definieren
setwd('I:/R/Grafik mit googleVis/data/')

## funtionen laden
require(stringr)
require(googleVis)
source('f.schadPrep.R')

## schadenfile einlesen
schadFile <- 'schaeden.201701.georef.csv'
indexFile <- 'versicherungsindex.gvz.csv'

schad <- schadPrep(schadFile, indexFile)

## schadendaten ab 1982
ind <- which(as.numeric(format(schad$schadat,'%Y')) > 1981)
if(length(ind)>0) schad <- schad[ind,]

## schadendaten bis und mit 2016
ind <- which(as.numeric(format(schad$schadat,'%Y')) > 2016)
if(length(ind)>0) schad <- schad[-ind,]

## yearly data
aggr.anz <- aggregate(schad$indexSchad,
                      by=list(as.numeric(format(schad$schadat,'%Y'))),
                      'length')
aggr.sum <- aggregate(schad$indexSchad,
                      by=list(as.numeric(format(schad$schadat,'%Y'))),
                      'sum')
yearly <- as.data.frame(cbind(aggr.sum, aggr.anz$x))
names(yearly) <- c('jahr', 'schadSum', 'schadAnz')
yearly$jahr <- format(as.Date(as.character(yearly$jahr), '%Y'), '%Y')
## ----

## Grafiken mit googleVis

## Line chart
Line <- gvisLineChart(yearly[,-3])
plot(Line)

## Line chart with two axis
Line2 <- gvisLineChart(yearly, 'jahr', c("schadSum","schadAnz"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                         {targetAxisIndex:1}]",
                         vAxes="[{title:'schadSum'}, {title:'schadAnz'}]"
                       ))
plot(Line2)

## Customizing Lines
Dashed <-  gvisLineChart(yearly, 'jahr', c("schadSum","schadAnz"),
                         options=list(
                           series="[{color:'green', targetAxisIndex: 0, 
                           lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                           {color: 'blue',targetAxisIndex: 1, 
                           lineWidth: 2, lineDashStyle: [4, 1]}]",
                           vAxes="[{title:'val1'}, {title:'val2'}]"
                         ))
plot(Dashed)

## Bar chart
Bar <- gvisBarChart(yearly[,-2])
plot(Bar)

## Column chart
Column <- gvisColumnChart(yearly[,-3])
plot(Column)

## Bubble chart
ind <- which(format(schad$schadat, '%m.%Y') == '12.1999')
datDez <- schad[ind,]

ind <- which(format(schad$schadat, '%m.%Y') == '05.1999')
datMai <- schad[ind,]

Bubble <- gvisBubbleChart(datMai, idvar="", 
                          xvar="baujahr", yvar="vs",
                          colorvar="artcode", sizevar="indexSchad")
                          # options=list(
                          #   hAxis='{minValue:1000, maxValue:2016}'))
plot(Bubble)


# ## Motion chart example
# datDez <- datDez[,-c(1, 5, 6)]
# plot(gvisMotionChart(datDez, "artcode", "schadat", options = list(width = 600, height = 400)))
# plot(gvisMotionChart(Fruits, "Fruit", "Year", options = list(width = 600, height = 400)))
