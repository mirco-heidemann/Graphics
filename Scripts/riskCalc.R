## Risiko als jaehrliche Schadenerwartung rechnen
## und darstellen

library(RColorBrewer)

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## risikofunktion
CalcRisk <- function(rtp, schad_szenarien){

  risk_1 <- schad_szenario_1 / 2 * (1 / (rtp_1 / 2) - 1 / rtp_1)
  risk_2 <- risk_1 + ((schad_szenario_2 + schad_szenario_1) / 2) * (1 / rtp_1 - 1 / rtp_2)
  risk_3 <- risk_2 + (schad_szenario_3 + schad_szenario_2) / 2 * (1 / rtp_2 - 1 / rtp_3)
  risk_4 <- risk_3 + (schad_szenario_4 + schad_szenario_3) / 2 * (1 / rtp_3 - 1 / rtp_4)

  df_temp <- data.frame(risk_1, risk_2, risk_3, risk_4)
  return(df_temp)
}

# CalcRisk <- function(rtp, schad_szenarien){
#   
#   risk0 <- schad_szenario0/2*(1/(rtp0/2)-1/rtp0)
#   risk_1 <- risk0+((schad_szenario_1+schad_szenario0)/2)*(1/rtp0-1/rtp_1)
#   risk_2 <- risk_1+((schad_szenario_2+schad_szenario_1)/2)*(1/rtp_1-1/rtp_2)
#   risk_3 <- risk_2+(schad_szenario_3+schad_szenario_2)/2*(1/rtp_2-1/rtp_3)
#   risk_4 <- risk_3+(schad_szenario_4+schad_szenario_3)/2*(1/rtp_3-1/rtp_4)
#   
#   df_temp <- data.frame(risk0, risk_1, risk_2, risk_3, risk_4)
#   return(df_temp)
# }

## risiko rechnen ----
## wiederkehrperioden definieren
rtp_1 <- 10
rtp_2 <- 50
rtp_3 <- 300
rtp_4 <- 1000
rtp <- cbind(rtp_1, rtp_2, rtp_3, rtp_4)

## Schadenszenarien in Mio. CHF
schad_szenario_1 <- 35
schad_szenario_2 <- 74
schad_szenario_3 <- 340
schad_szenario_4 <- 550
schad_szenario <- cbind(schad_szenario_1, schad_szenario_2, schad_szenario_3, schad_szenario_4)

## risiko (jaehrliche Schadenerwartung) in Mio. CHF
(risk <- CalcRisk(schad_szenario, rtp))


## Schaden-Ereignis und Risiko Plots ----
schadTitle <- "Hagelszenarien"
riskTitle <- "Hagelrisiko"
par(mfrow=c(1,2), mar=c(3.5,5,2.5,2.5))

## Ereignisschaden
x <- c(0,rtp)
y <- c(0,schad_szenario)

plot(x, y, type="b", axes = F, xlab= NA, ylab = NA, col='black',lwd=2)
segments(x0=0, y0=y, x1=x,
         y1=y, col="gray80", lty=3)

## find and save the corners of the plotting region
loc <- par("usr")

box()
axis(side=1, at=x, labels=formatC(x, format="d"), 
     las = 1, tck = -.015, cex.axis=0.6)
axis(side=2, at=y[-1], labels=formatC(y[-1], big.mark="'", format="d"), 
     las = 2,tck = -.015, cex.axis=0.6)
mtext(side = 1, 'Häufigkeit [Jahre]', line = 2, cex=0.6)
text(loc[1], loc[4], 'Schaden [Mio. CHF]', pos = 3, xpd = T, cex=0.6)
mtext(side = 3, schadTitle, line = 1, font = 2, cex=0.6)

## Risiko-Barplot
myCol <- brewer.pal(n=length(risk)+1, "YlOrRd")[-1]

bPlot <- barplot(height = as.numeric(risk),
        names.arg = rtp, las = 1, cex.names = 0.6,
        beside = true, axes = F, xlab= NA,
        ylab = NA, border=NA, col=myCol)

mtext(side = 1, 'Schadenszenarien', line = 2, cex=0.6)
mtext(side = 3, riskTitle, line = 1, font = 2, cex=0.6)

## find and save the corners of the plotting region
loc <- par("usr")

text(loc[1], loc[4], 'Jaehrliche Schaden-\nerwartung [Mio. CHF]', pos = 3, xpd = T, cex=0.6)

axis(side=2, at=c(0, as.numeric(risk)),
     labels=formatC(c(0, as.numeric(risk)), format = "f", digits = 1, big.mark="'"),
     las = 2,tck = -.015, cex.axis=0.6)

#box()
segments(x0=0, y0=as.numeric(risk), x1=bPlot,
         y1=as.numeric(risk), col=myCol, lty=3)

par(mfrow=c(1,1))

