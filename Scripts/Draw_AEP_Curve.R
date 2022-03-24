# Exceedance Probability: The probability of loss from an event exceeding a certain level.
# Usually refers to either the occurrence exceedance probability (OEP) or the aggregate
# exceedance probability, and represent a standard output of cat models. 
# 
# An occurrence probability curve (OEP) is a loss distribution that shows the likelihood
# of exceeding a specified loss threshold  from a single occurrence in any given year.
# The OEP can be calculated from an Event Loss Table (ELT).
# 
# An aggregate exceedance probability curve (AEP) is a loss distribution that shows
# the likelihood of exceeding  a specified loss threshold from one or more events in
# any given year. The AEP can be calculated by convolving the distributions from the
# individual Event Loss Table (ELT) entries (e.g. through Monte Carlo Simulation,
# Panjer or FFT). 
# 
# Secondary uncertainty refers to the distribution of potential loss amounts for a
# given event, while primary uncertainty measures the uncertainty in the likelihood
# of a particular event occurring. EP curves  typically cover primary uncertainty
# but not secondary uncertainty. Inclusion of secondary uncertainty produces smoother
# EP curves with longer tails.
# 
# An annual aggregate loss distribution  is the distribution of the annual aggregate
# losses , covering all relevant uncertainties , in particular secondary uncertainty



library(dplyr)
library(scales)
library(ggplot2)

df <- tibble::tribble(
  ~AEP,          ~P,
  0.001, 299.0973209,
  0.01, 254.7226534,
  0.03, 233.0298722,
  0.05, 223.9571177,
  0.1, 211.2898816,
  0.3, 190.5075232,
  0.5, 182.3294549,
  1, 170.5569051,
  3, 148.9113334,
  5,  138.991102,
  10, 125.4449161,
  20, 110.1408306,
  25,   104.74124,
  30, 100.2363357,
  40, 92.15268627,
  50, 85.75477796,
  60, 79.55311702,
  70, 73.44249835,
  75, 70.21061223,
  80, 66.79821521,
  90, 58.54507042,
  95, 52.44861458,
  97, 48.86357489,
  99, 43.12184627,
  99.5, 39.72675936,
  99.7,  37.5826596,
  99.9, 33.91759317
)

df %>% 
  ggplot(aes(x = qnorm(AEP/100), # transform to quantiles
             y = P/5)) + 
  geom_point(colour="firebrick", size = 2) +
  geom_line(colour="firebrick", size = 1) +
  scale_y_continuous(name = "Jahresschaden [Mio CHF]",
                     labels = scales::comma,
                     breaks = seq(0, 300/5, 50/5)) +
  scale_x_continuous(name = "Überschreitungswahrscheinlichkeit [%]",
                     # breaks = qnorm(df$AEP/100), #transform
                     breaks = qnorm(df$AEP/100)[seq(1,length(qnorm(df$AEP/100)),by=2)], #transform and show only every 2nd
                     # labels = df$AEP,
                     labels = df$AEP[seq(1,length(df$AEP),by=2)],
                     expand = c(0.035,0.035)) +

  labs(title = "Aggregate exceedance probability curve (AEP) - Hagelrisiko") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(colour="grey", size = (0.2)),
        panel.grid.minor = element_line(size = (0.2), colour="grey"))
