
# GVZ Intranet Umfrage zur Nutzung von E-Bikes.
# Folgendes wurde gefragt:
# - Generelle Benutzung?
# - Wenn ja, wie viele Fahrten pro Jahr?
# - Wenn ja, würde die Nutzung die Autofahrten reduzieren?
#
# Drei Graphiken aus der Umfrage erstellen:
# 1. Barplot Generelle Benutzung ja/nein mit Anzahl und in Prozent
# 2. Wenn ja, Histogramm der Fahrten pro Jahr
# 3. Wenn ja, Barplot der Reduktion von Autofahrten ja/nein mit Anzahl und in Prozent

library(tidyverse)
library(RColorBrewer)
library(grid)
library(ggwordcloud)

# Load data
# ---------
dat <- read_delim("data/umfrage_ebikes.csv",
                  delim = ";", col_names = TRUE,
                  locale = locale(encoding = 'ISO-8859-1'),
                  col_types = cols())

tbl_nutzen <- dat %>% 
  count(nutzen) %>% 
  mutate(perc = round(100 * n / nrow(dat),0))

tbl_autoredu <- dat %>% 
  filter(nutzen == "JA") %>% 
  count(auto_reduziert) %>% 
  mutate(perc = round(100 * n / nrow(dat[dat$nutzen == "JA", ]),0))


# Graphics mit ggplot
# ---------
theme_set(theme_bw(base_size = 12))

# 1. Barplot Generelle Benutzung ja/nein mit Anzahl und in Prozent
ggplot(tbl_nutzen, aes(x = nutzen, y = n, fill = nutzen)) +
  geom_bar(stat = "identity", width=0.7, colour = c("#7C8388", "#E53527"), alpha = 0.6) +
  geom_text(aes(label = paste0(n, " (", perc, "%)")),
            position = position_dodge(0.9),
            vjust = -0.75,
            fontface = "bold") +
  geom_hline(yintercept = 0, colour = "#7C8388") +
  scale_fill_manual(values = c("#7C8388", "#E53527")) +
  labs(title = "GVZ E-Bike zu geschäftlichen Terminen?", 
       subtitle = paste(nrow(dat), "Antworten")
       #caption = "Quelle: Klimaleitbild GVZ"
       ) + 
  # remove space between axis and plot
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    # remove alle gridlines
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    # remove border lines 
    panel.border = element_blank(),
    # "none" for no legend
    legend.position = "none",
    # remove x and y title
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    # customize x-axis text
    axis.text.x = element_text(face = "bold", color = "black", 
                               size = 12, angle = 0),
    # remove y-axis ticks and tick mark labels
    axis.ticks = element_blank(), axis.text.y = element_blank(),
    aspect.ratio = 4/3,
    # Removing/Reducing the whitespace surrounding the plot
    # (requires the grid library)
    plot.margin = unit(c(1,1,1,1),"mm"))

# 2. Wenn ja, Histogramm der Fahrten pro Jahr
dat_fahrten <- dat[!is.na(dat$fahrten_jahr),]

ggplot(dat_fahrten, aes(dat_fahrten$fahrten_jahr)) +
  geom_histogram(breaks=seq(0, 51, by=10),
                 col="#7C8388",
                 #col="#E53527",
                 # fill="white",
                 fill="#E53527",
                 alpha=.6,
                 binwidth=0.1) +
  xlab("\nAnzahl Fahrten pro Jahr") + ylab("Häufigkeit der Antwort\n") +
  labs(title = "Wie oft würdest du das E-Bike pro Jahr verwenden?",
       subtitle = paste(nrow(dat_fahrten), "Antworten der potenziellen E-Bike Nutzer")
       #caption = "Quelle: Klimaleitbild GVZ"
       ) +
  # remove space between axis and plot
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +

  theme(
    # remove alle gridlines
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    # remove border lines
    panel.border = element_blank(),
    # "none" for no legend
    legend.position = "none",
    # customize axis text
    axis.text.x = element_text(color = "black",
                               size = 12, angle = 0),
    axis.text.y = element_text(color = "black",
                               size = 12, angle = 0),
    # remove y-axis ticks and tick mark labels
    axis.ticks = element_blank(),
    aspect.ratio = 9/16,
    # Removing/Reducing the whitespace surrounding the plot
    # (requires the grid library)
    plot.margin = unit(c(1,1,1,1),"mm"))

# 3. Wenn ja, Barplot der Reduktion von Autofahrten
ggplot(tbl_autoredu, aes(x = auto_reduziert, y = n, fill = auto_reduziert)) +
  geom_bar(stat = "identity", width=0.7, colour = c("#7C8388", "#E53527"), alpha = 0.6) +
  geom_text(aes(label = paste0(n, " (", perc, "%)")),
            position = position_dodge(0.9),
            vjust = -0.75,
            fontface = "bold") +
  # ad horizontal line at bottom
  geom_hline(yintercept = 0, colour = "#7C8388") +
  scale_fill_manual(values = c("#7C8388", "#E53527")) +
  labs(title = "Reduktion von Autofahrten durch E-Bikes?", 
       subtitle = paste(nrow(dat[dat$nutzen == "JA", ]), "Antworten der potenzieller E-Bike Nutzer") 
       #caption = "Quelle: Klimaleitbild GVZ"
       ) + 
  # remove space between axis and plot
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme(
    # remove alle gridlines
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    # remove border lines 
    panel.border = element_blank(),
    # "none" for no legend
    legend.position = "none",
    # remove x and y title
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    # customize x-axis text
    axis.text.x = element_text(face = "bold", color = "black", 
                               size = 12, angle = 0),
    # remove y-axis ticks and tick mark labels
    axis.ticks = element_blank(), axis.text.y = element_blank(),
    aspect.ratio = 4/3,
    # Removing/Reducing the whitespace surrounding the plot
    # (requires the grid library)
    plot.margin = unit(c(1,1,1,1),"mm"))

# 4. Wordcloud vom Bemerkungsfeld
words <- read_delim("data/umfrage_ebikes_wordcloud.csv",
                  delim = ";", col_names = TRUE,
                  locale = locale(encoding = 'ISO-8859-1'),
                  col_types = cols()) %>% 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(100, 0)))

ggplot(words, aes(label = word, size = frequency, angle = angle, color = frequency)) +
  geom_text_wordcloud(eccentricity = 0.7) +
  scale_radius(range = c(1, 24), limits = c(0, NA)) +
  theme_minimal() +
  scale_color_gradient(low = "#E53527", high = "darkred")
