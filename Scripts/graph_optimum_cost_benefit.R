library(tidyverse)

x <- seq(0, 10, length.out = 1000)
# Schaden Kurve
dat <- data.frame(x = x, exp = dexp(x, rate=0.7))
# Schutzkosten Kurve
dat <- dat %>% 
  mutate(quad = ((x + 5)/10)^3,
         tot = exp + quad)

# Grafik-Thema definieren
my_theme <- theme_minimal()+
  theme(panel.border = element_blank(),
        #panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(b=0), size = 16),
        plot.subtitle=element_text(margin=margin(t=5, b = 10),size = 12), 
        legend.position = c(0.93, 0.05),
        axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.line = element_line(arrow = arrow(length = unit(0.3, "cm")), size = 1))

# Plot
costbe <- ggplot(dat) +
  geom_line(aes(x = x, y = exp), linetype = "dashed", colour = "#d8b365", size = 1.05) +
  geom_line(aes(x = x, y = quad), linetype = "dotdash", colour = "#5ab4ac", size = 1.05) +
  geom_line(aes(x = x, y = tot), linetype = "solid", colour = "#a6611a", size = 1.05) +
  labs(x = "Präventionsaufwand", y = "Kosten") + 
  # geom_segment(aes(x = x[which(tot == min(tot))] + 0.5, y = 0,
  #                  xend = x[which(tot == min(tot))] + 0.5, yend = min(tot) + 0.3),
  #              linetype = "dashed") +
  # geom_segment(aes(x = x[which(tot == min(tot))] - 0.5, y = 0,
  #                  xend = x[which(tot == min(tot))] - 0.5, yend = min(tot) + 0.3),
  #              linetype = "dashed") +
  annotate("rect", xmin = dat$x[which(dat$tot == min(dat$tot))] - 0.6,
           xmax = dat$x[which(dat$tot == min(dat$tot))] + 0.6,
           ymin = 0, ymax = 1, alpha = 0.1) +
  
  annotate("point", x = dat$x[which(dat$tot == min(dat$tot))], y = min(dat$tot),
           size = 4, colour = "#525252") +
  
  # geom_segment(aes(x = x[which(tot == min(tot))] - 0.6, y = min(tot) + 0.1,
  #                  xend = x[which(tot == min(tot))] + 0.6, yend = min(tot) + 0.1),
  #              arrow = arrow(length = unit(0.2, "cm"), ends = "both", type = "closed"),
  #              linetype = "dashed") +
  
  annotate("text", x = c(0.6, 4, 4, dat$x[which(dat$tot == min(dat$tot))]),
           y = c(0.26, 0.1, 0.89, min(dat$tot) + 0.07), size = 6,
           colour = c("#5ab4ac", "#d8b365", "#a6611a", "#525252"),
           label = c("Präventionskosten", "Schadenskosten", "Totale Kosten", "Optimum")) +
  
  
  scale_x_continuous(limits = c(0, 5), expand = c(0.005, 0.005)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  # ggtitle("Effiziente Präventionsmassnahmen im Gebäudeschutz vor Naturgefahren") +
  my_theme
costbe

# # Save the plot
# width_plot = 12
# height_plot = (9/16) * width_plot
# 
# ggsave(costbe, filename = "costbe.png", dpi = 1000, type = "cairo",
#        width = width_plot, height = height_plot, units = "in")
