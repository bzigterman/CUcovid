library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)

dville <- read_sheet("1K4RSNbuBPkOFgr18klpNL0vX-UUov7Ct1fRobOGZw6o",
                     sheet = 2)
dville_after_Oct <- subset(dville, date > "2020-09-30")
  

ggplot(dville_after_Oct, aes(x = as.Date(date),
                   y = active,
                   fill = who)) +
  geom_area() +
  xlab(NULL) +
  ylab(NULL) +
  guides(fill = guide_legend(title = NULL)) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Active Cases at the Danville Prison",
          "Source: Illinois Department of Corrections") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.1,.5),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("danville/dvilleprison.png", width = 8, height = 4.57142857, dpi = 320)
