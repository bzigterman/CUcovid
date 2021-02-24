library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(clipr)

# get data from google sheet and save as csv ----
dville <- read_sheet("1K4RSNbuBPkOFgr18klpNL0vX-UUov7Ct1fRobOGZw6o",
                         sheet = 1) %>%
  select(date, Staff_current, Inmates_current) %>%
  subset(date > "2020-09-30") %>%
  pivot_longer(!date, names_to = "who", values_to = "active")
write.csv(dville,"data/dville.csv", row.names = FALSE)

# chart of Danville prison cases ----
ggplot(dville, aes(x = as.Date(date),
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
  scale_fill_brewer(palette = "Dark2",
                    labels = c("Inmates","Staff")) +
  ggtitle("Active Cases at the Danville Prison",
          "Source: Illinois Department of Corrections") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.1,.5),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13))
# save file
ggsave("danville/dvilleprison.png", width = 8, height = 32/7, dpi = 320)
