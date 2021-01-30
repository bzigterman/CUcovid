library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)

# get the data from google sheet
regioncovid <- read_sheet("1UUGDwV5qahPos-bhWUfzf4Y1WYXEh-I0JBOJaoGMrJs",
                      sheet = 3)

# chart of Champaign Co positives without the UI 
ggplot(regioncovid, aes(x = as.Date(Date), y = ChampaignCoNoUIpositives)) +
  geom_col(fill = "#B45F06",
           alpha = .25) +
  geom_line(aes(y = rollmean(ChampaignCoNoUIpositives, 
                             7, 
                             fill = TRUE, 
                             align = "right")),
            colour = "#B45F06",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("Champaign County Cases without University of Illinois Tests",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/1ChampaignCountyNoUICases.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("ChampaignCountyNoUICasesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

# chart of champaign county tests without the UI
ggplot(regioncovid, aes(x = as.Date(Date), y = ChampaignCoNoUItests)) +
  geom_col(fill = "#1C4587",
           alpha = .25) +
  geom_line(aes(y = rollmean(ChampaignCoNoUItests, 
                             7, 
                             fill = TRUE, 
                             align = "right")),
            colour = "#1C4587",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("Champaign County Tests without University of Illinois Tests",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/CountyTestsnoUI.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("CountyTestsnoUIWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

# chart of champaign county positivity without the UI
ggplot(regioncovid, aes(x = as.Date(Date), y = ChampaignCoNoUIpositivity)) +
  geom_point(colour = "#1C4587",
             alpha = .25) +
  geom_line(aes(y = ChampaignCoNoUIavgPositivity),
            colour = "#1C4587",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(.01,.01))) +
  guides(fill = FALSE) +
  ggtitle("Champaign County Positivity without University of Illinois Tests",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/ChampaignConoUIPositivity.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("ChampaignConoUIPositivityWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

# chart of Positive tests in champaign county
ggplot(regioncovid, aes(x = as.Date(Date))) +
  geom_point(aes(y = UIpositives),
             colour = "#55c667ff",
             alpha = .25) +
  geom_point(aes(y = ChampaignCoNoUIpositives),
             colour = "#481567ff",
             alpha = .25) +
  geom_line(aes(y = rollmean(UIpositives, 
                             7, 
                             fill = TRUE, 
                             align = "right"),
                colour = "University of Illinois"),
            #colour = "#38761D",
            size = 1.5) +
  geom_line(aes(y = rollmean(ChampaignCoNoUIpositives, 
                             7, 
                             fill = TRUE, 
                             align = "right"),
                colour = "Non-University of Illinois"),
            #colour = "#741B47",
            size = 1.5) +
  scale_colour_manual(labels = c("Non-University of Illinois",
                                 "University of Illinois"),
                      values = c("Non-University of Illinois" = "#481567ff",
                                 "University of Illinois" = "#55c667ff"),
                      guide = guide_legend(title = NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     limits = c(0,max(rollmean(regioncovid$UIpositives,
                                               7,
                                               fill = TRUE,
                                               align = "right"))),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(.01,.01))) +
  ggtitle("Champaign County Cases",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.16,.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 
ggsave("region/2ChampaignCoCasesSplit.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("ChampaignCoCasesSplitWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

# chart of seven day positivity without UI tests
ggplot(regioncovid, aes(x = as.Date(Date))) +
  geom_point(aes(y = Region6noUIpositivity),
             colour = "black",
             alpha = .25) +
  geom_point(aes(y = ChampaignCoNoUIpositivity),
             colour = "#B45F06",
             alpha = .25) +
  geom_line(aes(y = Region6noUIavgPositivity,
                colour = "Region 6"),
            size = 1.5) +
  geom_line(aes(y = ChampaignCoNoUIavgPositivity,
                colour = "Champaign County"),
            size = 1.5) +
  scale_colour_manual(labels = c("Champaign County",
                                 "Region 6"),
                      values = c("Region 6" = "black",
                                 "Champaign County" = "#B45F06"),
                      guide = guide_legend(title = NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(.01,.01))) +
  ggtitle("Positivity without University of Illinois tests",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.16,.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 
ggsave("region/3NoUIpositivity.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("NoUIpositivityWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

# chart of state positivity
ggplot(regioncovid, aes(x = as.Date(Date))) +
  geom_line(aes(y = StatePositivityWithUItests,
                colour = "With University of Illinois tests"),
            size = 1.5) +
  geom_line(aes(y = StatePositivityWithNoUI,
                colour = "Without University of Illinois tests"),
            size = 1.5) +
  scale_colour_manual(
    labels = c("With University of Illinois tests",
               "Without University of Illinois tests"),
    values = c("With University of Illinois tests" = "#FF552E",
               "Without University of Illinois tests" = "#13294B"),
    guide = guide_legend(title = NULL)
    ) +
  xlab(NULL) +
  ylab(NULL) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(.01,.01))) +
  ggtitle("Illinois Test Positivity",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.2,.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 
ggsave("region/4statepositivity.png", 
       width = 8, height = 4.57142857, dpi = 320)
ggsave("statepositivityWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.57142857, dpi = 320)

