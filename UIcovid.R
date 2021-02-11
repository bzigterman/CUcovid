library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)

# get the data from google sheet
uicovid <- read_sheet("1UUGDwV5qahPos-bhWUfzf4Y1WYXEh-I0JBOJaoGMrJs",
                      sheet = 2)
write.csv(uicovid,"data/uicovid.csv", row.names = FALSE)

# new cases with 7 day avg
ggplot(uicovid, aes(x = as.Date(Date), y = New_Cases)) +
  geom_col(fill = "#B45F06",
           alpha = .25) +
  geom_line(aes(y = rollmean(New_Cases, 7, fill = TRUE, align = "right")),
            colour = "#B45F06",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("New Cases at the University of Illinois",
          "With seven-day moving average")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("UI/2UICases.png", width = 8, height = 32/7, dpi = 320)
ggsave("UICasesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)
# ggsave("1TweetUICases.png", width = 8, height = 32/7, dpi = 150)

# new tests with 7 day avg
ggplot(uicovid, aes(x = as.Date(Date), y = New_Tests)) +
  geom_col(fill = "#1C4587",
           alpha = .25) +
  geom_line(aes(y = rollmean(New_Tests, 7, fill = TRUE, align = "right")),
            colour = "#1C4587",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("New Tests at the University of Illinois",
          "With seven-day moving average")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("UI/3UITests.png", width = 8, height = 32/7, dpi = 320)
ggsave("UITestsWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)
# ggsave("2TweetUITests.png", width = 8, height = 32/7, dpi = 150)

# positivity with 7 day avg
ggplot(uicovid, aes(x = as.Date(Date), y = positivity)) +
  geom_point(colour = "#1C4587",
             alpha = .25) +
  geom_line(aes(y = Week_avg_pos),
            colour = "#1C4587",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     # limits = c(0,max(uicovid$Week_avg_pos)),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(.01,.01))) +
  guides(fill = FALSE) +
  ggtitle("Test Positivity at the University of Illinois",
          "With seven-day moving average")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("UI/4UIPositivity.png", width = 8, height = 32/7, dpi = 320)
ggsave("UIPositivityWeb.png", # save to my website
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)
# ggsave("3TweetUIPositivity.png", width = 8, height = 32/7, dpi = 150)

# # bar chart/table of this year vs last year
# uicovidbyyear <- read_sheet("1UUGDwV5qahPos-bhWUfzf4Y1WYXEh-I0JBOJaoGMrJs",
#                       sheet = 4)
# ggplot(uicovidbyyear, aes(y = factor(Year),
#                           x = Location)) +
#   geom_text(aes(label = comma(Cases)),
#             size = 20,
#             family = "Barlow") +
#   scale_y_discrete(limits = c("This year:","Last year:")) +
#   scale_x_continuous(breaks = NULL) +
#   xlab(NULL) +
#   ylab(NULL) + 
#   ggtitle("Total Cases at the University of Illinois") +
#   theme_void() +
#   theme(
#     text = element_text(family = "Barlow",
#                         size = 20),
#     axis.text.y = element_text(size = 30),
#     plot.title = element_text(size = 25, family = "Oswald")
#   )
# ggsave("UITotals.png", width = 8, height = 32/7, dpi = 320)
# ggsave("4TweetUITotals.png", width = 8, height = 32/7, dpi = 150)

# fall vs spring semester new cases
ggplot(uicovid, aes(x = Semester_day/7, y = New_Cases, colour = Semester)) +
  #geom_vline(xintercept = 0, colour = "grey50") +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "white") +
  geom_point(alpha = .25) +
  geom_line(aes(y = rollmean(New_Cases, 7, fill = TRUE, align = "right")),
            size = 1.5) +
  xlab("Weeks into Semester") +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     limits = c(0,max(rollmean(uicovid$New_Cases,
                                               7,
                                               fill = TRUE,
                                               align = "right"))),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_continuous(breaks = c(-2,0,4,8,12,16),
                     expand = expansion(mult = c(.01,.01))) +
  #scale_colour_manual(breaks = c("Fall","Spring"),
  #                    values = c("#d16c1f","#26ab5c")) +
  scale_colour_brewer(breaks = c("Fall","Spring"),
                      palette = "Set1",
                      guide = guide_legend(title = NULL)) +
  ggtitle("Fall vs. Spring Semester at the University of Illinois",
          "New cases with seven-day moving average")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.058,.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13)) 

ggsave("UI/1UISemCompare.png", width = 8, height = 32/7, dpi = 320)
ggsave("UISemCompare.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)

# fall vs spring semester total cases
ggplot(uicovid, aes(x = Semester_day/7, y = Sem_totals, colour = Semester)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "white") +
  geom_line(size = 1.5) +
#  geom_line(aes(y = rollmean(Total_cases, 7, fill = TRUE, align = "right")),
 #           size = 1.5) +
  xlab("Weeks into Semester") +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     # limits = c(0,max(rollmean(uicovid$Sem_totals,
                     #                           7,
                     #                           fill = TRUE,
                     #                           align = "right"))),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_continuous(breaks = c(-2,0,4,8,12,16),
                     expand = expansion(mult = c(.01,.01))) +
  #scale_colour_manual(breaks = c("Fall","Spring"),
  #                    values = c("#d16c1f","#26ab5c")) +
  scale_colour_brewer(breaks = c("Fall","Spring"),
                      palette = "Set1",
                      guide = guide_legend(title = NULL)) +
  ggtitle("Fall vs. Spring Semester at the University of Illinois",
          "Total cases from two weeks before classes start")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald"),
        legend.position = c(.058,.85),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13)) 

# ggsave("UI/1UISemCompare.png", width = 8, height = 32/7, dpi = 320)
# ggsave("UISemCompare.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 32/7, dpi = 320)


# todo
# save total cases comparison, change numbering on charts