library(googlesheets4)
library(scales)
library(zoo)
library(tidyverse)

# get the data from google sheet ----
CUcovid <- read_sheet("1UUGDwV5qahPos-bhWUfzf4Y1WYXEh-I0JBOJaoGMrJs",
                      sheet = 1) %>%
  mutate(avgnewcases = rollmean(New_cases, k = 7, 
                                fill = NA, align = "right")) %>%
  mutate(avgnewtests = rollmean(New_tests, k = 7, 
                                fill = NA, align = "right")) %>%
  mutate(avgnewhospitalized = rollmean(Hospitalized, k = 7, 
                                       fill = NA, align = "right")) %>%
  mutate(avgnewdead = rollmean(New_Deceased, k = 7, 
                               fill = NA, align = "right")) %>%
  mutate(avgmonthlynewdead = rollmean(New_Deceased, k = 31, 
                                      fill = NA, align = "right")) %>%
  mutate(weeklydead = avgnewdead * 7) %>%
  mutate(biweeklydead = rollmean(New_Deceased, k = 14, 
                                 fill = NA, align = "right")*14) %>%
  mutate(monthlydead = rollmean(New_Deceased, k = 31, 
                                fill = NA, align = "right")*31) 
  
write.csv(CUcovid,"data/CUcovid.csv", row.names = FALSE)
CUcovidactive <- CUcovid %>%
  select(Date, Hospitalized, Not_hospitalized) %>%
  pivot_longer(!Date, names_to = "Active", values_to = "count")

# new cases with 7 day avg ----
ggplot(CUcovid, aes(x = as.Date(Date), y = New_cases)) +
  geom_col(fill = "#B45F06",
           alpha = .25) +
  geom_line(aes(y = avgnewcases),
            colour = "#B45F06",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     limits = c(0,max(CUcovid$avgnewcases,
                                      na.rm = TRUE)),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  #guides(fill = FALSE) +
  labs(title = "New Cases in Champaign County",
       subtitle =   "With seven-day moving average",
       caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("CU/CUCases.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUCasesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# new tests with 7 day avg ----
ggplot(CUcovid, aes(x = as.Date(Date), y = New_tests)) +
  geom_col(fill = "#1C4587",
           alpha = .25) +
  geom_line(aes(y = avgnewtests),
            colour = "#1C4587",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     limits = c(0,max(CUcovid$avgnewtests,
                                      na.rm = TRUE)),
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "New Tests in Champaign County",
       subtitle =  "With seven-day moving average",
       caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("CU/CUTests.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUTestsWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# stacked geom_area of active cases ----
ggplot(CUcovidactive, aes(x = as.Date(Date), y = count, fill = Active)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_fill_manual(values = c("#ff5f1a","#ffb648"),
                    labels = c("Hospitalized", "Not Hospitalized")) +
  guides(fill = guide_legend(reverse = TRUE,
                             title = NULL)) +
  labs(title = "Active Cases in Champaign County",
         caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        legend.position = c(.25,.62),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13)) 
ggsave("CU/CUactive.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUactiveWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# hospitalizations ----
ggplot(filter(CUcovid, Hospitalized != is.null(TRUE)), 
              aes(x = as.Date(Date), y = Hospitalized)) +
  geom_col(fill = "#ff5f1a",
           alpha = .25) +
  geom_line(aes(y = avgnewhospitalized),
            colour = "#ff5f1a",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "Currently Hospitalized in Champaign County",
       subtitle =  "With seven-day moving average",
       caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("CU/CUhospitalized.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUhospitalizedWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# quarantined ----
ggplot(filter(CUcovid, Quarantined != is.null(TRUE)), 
       aes(x = as.Date(Date), y = Quarantined)) +
  # geom_col(fill = "#009e89",
  #          alpha = .25) +
  # geom_line(aes(y = Quarantined),
  #           colour = "#009e89",
  #           size = 1) +
  geom_area(fill = "#009e89",
            alpha = .25,
            colour = "#009e89",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Currently Quarantined in Champaign County",
       caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("CU/CUquarantined.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUquarantinedWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# deaths ----
ggplot(CUcovid, 
       aes(x = as.Date(Date), y = New_Deceased)) +
  geom_col(fill = "#d90000",
           alpha = .25) +
  geom_line(aes(y = avgmonthlynewdead),
            colour = "#d90000",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "New Deaths in Champaign County",
          subtitle = "With monthly moving average",
       caption = "Source: Champaign-Urbana Public Health District")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("CU/CUdeaths.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CUdeaths.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# todo ----
# - [x] add facet grid or wrap for the charts, use pivot longer to put in one table to align dates