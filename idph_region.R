library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(scales)
library(zoo)
library(clipr)

idph_region6 <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=6&daysIncluded=0",
                            format = "csv") %>%
  filter(RegionID == 6) %>%
  mutate(Date = mdy_hms(ReportDate)) %>%
  mutate(avgnewcases = rollmean(PositiveTests, k = 7, 
                                       fill = NA, align = "right"))
write.csv(idph_region6,"idph/region6.csv", row.names = FALSE)

idph_region6noUI <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=6&daysIncluded=0",
                                format = "csv") %>%
  filter(RegionID == 14) %>%
  mutate(Date = mdy_hms(ReportDate)) 
write.csv(idph_region6noUI,"idph/region6noUI.csv", row.names = FALSE)

#idph_region6noUI <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=14&daysIncluded=0",
 #                           format = "csv")# %>%
  #mutate(Date = mdy_hms(ReportDate)) 

# region 6 hospital beds in use chart

ggplot(idph_region6, aes(x = as.Date(Date), y = COVIDHospitalBedsInUse)) +
  # geom_col(fill = "#910000",
  #          alpha = .25) +
  # geom_line(colour = "#910000",
  #           size = 1) +
  geom_area(fill = "#910000",
            colour = "#910000",
            alpha = .25,
            size = 1) +
  # geom_line(aes(y = rollmean(COVIDHospitalBedsInUse, 
  #                            7, 
  #                            fill = TRUE, 
  #                            align = "right")),
  #           colour = "#910000",
  #           size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "Hospital Beds in Use for COVID-19 in Region 6",
       caption =  "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("region/region6hospitalbeds.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
ggsave("region6hospitalbeds.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)


# region 6 new hospitalizations in use chart
ggplot(idph_region6, aes(x = as.Date(Date), y = CLIAdmissions_RollingAvg)) +
  # geom_col(fill = "#660101",
  #          alpha = .25) +
  # geom_line(colour = "#660101",
  #           size = 1) +
  geom_area(colour = "#660101",
            fill = "#660101",
            alpha = .25,
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "Average New Hospitalizations in Region 6",
       subtitle =  "With seven-day moving average",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/region6newhospital.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
ggsave("region6newhospital.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# new cases chart for region 6
ggplot(idph_region6, aes(x = as.Date(Date), y = PositiveTests)) +
  geom_col(fill = "#B45F06",
           alpha = .25) +
  geom_line(aes(y = avgnewcases),
            colour = "#B45F06",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "New Cases in Region 6",
       subtitle =  "With seven-day moving average",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/region6newcases.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
ggsave("region6newcases.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

