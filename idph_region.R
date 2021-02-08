library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(clipr)

idph_region6 <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=6&daysIncluded=0",
                                  format = "csv") %>%
  filter(RegionID == 6) %>%
  mutate(Date = mdy_hms(ReportDate)) 
write.csv(idph_region6,"idph/region6.csv", row.names = FALSE)



#idph_region6noUI <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=14&daysIncluded=0",
 #                           format = "csv")# %>%
  #mutate(Date = mdy_hms(ReportDate)) 

# region 6 hospital beds in use chart

ggplot(idph_region6, aes(x = as.Date(Date), y = COVIDHospitalBedsInUse)) +
  geom_col(fill = "#910000",
           alpha = .25) +
  geom_line(aes(y = rollmean(COVIDHospitalBedsInUse, 
                             7, 
                             fill = TRUE, 
                             align = "right")),
            colour = "#910000",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("Hospital Beds in Use for COVID-19 in Region 6",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("region/region6hospitalbeds.png", 
       width = 8, height = 32/7, dpi = 320)
ggsave("region6hospitalbeds.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)


# region 6 new hospitalizations in use chart

ggplot(idph_region6, aes(x = as.Date(Date), y = CLIAdmissions_RollingAvg)) +
  geom_col(fill = "#660101",
           alpha = .25) +
  geom_line(colour = "#660101",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("Average New Hospitalizations in Region 6",
          "Seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/region6newhospital.png", 
       width = 8, height = 32/7, dpi = 320)
ggsave("region6newhospital.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 320)

