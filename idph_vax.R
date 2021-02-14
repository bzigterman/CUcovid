library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(clipr)

# import and clean data
idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(PercentDose1 = PersonsDose1/209983) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) %>%
  mutate(Text = paste(AdministeredCount,
                      " total doses administered in the past day, up ", 
                      AdministeredCountChange, 
                      " from the day before. ▪︎ ",
                      PersonsDose1,
                      " have received the first dose, up ",
                      Dose1Change,
                      " from the day before. ",
                      percent(PercentDose1,
                              accuracy = .1),
                      "% of the population has received first dose. ▪︎ ",
                      PersonsFullyVaccinated,
                      " have received both doses, up ",
                      Dose2Change,
                      " from the day before. ",
                      percent(PctVaccinatedPopulation,
                              accuracy = .1),
                      "% of the population is fully vaccinated.",
                      sep = ""))
write.csv(idph_vax_champaign,"idph/vax_champaign.csv", row.names = FALSE)
write_clip(tail(idph_vax_champaign$Text, n = 1)) # paste text to clipboard

# new vaccines administered chart copy
ggplot(idph_vax_champaign, 
       aes(x = as.Date(Date), y = AdministeredCountChange)) +
  geom_col(fill = "#674EA7",
           alpha = .25) +
  geom_line(aes(y = AdministeredCountRollAvg),
            colour = "#674EA7",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("New Vaccine Doses Administered in Champaign County",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/NewVaccines.png", width = 8, height = 32/7, dpi = 320)
ggsave("NewVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# first and second dose comparison chart
ggplot(idph_vax_champaign, aes(x = as.Date(Date),
                y = PersonsDose1))  +
  geom_area(colour = "#674EA7",
            fill = "#674EA7",
            alpha = .3) +
  geom_area(aes(x = as.Date(Date), y = PersonsFullyVaccinated),
            colour = "#674EA7",
            fill = "#674EA7",
            alpha = 1) +
  geom_label(aes(label = paste("First dose:",
                               percent(PercentDose1, accuracy = .1))),
             data = tail(idph_vax_champaign, 1),
             size = 4,
             hjust = 1.3,
             family = "Barlow") +
  geom_label(data = tail(idph_vax_champaign, 1),
             aes(x = as.Date(Date), 
                 y = PersonsFullyVaccinated,
                 label = paste("Fully vaccinated:",
                               percent(PctVaccinatedPopulation, accuracy = .1))),
             size = 4,
             hjust = 1.2,
             vjust = 0,
             family = "Barlow") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("People Vaccinated in Champaign County",
          "With percent of total population. Source: IDPH") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/chamvax.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# first dose projection
avgdose1change <- mean(tail(idph_vax_champaign$Dose1Change,7), na.rm = TRUE)
xmin <- max(as.Date(idph_vax_champaign$Date))
xmax <- as.Date("2023-04-01")
ymin <- max(idph_vax_champaign$PersonsDose1)
projected <- data.frame(Date = seq(xmin, xmax, by = "day"))
projected$PersonsDose1 <- seq(ymin,
                              by = avgdose1change,
                              length.out = nrow(projected))
projected <- projected %>%
  filter(PersonsDose1 <209983) %>%
  mutate(PercentDose1 = PersonsDose1/209983)

# points
current_vax <- idph_vax_champaign %>%
  tail(1)
half_vax <- projected %>%
  filter(PercentDose1 >.5) %>%
  head(1)
eighty_vax <- projected %>%
  filter(PercentDose1 >.8) %>%
  head(1)

# plot
ggplot(idph_vax_champaign, aes(x = as.Date(Date),
                               y = PercentDose1)) +
  geom_line(colour = "#800080", # actual line 
            size = 1.5) +
  geom_line(data = projected, # projected line
            aes(x = as.Date(Date),
                y = PercentDose1),
            linetype = "dashed",
            size = 1.5, 
            colour = "#008040") +
  geom_point(data = current_vax, # current point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#800080",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = current_vax, # current label
             aes(x = as.Date(Date),
                 y = PercentDose1,
                 label = paste(percent(PercentDose1),
                               "have already received first dose")),
            family = "Barlow",
            hjust = -.05,
            vjust = 1.5,
            size = 4.4) +
  geom_point(data = half_vax, # half point projection point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = half_vax, # half point projection label
            aes(x = as.Date(Date),
                y = PercentDose1,
                label = paste("50% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -1.3,
            size = 4.4) +
  geom_point(data = eighty_vax, # eighty pct point projection point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = eighty_vax, # eighty pct point projection label
            aes(x = as.Date(Date),
                y = PercentDose1,
                label = paste("80% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -1.3,
            size = 4.4) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     limits = c(0,1),
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("When a Percent of Champaign County Residents Might Receive First Vaccine Dose",
          "Based on average new first doses administered over the past week") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 18, family = "Oswald")) 

ggsave("vax/VaccineProjection.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccineProjection.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# todo
# - [x] save the charts, prob replace the manual-made ones
# - [x] save the data to idph folder
# - [x] generate text