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

# todo
# - [x] save the charts, prob replace the manual-made ones
# - [x] save the data to idph folder
# - [x] generate text