library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(clipr)

c("Champaign","Vermilion","Ford","Edgar","Douglas","Piatt","Iroquois",
  "De%20Witt","Macon","Moultrie")

# import and clean data

vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
            format = "csv") 
vax_vermilion <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Vermilion",
                             format = "csv") 
vax_ford <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Ford",
                             format = "csv") 
vax_edgar<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Edgar",
                        format = "csv") 
vax_douglas<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Douglas",
                          format = "csv") 
vax_piatt<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Piatt",
                        format = "csv") 
vax_iroquois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Iroquois",
                            format = "csv") 
vax_dewitt<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=De%20Witt",
                         format = "csv") 
vax_macon<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Macon",
                        format = "csv") 
vax_moultrie<- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Moultrie",
                           format = "csv") 
vax_nearby <- full_join(vax_champaign, vax_vermilion) %>%
  full_join(vax_ford) %>%
  full_join(vax_edgar) %>%
  full_join(vax_douglas) %>%
  full_join(vax_piatt) %>%
  full_join(vax_iroquois) %>%
  full_join(vax_dewitt) %>%
  full_join(vax_macon) %>%
  full_join(vax_moultrie) %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) #%>%
  #mutate(PercentDose1 = PersonsDose1/209983) #%>%
 # mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
#  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated))

# line chart comparing all the counties
ggplot(vax_nearby, aes(x = as.Date(Date), y = PctVaccinatedPopulation,
                       colour = CountyName)) +
  geom_line() +
  geom_text(data = filter(vax_nearby, as.Date(Date) == last(Date)),
            aes(label = CountyName,
                colour = CountyName),
            hjust = 0,
            family = "Barlow",
            size = 4.6) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Percent of Population Fully Vaccinated in Nearby Counties")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("vax/nearby.png", width = 8, height = 32/7, dpi = 320)
ggsave("nearby.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# cleveland dot plot
last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1))
ggplot(last_vax_nearby, aes(x = PctVaccinatedPopulation, 
                            y = reorder(CountyName,
                                        PctVaccinatedPopulation))) +
  geom_segment(aes(yend = CountyName), xend = 0, colour = "grey50",
               size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = percent(PctVaccinatedPopulation, .1)),
            hjust = -.5,
            size = 4,
            family = "Barlow") +
  #facet_grid(. ~ CountyName) +
  # geom_text(data = filter(vax_nearby, as.Date(Date) == last(Date)),
  #           aes(label = CountyName,
  #               colour = CountyName),
  #           hjust = 0,
  #           family = "Barlow",
  #           size = 4.6) +
  scale_x_continuous(labels = percent,
                     #position = "right",
                     limits = c(0,max(last_vax_nearby$PctVaccinatedPopulation)),
                     #   xlim = 0,
                     expand = expansion(mult = c(0,.1))) +
  # scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  ggtitle("Percent of Population Fully Vaccinated in Nearby Counties",
          "Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        #        panel.grid.minor.x = element_blank(),   
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("vax/nearbycombined.png", width = 8, height = 32/7, dpi = 320)

# todo
# add new dose1 and new dose2 and percent of each dose for each county
# before merging
# facet by county
# set county pops as variables