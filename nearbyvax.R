library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
#library(dplyr)
#library(ggplot2)
library(scales)
library(zoo)
library(clipr)
library(ggrepel)

# get data ready ----

c("Champaign","Vermilion","Ford","Edgar","Douglas","Piatt","Iroquois",
  "De%20Witt","Macon","Moultrie")

region6 <- c("Iroquois", "Ford", "De%20Witt", "Piatt", "Champaign", "Vermilion", "Macon", "Moultrie", "Douglas", "Edgar", "Shelby", "Coles", "Cumberland", "Clark", "Fayette", "Effingham", "Jasper", "Crawford", "Clay", "Richland", "Lawrence") 

lefttodo <-  c("Shelby", "Coles", "Cumberland", "Clark", "Fayette", "Effingham", "Jasper", "Crawford", "Clay", "Richland", "Lawrence")

# set population variables
champaignpop <- 209983
vermilionpop <- 76806
fordpop <- 13264
edgarpop <- 17360
douglaspop <- 19479
piattpop <- 16396
iroquoispop <- 27604
dewittpop <- 15769
maconpop <- 104712
moultriepop <- 14717
illinoispop <- 12741080
colespop <- 50885
mcleanpop <- 172828



# import and clean data

vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
            format = "csv") %>%
  mutate(population = champaignpop) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_vermilion <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Vermilion",
                             format = "csv") %>%
  mutate(population = vermilionpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_ford <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Ford",
                             format = "csv") %>%
  mutate(population = fordpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_edgar <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Edgar",
                        format = "csv") %>%
  mutate(population = edgarpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_douglas <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Douglas",
                          format = "csv") %>%
  mutate(population = douglaspop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_piatt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Piatt",
                        format = "csv") %>%
  mutate(population = piattpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_iroquois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Iroquois",
                            format = "csv") %>%
  mutate(population = iroquoispop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_dewitt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=De%20Witt",
                         format = "csv") %>%
  mutate(population = dewittpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_macon <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Macon",
                        format = "csv") %>%
  mutate(population = maconpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_moultrie <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Moultrie",
                           format = "csv") %>%
  mutate(population = moultriepop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_coles <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Coles",
                            format = "csv") %>%
  mutate(population = colespop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_mclean <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=McLean",
                         format = "csv") %>%
  mutate(population = mcleanpop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_illinois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                            format = "csv") %>%
  mutate(population = illinoispop)  %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 


vax_nearby <- full_join(vax_champaign, vax_vermilion) %>%
  full_join(vax_ford) %>%
  full_join(vax_edgar) %>%
  full_join(vax_douglas) %>%
  full_join(vax_piatt) %>%
  full_join(vax_iroquois) %>%
  full_join(vax_dewitt) %>%
  full_join(vax_macon) %>%
  full_join(vax_moultrie) %>%
  #full_join(vax_mclean) %>%
  #full_join(vax_coles) %>%
  full_join(vax_illinois) %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
#  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(PercentDose1 = PersonsDose1/population) %>%
  mutate(PercentOnlyDose1 = PercentDose1 - PctVaccinatedPopulation)
#%>%
 # mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
#  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated))

# line chart comparing all the counties for dose 2 ----
ggplot(vax_nearby, aes(x = as.Date(Date), y = PctVaccinatedPopulation,
                       colour = CountyName)) +
  geom_line() +
  geom_text(data = filter(vax_nearby, as.Date(Date) == last(Date)),
            aes(label = CountyName,
                colour = CountyName),
            #   direction = "y",
            hjust = 0,
            family = "Barlow",
            size = 4.6) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title ="Percent of Population Fully Vaccinated in Nearby Counties",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
# ggsave("vax/nearbydose2.png", width = 8, height = 8*(628/1200), dpi = 320)
# ggsave("nearbydose2.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)

#?geom_text_repel
#?geom_rect
#max(vax_nearby$Date)
#ymd(tail(vax_nearby$Date,1))
#last_day <- filter(vax_nearby, as.Date(Date) == last(Date))
# line chart comparing all counties dose 2, with ggrepel ----
ggplot(vax_nearby, aes(x = as.Date(Date), y = PctVaccinatedPopulation,
                       group = CountyName,
                       colour = CountyName)) +
  # geom_rect(
  #           aes(xmin = as.Date(ymd(tail(Date,1))), xmax = Inf, 
  #           ymin = 0, ymax = Inf),
  #           colour = "white",
  #           fill = "white"
  #           ) +
  # annotate("rect", 
  #          xmin = as.Date("2021-03-06"), xmax = Inf, 
  #          ymin = 0, ymax = Inf,
  #          fill = "white") +
  geom_line() +
  geom_text_repel(data = filter(vax_nearby, as.Date(Date) == last(Date)),
                  aes(label = CountyName,
                      colour = CountyName),
                  nudge_x = 600,
                  segment.curvature = -0.05,
                  segment.color = 'grey',
                  segment.inflect = TRUE,
                  #segment.curvature = -0.1,
                  hjust = "left", direction="y",
                  family = "Barlow",
                  size = 4.6)+
  scale_y_continuous(#labels = percent, 
                     labels = label_percent(accuracy = 1),
                     # position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.35))) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Percent of Population Fully Vaccinated in Nearby Counties",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/nearbydose2.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("nearbydose2.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# line chart comparing all counties dose 1, with ggrepel ----
#as.Date("2021-03-03")
#as.Date(ymd(tail(vax_nearby$Date,1)))
ggplot(vax_nearby, aes(x = as.Date(Date), y = PercentDose1,
                       group = CountyName,
                       colour = CountyName)) +
  # geom_rect(
  #           aes(xmin = as.Date(ymd(tail(Date,1))), xmax = Inf, 
  #           ymin = 0, ymax = Inf),
  #           colour = "white",
  #           fill = "white"
  #           ) +
  # annotate("rect",
  #          xmin = as.Date(ymd(tail(vax_nearby$Date,1))), 
  #          xmax = Inf,
  #          ymin = 0, ymax = Inf,
  #          fill = "white") +
  geom_line() +
  geom_text_repel(data = filter(vax_nearby, as.Date(Date) == last(Date)),
                  aes(label = CountyName,
                      colour = CountyName),
                  nudge_x = 600,
                  segment.curvature = -0.05,
                  segment.color = 'grey',
                  segment.inflect = TRUE,
                  #segment.curvature = -0.1,
                  hjust = "left", direction="y",
                  family = "Barlow",
                  size = 4.6)+
  scale_y_continuous(#labels = percent, 
                     labels = label_percent(accuracy = 1),
                     # position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.35))) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Percent with At Least One Vaccine Dose in Nearby Counties",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.background = element_blank(),
        legend.position = "none",
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/nearbydose1.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("nearbydose1.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# line chart of dose 1 ----
ggplot(vax_nearby, aes(x = as.Date(Date), y = PercentDose1,
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
  labs(title = "Percent of Population Partially Vaccinated in Nearby Counties",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
# ggsave("vax/nearbydose1.png", width = 8, height = 8*(628/1200), dpi = 320)
# ggsave("nearbydose1.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)



# cleveland dot plot of dose 2 ----
last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1))
ggplot(last_vax_nearby, aes(x = PctVaccinatedPopulation, 
                            y = reorder(CountyName,
                                        PctVaccinatedPopulation))) +
  geom_segment(aes(yend = CountyName), xend = 0, colour = "grey80",
               size = 1.2) +
  geom_point(size = 2.4) +
  geom_text(aes(label = percent(PctVaccinatedPopulation, .1)),
            hjust = -.35,
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
                     limits = c(0,1),
                     #limits = c(0,max(last_vax_nearby$PctVaccinatedPopulation)),
                     #   xlim = 0,
                     expand = expansion(mult = c(0,.05))) +
  # scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Population Fully Vaccinated in Nearby Counties",
        caption =  "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        #        panel.grid.minor.x = element_blank(),   
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
# ggsave("vax/nearbycombineddose2.png", width = 8, height = 8*(628/1200), dpi = 320)
# ggsave("nearbycombineddose2.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)

# cleveland dot plot of dose 1
last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1))
ggplot(last_vax_nearby, aes(x = PercentDose1, 
                            y = reorder(CountyName,
                                        PercentDose1))) +
  geom_segment(aes(yend = CountyName), xend = 0, colour = "grey80",
               size = 1.2) +
  geom_point(size = 2.4) +
  geom_text(aes(label = percent(PercentDose1, .1)),
            hjust = -.35,
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
                     limits = c(0,1),
                     #limits = c(0,max(last_vax_nearby$PctVaccinatedPopulation)),
                     #   xlim = 0,
                     expand = expansion(mult = c(0,.05))) +
  # scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Population with At Least One Dose in Nearby Counties",
        caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        #        panel.grid.minor.x = element_blank(),   
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
# ggsave("vax/nearbycombinedonedose.png", width = 8, height = 8*(628/1200), dpi = 320)
# ggsave("nearbycombinedonedose.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)

# combined cleveland dot ----
last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1)) %>%
  arrange(desc(PercentDose1))
topcounty <- head(last_vax_nearby$CountyName,1)

ggplot(last_vax_nearby, aes(y = reorder(CountyName,
                                        PercentDose1))) +
 
  geom_segment(aes(x = PercentDose1, # line segment to dose1%
                   yend = CountyName),
               xend = 0, 
               colour = "#d8cee8",
               size = 3.4, # was 2
               #alpha = .3
               ) +
  geom_segment(aes(x = PctVaccinatedPopulation, # first line segment to dose2
                   yend = CountyName), 
               xend = 0, 
               colour = "#674EA7",
               size = 3.4) + # was 3.6 +
  geom_text(data = filter(last_vax_nearby, # label for all but top county dose1
                          CountyName != topcounty),
            aes(x = PercentDose1,
                label = percent(PercentDose1, .1)),
            hjust = -.2,
            size = 3.5,
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for top county, dose1
                          CountyName == topcounty),
            aes(x = PercentDose1,
                label = paste(percent(PercentDose1, .1),
                              "has received at least one dose")),
            hjust = -0.04,
            size = 3.5,
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for all but top county, dose2
                          CountyName != topcounty),
            aes(x = PctVaccinatedPopulation,
                label = percent(PctVaccinatedPopulation, .1)),
            hjust = .4,
            vjust = -.9,
            size = 3.5,
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for top county, dose2
                          CountyName == topcounty),
            aes(x = PctVaccinatedPopulation,
                label = paste("Fully vaccinated:",percent(PctVaccinatedPopulation, .1)
                              )),
            hjust = .85,
            vjust = -.9,
            size = 3.5,
            family = "Barlow") +
  scale_x_continuous(labels = percent,
                     #position = "top",
                     limits = c(0,1),
                     expand = expansion(mult = c(0,.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Total Population Vaccinated in Nearby Counties",
        caption =  "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        #axis.line.y = element_line(colour = "grey40"),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        panel.grid.major.x = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
# ggsave("vax/nearbybothdoses.png", width = 8, height = 8*(628/1200), dpi = 320)
# #ggsave("vax/card/nearbybothdosesCard.png", width = 8, height = 1256/300, dpi = 320)
# ggsave("nearbybothdoses.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)

# combined cleveland dot, with wider bars and interior labels ----

last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1)) %>%
  arrange(desc(PercentDose1))
topcounty <- head(last_vax_nearby$CountyName,1)

ggplot(last_vax_nearby, aes(y = reorder(CountyName,
                                        PercentDose1))) +
  
  geom_segment(aes(x = PercentDose1, # line segment to dose1%
                   yend = CountyName),
               xend = 0, 
               colour = "#d8cee8",
               size = 7.7, # was 2
               #alpha = .3
  ) +
  geom_segment(aes(x = PctVaccinatedPopulation, # first line segment to dose2
                   yend = CountyName), 
               xend = 0, 
               colour = "#674EA7",
               size = 7.7) + # was 3.6 +
  geom_text(data = filter(last_vax_nearby, # label for all but top county dose1
                          CountyName != topcounty),
            aes(x = PercentDose1,
                label = percent(PercentDose1, .1)),
            hjust = -.1,
            size = 3.5,
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for top county, dose1
                          CountyName == topcounty),
            aes(x = PercentDose1,
                label = paste(percent(PercentDose1, .1),
                              "has received at least one dose")),
            hjust = -0.02,
            size = 3.5,
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for all but top county, dose2
                          CountyName != topcounty),
            aes(x = PctVaccinatedPopulation,
                label = percent(PctVaccinatedPopulation, .1)),
            hjust = 1.1,
            #vjust = -.9,
            size = 3.5,
            colour = "white",
            family = "Barlow") +
  geom_text(data = filter(last_vax_nearby, # label for top county, dose2
                          CountyName == topcounty),
            aes(x = PctVaccinatedPopulation,
                label = paste("Fully vaccinated:",
                              percent(PctVaccinatedPopulation, .1)
                              )),
            hjust = 1.02,
            #vjust = -.9,
            size = 3.5,
            colour = "white",
            family = "Barlow") +
  scale_x_continuous(labels = percent,
                     #position = "top",
                     limits = c(0,1),
                     expand = expansion(mult = c(0,.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Total Population Vaccinated in Nearby Counties",
       caption =  "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/nearbybothdoses.png", width = 8, height = 8*(628/1200), dpi = 320)
#ggsave("vax/card/nearbybothdosesCard.png", width = 8, height = 1256/300, dpi = 320)
ggsave("nearbybothdoses.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# grid of nearby vaccination rates ----
last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1)) %>%
  arrange(desc(PercentDose1))

vax_nearby_facet <- vax_nearby %>%
  select(CountyName, Date, PctVaccinatedPopulation, PercentOnlyDose1, PercentDose1) %>%
  pivot_longer(cols = c(PctVaccinatedPopulation, PercentOnlyDose1),
               names_to = "Doses",
               values_to = "Percent") #%>%
 # arrange(desc(PercentDose1))
  #%>%
 # reorder(CountyName, PctVaccinatedPopulation +PercentOnlyDose1)
vax_nearby_facet$Doses <- as.factor(vax_nearby_facet$Doses)
vax_nearby_facet$Doses <- factor(vax_nearby_facet$Doses, 
                                 levels = rev(levels(vax_nearby_facet$Doses)))
vax_nearby_facet$CountyName <- as.factor(vax_nearby_facet$CountyName)
vax_nearby_facet$CountyName <- factor(vax_nearby_facet$CountyName,
                                      levels = last_vax_nearby$CountyName)



ggplot(data = vax_nearby_facet,
       aes(x = as.Date(Date),
           y = Percent,
           fill = Doses,
           colour = Doses)) +
  geom_area(alpha = .6) +
  scale_fill_manual(labels = c("At least one dose",
                                 "Fully vaccinated"),
                    values = c("#d8cee8","#674EA7"),
                    guide = guide_legend(title = NULL)) +
  scale_colour_manual(labels = c("At least one dose",
                                 "Fully vaccinated"),
                      values = c("#A897CC","#674EA7"),
                      guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = percent,
                     position = "right"
                     # breaks = c(0,
                     #            max(vax_nearby_facet$Percent),
                     #            max(vax_nearby_facet$PercentDose1))
                     ) +
  scale_x_date(expand = c(0,0)) +
  xlab(NULL) +
  ylab(NULL) +
  #expand_limits(y = 0) +
  #guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~ CountyName) + 
  labs(title = "Percent of Total Population Vaccinated in Nearby Counties",
       #subtitle =  "With seven-day moving average",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        legend.position = c(.9,.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        #legend.text = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        #strip.placement = "outside",
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 24, family = "Oswald"))

ggsave("vax/nearbyfacet.png", width = 8, height = 6, dpi = 320)
#ggsave("vax/card/nearbybothdosesCard.png", width = 8, height = 1256/300, dpi = 320)
ggsave("nearbyfacet.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 6, dpi = 320)

#?facet_wrap
# todo
# add new dose1 and new dose2 and percent of each dose for each county
# before merging
# facet by county
# set county pops as variables