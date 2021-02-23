library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
# path = "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyRates"
# champaign_test <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyRates?countyName=Champaign")
# champaign_test <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyRates",
#                               format = "json") 


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


c("Champaign","Vermilion","Ford","Edgar","Douglas","Piatt","Iroquois",
  "De%20Witt","Macon","Moultrie")

# combine csvs

idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                             format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(population = champaignpop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                  fill = NA, align = "right")) 

idph_cases_vermilion <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Vermilion",
                                    format = "json") 
idph_cases_vermilion <- idph_cases_vermilion$values %>%
  mutate(population = vermilionpop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_ford <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Ford",
                                    format = "json") 
idph_cases_ford <- idph_cases_ford$values %>%
  mutate(population = fordpop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) 

idph_cases_edgar <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Edgar",
                                    format = "json") 
idph_cases_edgar <- idph_cases_edgar$values %>%
  mutate(population = edgarpop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_douglas <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Douglas",
                                    format = "json") 
idph_cases_douglas <- idph_cases_douglas$values %>%
  mutate(population = douglaspop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_piatt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Piatt",
                                    format = "json") 
idph_cases_piatt <- idph_cases_piatt$values %>%
  mutate(population = piattpop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_iroquois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Iroquois",
                                    format = "json") 
idph_cases_iroquois <- idph_cases_iroquois$values %>%
  mutate(population = iroquoispop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_dewitt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=De%20Witt",
                                    format = "json") 
idph_cases_dewitt <- idph_cases_dewitt$values %>%
  mutate(population = dewittpop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_macon <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Macon",
                                    format = "json") 
idph_cases_macon <- idph_cases_macon$values %>%
  mutate(population = maconpop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_moultrie <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Moultrie",
                                    format = "json") 
idph_cases_moultrie <- idph_cases_moultrie$values %>%
  mutate(population = moultriepop) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths))  %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) 

idph_cases_nearby <- full_join(idph_cases_champaign, idph_cases_vermilion) %>%
  full_join(idph_cases_ford) %>%
 # full_join(idph_cases_edgar) %>%
  full_join(idph_cases_douglas) %>%
  full_join(idph_cases_piatt) %>%
  full_join(idph_cases_iroquois) %>%
 # full_join(idph_cases_dewitt) %>%
 # full_join(idph_cases_macon) %>%
 # full_join(idph_cases_moultrie) %>%
  mutate(Date = ymd_hms(reportDate)) %>%
  mutate(new_case_rate = (100000*avg_new_cases)/population) %>%
  mutate(new_deaths_rate = (1000000*avg_new_deaths)/population)

# chart comparing cases
ggplot(idph_cases_nearby, aes(x = as.Date(Date), y = new_case_rate,
                       colour = CountyName)) +
  geom_line() +
  geom_text(data = filter(idph_cases_nearby, as.Date(Date) == last(Date)),
            aes(label = CountyName,
                colour = CountyName),
            hjust = 0,
            family = "Barlow") +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("New Cases per 100,000 Residents in Nearby Counties",
          "Seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("region/nearbycases.png", width = 8, height = 32/7, dpi = 320)
# ggsave("nearby.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 32/7, dpi = 150)

# chart comparing deaths
ggplot(idph_cases_nearby, aes(x = as.Date(Date), y = new_deaths_rate,
                              colour = CountyName)) +
  geom_line() +
  geom_text(data = filter(idph_cases_nearby, as.Date(Date) == last(Date)),
            aes(label = CountyName,
                colour = CountyName),
            hjust = 0,
            family = "Barlow") +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,.15))) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("avg new deaths over time in nearby counties")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(size = 22, family = "Oswald")) 

  