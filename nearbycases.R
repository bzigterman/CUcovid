library(tidyverse)
library(lubridate)

c("Champaign","Vermilion","Ford","Edgar","Douglas","Piatt","Iroquois",
  "De%20Witt","Macon","Moultrie")

idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                             format = "json") 
idph_cases_champaign <- idph_cases_champaign$values

idph_cases_vermilion <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Vermilion",
                                    format = "json") 
idph_cases_vermilion <- idph_cases_vermilion$values

idph_cases_ford <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Ford",
                                    format = "json") 
idph_cases_ford <- idph_cases_ford$values

idph_cases_edgar <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Edgar",
                                    format = "json") 
idph_cases_edgar <- idph_cases_edgar$values

idph_cases_douglas <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Douglas",
                                    format = "json") 
idph_cases_douglas <- idph_cases_douglas$values

idph_cases_piatt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Piatt",
                                    format = "json") 
idph_cases_piatt <- idph_cases_piatt$values

idph_cases_iroquois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Iroquois",
                                    format = "json") 
idph_cases_iroquois <- idph_cases_iroquois$values

idph_cases_dewitt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=De%20Witt",
                                    format = "json") 
idph_cases_dewitt <- idph_cases_dewitt$values

idph_cases_macon <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Macon",
                                    format = "json") 
idph_cases_macon <- idph_cases_macon$values

idph_cases_moultrie <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Moultrie",
                                    format = "json") 
idph_cases_moultrie <- idph_cases_moultrie$values

idph_cases_nearby <- full_join(idph_cases_champaign, idph_cases_vermilion) %>%
  full_join(idph_cases_ford) %>%
  full_join(idph_cases_edgar) %>%
  full_join(idph_cases_douglas) %>%
  full_join(idph_cases_piatt) %>%
  full_join(idph_cases_iroquois) %>%
  full_join(idph_cases_dewitt) %>%
  full_join(idph_cases_macon) %>%
  full_join(idph_cases_moultrie) %>%
  mutate(Date = ymd_hms(reportDate)) 

# chart comparing cases
ggplot(idph_cases_nearby, aes(x = as.Date(Date), y = confirmed_cases,
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
  ggtitle("total cases over time in nearby counties")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(size = 22, family = "Oswald")) 
#ggsave("vax/nearby.png", width = 8, height = 32/7, dpi = 320)
# ggsave("nearby.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 32/7, dpi = 150)

  
  