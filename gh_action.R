library(tidyverse)
library(rio)
library(httr)
library(usmap)
library(RColorBrewer)
library(lubridate)
library(extrafont)
font_import(prompt=FALSE)
loadfonts()


usa_county_vaccine_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
usa_county_vaccine <- rio::import(usa_county_vaccine_url,
                                  format = "json")
usa_county_vaccine <- usa_county_vaccine$vaccination_county_condensed_data 
usa_county_vaccine <- usa_county_vaccine %>%
  filter(County != "Unknown County") %>%
  filter(StateName != "Puerto Rico") %>%
  mutate(GEOID = FIPS) %>%
  mutate(fips = FIPS) %>%
  mutate(date = ymd(as_date(Date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(total_class = 
           cut(x = Series_Complete_Pop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0-10%","10-20%","20-30%","30-40%","40-50%",
                          "50-60%","60-70%","70-80%","80-90%","90-100%"),
               include.lowest = TRUE)) 

# make vaccine map ----
plot_usmap(data = usa_county_vaccine, values = "total_class",
           size = .01) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Data last updated",
                        tail(usa_county_vaccine$short_date,1)),
       fill = NULL)+
  theme(text = element_text(family = "Verdana"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Georgia")) 

ggsave("gh_action/usa_vax_total.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
