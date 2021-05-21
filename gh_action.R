library(tidyverse)
library(rio)
library(httr)
library(usmap)
library(RColorBrewer)
library(lubridate)
library(extrafont)
library(patchwork)
library(tidycensus)
font_import(prompt=FALSE)
loadfonts()

# us vaccine data ----
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
        plot.title = element_text(size = 16, family = "DIN Condensed Bold")) 

ggsave("gh_action/usa_vax_total.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# us cases data ----
usa_cases_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_latest_external_data"
usa_cases <- rio::import(usa_cases_url,
                         format = "json")
usa_cases <- usa_cases$integrated_county_latest_external_data
usa_cases <- usa_cases %>%
  filter(County != "Unknown County") %>%
  filter(State_name != "Puerto Rico") %>%
  mutate(GEOID = fips_code) %>%
  mutate(fips = fips_code) %>%
  mutate(date = ymd(as_date(report_date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(new_cases_class = cut(x = cases_per_100K_7_day_count_change/7,
                               breaks = c(0,5,15,25,35,50,100,Inf),
                               labels = c("0–5","5–15","15–25","25–35","35–50","50–100","100+"),
                               include.lowest = TRUE,
                               ordered_result = TRUE))



# make transmission map ----
plot_usmap(data = usa_cases, values = "community_transmission_level",
           size = .01) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "Oranges",
                    direction = 1) +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Data last updated",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Verdana"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        #legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "DIN Condensed Bold")) 

ggsave("gh_action/usa_transmission.png", width = 8, height = 8*(628/1200), dpi = 320)

# IL CDC data ----
cdc_county_vaccine_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
cdc_county_vaccine <- rio::import(cdc_county_vaccine_url,
                                  format = "json")
cdc_county_vaccine <- cdc_county_vaccine$vaccination_county_condensed_data 
cdc_county_vaccine <- cdc_county_vaccine %>%
  filter(StateName == "Illinois") %>%
  filter(County != "Unknown County") %>%
  mutate(GEOID = FIPS) %>%
  mutate(date = ymd(as_date(Date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(total_class = 
           cut(x = Series_Complete_Pop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0-10%","10-20%","20-30%","30-40%","40-50%",
                          "50-60%","60-70%","70-80%","80-90%","90-100%"),
               include.lowest = TRUE
           )) %>%
  mutate(adult_class = 
           cut(x = Series_Complete_18PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0-10%","10-20%","20-30%","30-40%","40-50%",
                          "50-60%","60-70%","70-80%","80-90%","90-100%"),
               include.lowest = TRUE
           )) %>%
  mutate(senior_class = 
           cut(x = Series_Complete_65PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0-10%","10-20%","20-30%","30-40%","40-50%",
                          "50-60%","60-70%","70-80%","80-90%","90-100%"),
               include.lowest = TRUE
           ))
cdc_vaccines_geo_merged <- merge(cdc_county_vaccine,
                                 il_counties_clean,
                                 by = "GEOID")  

cdc_cases_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_latest_external_data"
cdc_cases <- rio::import(cdc_cases_url,
                         format = "json")
cdc_cases <- cdc_cases$integrated_county_latest_external_data
cdc_cases <- cdc_cases %>%
  filter(State_name == "Illinois") %>%
  mutate(GEOID = fips_code) %>%
  mutate(date = ymd(as_date(report_date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(new_cases_class = 
           cut(x = cases_per_100K_7_day_count_change/7,
               breaks = c(0,5,15,25,35,50,100,Inf),
               labels = c("0–5","5–15","15–25","25–35","35–50","50–100","100+"),
               include.lowest = TRUE,
               ordered_result = TRUE
           ))

cdc_cases_merged <- merge(cdc_cases,
                          il_counties_clean,
                          by = "GEOID")



# illinois shapefiles ----
il_counties <- get_acs(state = "IL", geography = "county", 
                       variables = "B19013_001", geometry = TRUE)
il_counties_clean <- il_counties %>%
  mutate(variable = NULL) %>%
  mutate(estimate = NULL) %>%
  mutate(moe = NULL)

# IL vax map ----
cdc_total_vax <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = total_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
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
        plot.title = element_text(size = 16, family = "DIN Condensed Bold")) 
cdc_total_vax

# il cases map
cdc_cases_map <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = new_cases_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Oranges",
    direction = 1) +
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_cases_merged$short_date,1)),
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
        plot.title = element_text(size = 16, family = "DIN Condensed Bold")) 
cdc_cases_map

# combined cases and vax map ----
cdc_cases_map + 
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       caption =  NULL,
       fill = NULL)+
  theme(plot.title = element_text(size = 14)
  ) +
  cdc_total_vax +
  theme(plot.title = element_text(size = 14)) 
ggsave("gh_action/CDC_cases_vax_IL.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
