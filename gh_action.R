library(tidyverse)
library(zoo)
library(scales)
library(rio)
library(httr)
library(usmap)
library(RColorBrewer)
library(lubridate)
library(extrafont)
library(sf)
library(patchwork)
font_import(prompt=FALSE)
loadfonts()
options(tigris_use_cache = TRUE)

# us----
## us vaccine data ----
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
               breaks = c(0,10,20,30,40,50,60,70,80,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–100%"),
               include.lowest = TRUE)) 

## make vaccine map ----
plot_usmap(data = usa_county_vaccine, values = "total_class",
           size = .01) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Data updated",
                        tail(usa_county_vaccine$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
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
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 

ggsave("gh_action/usa_vax_total.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

## us cases data ----
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



## make transmission map ----
plot_usmap(data = usa_cases, values = "community_transmission_level",
           size = .01) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "Oranges",
                    direction = 1) +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Data updated",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),  
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 

ggsave("gh_action/usa_transmission.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# illinois ----
illinoispop <- 12741080

## illinois shapefiles ----
rdsurl <- "https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/il_counties.rds"
il_counties_clean <- rio::import(rdsurl)

## IL CDC data ----
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
               breaks = c(0,10,20,30,40,50,60,70,80,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–100%"),
               include.lowest = TRUE
           )) %>%
  mutate(adult_class = 
           cut(x = Series_Complete_18PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–100%"),
               include.lowest = TRUE
           )) %>%
  mutate(senior_class = 
           cut(x = Series_Complete_65PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–100%"),
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

## IL vax map ----
# cdc_total_vax <- ggplot(data = cdc_vaccines_geo_merged) +
#   geom_sf(data = cdc_vaccines_geo_merged,
#           mapping = aes(fill = total_class,
#                         geometry = geometry),
#           size = .25) +
#   coord_sf(crs = st_crs(4326)) +
#   scale_fill_brewer(
#     palette = "Purples",
#     direction = 1) +
#   labs(title = "Percent Fully Vaccinated",
#        caption =  paste("Source: CDC. Data updated",
#                         tail(cdc_vaccines_geo_merged$short_date,1)),
#        fill = NULL)+
#   theme(axis.text = element_blank(),
#         axis.line.x = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         legend.position = "right",
#         panel.grid.major = element_blank(),  
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.key.size = unit(.5, "cm"),
#         panel.background = element_blank(),
#         plot.caption = element_text(colour = "grey40")) 
# cdc_total_vax

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
       caption =  paste("Source: CDC. Data updated",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
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
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 
cdc_cases_map

## il transmission level ----
cdc_transmission <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = community_transmission_level,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "Oranges",
                    direction = 1) +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Data updated",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.caption.position = "plot",
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 
cdc_transmission 

## combined cases and vax map ----
cdc_transmission +
  labs(title = "Community Transmission Levels",
       caption =  NULL,
       fill = NULL)+
  theme(plot.title = element_text(size = 10)
  ) +
cdc_cases_map + 
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       #caption =  NULL,
       fill = NULL)+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9)
  ) #+
  #cdc_total_vax +
  #theme(plot.title = element_text(size = 10)) 
ggsave("gh_action/IL_cases_transmission.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

## combined il vax ----
### pct 65+ map ----
cdc_total_vax_65 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = senior_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  labs(title = "Percent Fully Vaccinated 65+",
       caption =  paste("Source: CDC. Data updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
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
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 
cdc_total_vax_65

### pct 18+ ----
cdc_total_vax_18 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = adult_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  labs(title = "Percent Fully Vaccinated 18+",
       caption =  paste("Source: CDC. Data updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
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
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 
cdc_total_vax_18

### pct total pop vaccinated map ----
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
       caption =  paste("Source: CDC. Data updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(#text = element_text(family = "Gill Sans"),
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
        plot.caption = element_text(colour = "grey40")#,
        #plot.title = element_text(size = 16, family = "Gill Sans")
        ) 
cdc_total_vax

## combined vax maps ----
cdc_total_vax + 
  labs(title = "Percent Fully Vaccinated",
       subtitle = "Total Population",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6)) +
  cdc_total_vax_18 +
  labs(title = NULL,
       subtitle = "18 and older",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6)) +
  cdc_total_vax_65 +
  labs(title = NULL,
       subtitle = "65 and older",
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6))

ggsave("gh_action/IL_vax_combined.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# idph data ----
## state case data ----
# 
# idph_cases_IL <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Illinois",
#                              format = "json") 
# idph_cases_IL <- idph_cases_IL$values %>%
#   mutate(population = illinoispop)  %>%
#   mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
#   mutate(new_deaths = deaths - lag(deaths)) %>%
#   mutate(avg_new_cases = rollmean(new_cases, k = 7, 
#                                   fill = NA, align = "right")) %>%
#   mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
#                                    fill = NA, align = "right")) %>%
#   mutate(Date = ymd_hms(reportDate)) 
# 
# idph_cases_deaths_IL <- idph_cases_IL %>%
#   select(Date, avg_new_cases, avg_new_deaths) %>%
#   mutate("Average New Cases" =  avg_new_cases) %>%
#   mutate("Average New Deaths" =  avg_new_deaths) %>%
#   pivot_longer(cols = c("Average New Cases","Average New Deaths"),
#                values_to = "Number",
#                names_to = "New") %>%
#   mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
#                             mday(Date))) 

## facet chart of IL cases and deaths ----
# ggplot(idph_cases_deaths_IL,
#        aes(x = as.Date(Date),
#            y = Number,
#            colour = New)) +
#   geom_line() +
#   facet_wrap(~ New, scales = "free_y",
#              ncol = 1) +
#   labs(caption = paste("Source: IDPH. Data updated",
#                        tail(idph_cases_deaths_IL$short_date,1))) +
#   xlab(NULL) +
#   ylab(NULL) +
#   scale_x_date(expand = c(0,0)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1),
#                      position = "right",
#                      expand = expansion(mult = c(0,.05))
#   ) +
#   expand_limits(y = 0) +
#   scale_colour_manual(guide = FALSE,
#                       values = c("#B45F06",
#                                  "#d90000","#674EA7","#674EA7")) +
#   theme(#text = element_text(family = "Gill Sans"),
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 8),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(colour = "grey93"),
#         strip.text = element_text(size = 11),
#         strip.background = element_blank(),
#         plot.caption = element_text(colour = "grey40")#,
#         #plot.title = element_text(size = 18, family = "Gill Sans")
#         )
# 
# ggsave("gh_action/state_Cases_Deaths.png", width = 5, height = 8*(628/1200), dpi = 320)

## region data ----
# idph_region6 <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=6&daysIncluded=0",
#                             format = "csv") %>%
#   filter(RegionID == 6) %>%
#   mutate(Date = mdy_hms(ReportDate)) %>%
#   mutate(avgnewcases = rollmean(PositiveTests, k = 7, 
#                                 fill = NA, align = "right"))
# idph_region6_cases_hospital <- idph_region6 %>%
#   select(Date,avgnewcases,COVIDHospitalBedsInUse) %>%
#   mutate("Average New Cases" = avgnewcases) %>%
#   mutate("Hospital Beds in Use for COVID-19" = COVIDHospitalBedsInUse) %>%
#   select(Date,"Hospital Beds in Use for COVID-19","Average New Cases") %>%
#   pivot_longer(cols = c("Hospital Beds in Use for COVID-19",
#                         "Average New Cases"),
#                values_to = "value",
#                names_to = "name") %>%
#   mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
#                             mday(Date))) 

## facet chart of region cases and hospital beds in use ----
# ggplot(idph_region6_cases_hospital,
#        aes(x = as.Date(Date),
#            y = value,
#            colour = name)) +
#   geom_line() +
#   facet_wrap(~ name, scales = "free_y",
#              ncol = 1) +
#   labs(caption = paste("Source: IDPH. Data updated",
#                        tail(idph_region6_cases_hospital$short_date,1))) +
#   xlab(NULL) +
#   ylab(NULL) +
#   scale_x_date(expand = c(0,0)) +
#   scale_y_continuous(labels = label_comma(accuracy = 1),
#                      position = "right",
#                      expand = expansion(mult = c(0,.05))
#   ) +
#   expand_limits(y = 0) +
#   scale_colour_manual(guide = FALSE,
#                       values = c("#B45F06",
#                                  "#d90000","#674EA7","#674EA7")) +
#   theme(#text = element_text(family = "Gill Sans"),
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 8),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(colour = "grey93"),
#         strip.text = element_text(size = 11),
#         strip.background = element_blank(),
#         plot.caption = element_text(colour = "grey40")#,
#         #plot.title = element_text(size = 18, family = "Gill Sans")
#         )
# 
# ggsave("gh_action/region_Cases_Hospital.png", 
#        width = 5, height = 8*(628/1200), dpi = 320)

# idph champaign county cases ----
champaignpop <- 209983

idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                                    format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(population = champaignpop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(monthlydead = rollmean(new_deaths, k = 31, 
                                   fill = NA, align = "right")*31)  %>%
  mutate(Date = ymd_hms(reportDate)) 

idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

idph_cases_vax <- full_join(idph_cases_champaign, idph_vax_champaign) %>%
  select(Date, PersonsFullyVaccinated, AdministeredCountRollAvg,
         monthlydead, avg_new_cases)

idph_cases_vax_longer <- idph_cases_vax %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode(names, 
                           "PersonsFullyVaccinated" = "3. People Fully Vaccinated",
                           "avg_new_cases" = "1. Average New Cases",
                           "monthlydead" = "2. Deaths in Past Month",
                           "AdministeredCountRollAvg" = "4. Average New Vaccine Doses"))  %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date)))


## plot ----
ggplot(idph_cases_vax_longer,
       aes(x = as.Date(Date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(#title = "Metrics Since Vaccinations Began Dec. 16",
       caption = paste("Source: IDPH. Data updated",
                       tail(idph_cases_vax_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = FALSE,
                      values = c("#B45F06","#d90000","#674EA7","#674EA7")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))
ggsave("gh_action/Champaign_facet.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# idph Illinois cases ----
idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Illinois",
                                    format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(population = illinoispop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(monthlydead = rollmean(new_deaths, k = 7, 
                                fill = NA, align = "right"))  %>%
  mutate(Date = ymd_hms(reportDate)) 

idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

idph_cases_vax <- full_join(idph_cases_champaign, idph_vax_champaign) %>%
  select(Date, PersonsFullyVaccinated, AdministeredCountRollAvg,
         monthlydead, avg_new_cases)

idph_cases_vax_longer <- idph_cases_vax %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode(names, 
                        "PersonsFullyVaccinated" = "3. People Fully Vaccinated",
                        "avg_new_cases" = "1. Average New Cases",
                        "monthlydead" = "2. Average New Deaths",
                        "AdministeredCountRollAvg" = "4. Average New Vaccine Doses"))  %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date)))


## plot ----
ggplot(idph_cases_vax_longer,
       aes(x = as.Date(Date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(#title = "Metrics Since Vaccinations Began Dec. 16",
       caption = paste("Source: IDPH. Data updated",
                       tail(idph_cases_vax_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = FALSE,
                      values = c("#B45F06","#d90000","#674EA7","#674EA7")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))
ggsave("gh_action/IL_facet.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# us facet ----

## get data ----
### cases ----
jhu_new_cases_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_cases.csv"
jhu_new_cases <- rio::import(jhu_new_cases_url, format = "csv") %>%
  select(date,"United States") %>%
  rename(new_cases = "United States") %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right"))
  

### deaths ----
jhu_new_deaths_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_deaths.csv"
jhu_new_deaths <- rio::import(jhu_new_deaths_url, format = "csv") %>%
  select(date,"United States") %>%
  rename(new_deaths = "United States") %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                  fill = NA, align = "right"))


### vaccines ----
owid_vaccines_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv"
owid_vaccines <- rio::import(owid_vaccines_url, format = "csv") %>%
  filter(iso_code == "USA") %>%
  select(date, people_fully_vaccinated,daily_vaccinations)

### combined
us_data <- full_join(jhu_new_cases, jhu_new_deaths) %>%
  full_join(owid_vaccines)
us_data_longer <- us_data %>%
  select(date, people_fully_vaccinated, avg_new_cases, avg_new_deaths,
         daily_vaccinations) %>%
  pivot_longer(!date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode(names, 
                        "people_fully_vaccinated" = "3. People Fully Vaccinated",
                        "avg_new_cases" = "1. Average New Cases",
                        "avg_new_deaths" = "2. Average New Deaths",
                        "daily_vaccinations" = "4. Average New Vaccine Doses"))  %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date)))

## plot ----
ggplot(us_data_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(#title = "Metrics Since Vaccinations Began Dec. 16",
    caption = paste("Source: Our World in Data and JHU CSSE COVID-19 Data. Data updated",
                    tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = FALSE,
                      values = c("#B45F06","#d90000","#674EA7","#674EA7")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("gh_action/US_facet.png", 
       width = 8, height = 8*(628/1200), dpi = 320)


# vax comparison chart ----

## set population variables ----
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



## import and clean data ----

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
  full_join(vax_mclean) %>%
  full_join(vax_coles) %>%
  full_join(vax_illinois) %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PercentDose1 = PersonsDose1/population) %>%
  mutate(PercentOnlyDose1 = PercentDose1 - PctVaccinatedPopulation) %>%
  mutate(New_doses_per_100K = (AdministeredCountRollAvg/population)*100000) %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date)))


## plot ----

last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1)) %>%
  arrange(desc(PctVaccinatedPopulation))
#topcounty <- head(last_vax_nearby$CountyName,1)

ggplot(last_vax_nearby, aes(y = reorder(CountyName,
                                        PctVaccinatedPopulation))) +
  geom_segment(aes(x = PctVaccinatedPopulation, # first line segment to dose2
                   yend = CountyName), 
               xend = 0, 
               colour = "#674EA7",
               size = 7) + 
geom_text(data = last_vax_nearby,
          aes(x = PctVaccinatedPopulation,
              label = percent(PctVaccinatedPopulation, .1)),
          hjust = 1.1,
          size = 3.5,
          colour = "white") +
  # geom_text(data = filter(last_vax_nearby, # label for top county, dose2
  #                         CountyName == topcounty),
  #           aes(x = PctVaccinatedPopulation,
  #               label = percent(PctVaccinatedPopulation, .1)),
  #           hjust = 1.1,
  #           size = 3.5,
  #           colour = "white") +
  scale_x_continuous(labels = percent,
                     limits = c(0,1),
                     expand = expansion(mult = c(0,.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Total Population Fully Vaccinated in Nearby Counties",
       caption =  paste("Source: IDPH. Data updated",
                        tail(last_vax_nearby$short_date,1)))+
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        plot.caption = element_text(colour = "grey40")) 

ggsave("gh_action/nearby_fully_vaccinated.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
