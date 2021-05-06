library(RColorBrewer)


# cdc vaccines ----

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
               breaks = 5,
               dig.lab = 2
               #labels = c(paste(
               #labels = quantile(Series_Complete_Pop_Pct)/100)
           )) %>%
  mutate(total_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                 replacement = "", 
                                 total_class)) %>%
  separate(col = total_class_temp, into = c("lwr", "upr")) %>%
  mutate(total_class_new = paste(lwr," - ", upr,"%", sep = "")) %>%
  mutate(adult_class = 
           cut(x = Series_Complete_18PlusPop_Pct,
               breaks = 5,
               dig.lab = 2
               #labels = c(paste(
               #labels = quantile(Series_Complete_Pop_Pct)/100)
           )) %>%
  mutate(adult_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                 replacement = "", 
                                 adult_class)) %>%
  separate(col = adult_class_temp, into = c("lwr", "upr")) %>%
  mutate(adult_class_new = paste(lwr," - ", upr,"%", sep = "")) %>%
  mutate(senior_class = 
           cut(x = Series_Complete_65PlusPop_Pct,
               breaks = 5,
               dig.lab = 2
           )) %>%
  mutate(senior_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                  replacement = "", 
                                  senior_class)) %>%
  separate(col = senior_class_temp, into = c("lwr", "upr")) %>%
  mutate(senior_class_new = paste(lwr," - ",upr,"%", sep = ""))

# cut(x = cdc_county_vaccine$Series_Complete_65PlusPop_Pct,
#     breaks = 5,
#     dig.lab = 2)


# labels <- levels(cut_interval(
#   x = cdc_county_vaccine$Series_Complete_Pop_Pct,
#   n = 5,
#   dig.lab = 1))
# labels

# cdc_county_vaccine$
# cut_interval(
# ?cut
# quantile(cdc_county_vaccine$Series_Complete_Pop_Pct)

cdc_vaccines_geo_merged <- merge(cdc_county_vaccine,
                                 il_counties_clean,
                                 by = "GEOID")  
cdc_vax_merged_longer <- cdc_vaccines_geo_merged %>%
  select(County, 
         Series_Complete_Pop_Pct,
         Series_Complete_18PlusPop_Pct,
         Series_Complete_65PlusPop_Pct,
         geometry) %>%
  mutate(Total = Series_Complete_Pop_Pct) %>%
  mutate(Seniors = Series_Complete_65PlusPop_Pct) %>%
  mutate(Adults = Series_Complete_18PlusPop_Pct) %>%
  pivot_longer(cols = c(Total,
                        Adults,
                        Seniors),
               names_to = "age",
               values_to = "percent") 

# state cdc map facet ----
cdc_vax_map <- ggplot(data = cdc_vax_merged_longer) +
  geom_sf(data = cdc_vax_merged_longer,
          mapping = aes(fill = percent/100,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_gradient(low = "#F3F1F8",
                      high = "#674EA7",
                      labels = scales::percent) +
  labs(title = "Percent Vaccinated",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  facet_wrap(~ age) +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 

cdc_vax_map

# pct 65+ map ----
cdc_total_vax_65 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = senior_class_new,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  # scale_fill_gradient(low = "#F3F1F8",
  #                     high = "#674EA7",
  #                     labels = scales::label_percent(accuracy = 1.)) +
  labs(title = "Percent Fully Vaccinated 65+",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
cdc_total_vax_65
ggsave("map/CDC_vax_65.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_vax_65.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)


# pct 18+ ----

cdc_total_vax_18 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = adult_class_new,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  # scale_fill_gradient(low = "#F3F1F8",
  #                     high = "#674EA7",
  #                     labels = scales::label_percent(accuracy = 1.)) +
  labs(title = "Percent Fully Vaccinated 18+",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
cdc_total_vax_18
ggsave("map/CDC_vax_18.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_vax_18.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)


# pct total pop vaccinated map ----
cdc_total_vax <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = total_class_new,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  # scale_fill_gradient(low = "#F3F1F8",
  #                     high = "#674EA7",
  #                     labels = scales::label_percent(accuracy = 1.)) +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
cdc_total_vax
ggsave("map/CDC_vax_total.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_vax_total.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# combined vax maps ----
cdc_total_vax + 
  labs(title = "Percent Fully Vaccinated",
       subtitle = "Total Population",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12, 
                                  hjust = .6)) +
  cdc_total_vax_18 +
  labs(title = NULL,
       subtitle = "18 and older",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 12, 
                                     hjust = .6)) +
  cdc_total_vax_65 +
  labs(title = NULL,
       subtitle = "65 and older",
       #caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 12, 
                                     hjust = .6))
  
ggsave("map/CDC_vax_combined.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("vax/CDC_vax_combined.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_vax_combined.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# cdc cases ----
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
               breaks = c(0,10,30,50,70,100,250,Inf),
               include.lowest = TRUE
               )) %>%
  mutate(new_cases_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                  replacement = "", 
                                  new_cases_class)) %>%
  separate(col = new_cases_class_temp, into = c("lwr", "upr")) %>%
  mutate(new_cases_class_new = paste(lwr," - ",upr, sep = ""))
  

cdc_cases_merged <- merge(cdc_cases,
                          il_counties_clean,
                          by = "GEOID")

cdc_cases_map <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = new_cases_class_new,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    palette = "Oranges",
    direction = 1) +
  # scale_fill_gradient(low = "#F7EDE3",
  #                     high = "#B45F06",
  #                     labels = scales::comma) +
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
cdc_cases_map
ggsave("map/CDC_cases_IL.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_cases_IL.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# combined cases and vax ----
cdc_cases_map + 
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       caption =  NULL,
       fill = NULL)+
  theme(plot.title = element_text(size = 12)#,
        #legend.position = "left"
  ) +
  cdc_total_vax +
  theme(plot.title = element_text(size = 12)) 
ggsave("map/CDC_cases_vax_IL.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("region/CDC_cases_vax_IL.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_cases_vax_IL.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# community transmission level ----
cdc_transmission <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = community_transmission_level,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  # scale_fill_manual(limits = c("low","moderate","substantial","high"),
  #                     values = c("lightgreen",
  #                              "yellow",
  #                              "orange",
  #                              "red")) +
  # scale_fill_viridis_d(limits = c("low","moderate","substantial","high"),
  #                      option = "inferno",
  #                      direction = -1) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "OrRd",
                    direction = 1) +
  # scale_fill_gradient(low = "#F7EDE3",
  #                     high = "#B45F06",
  #                     labels = scales::comma) +
  labs(title = "Community Transmission Levels",
       subtitle = "Community transmission levels are\ncalculated by the CDC based on\nnew cases per capita in the past week\nand test positivity.",
       caption =  paste("Source: CDC. Data last updated",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "right",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.caption.position = "plot",
        #plot.caption = element_text(hjust = 0),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
cdc_transmission 
ggsave("map/cdc_transmission.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("region/cdc_transmission.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("cdc_transmission.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)
