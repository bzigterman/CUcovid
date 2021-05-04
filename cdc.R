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
                            mday(date)))


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

# state cdc map facet
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
          mapping = aes(fill = Series_Complete_65PlusPop_Pct/100,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_gradient(low = "#F3F1F8",
                      high = "#674EA7",
                      labels = scales::label_percent(accuracy = 1.)) +
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
          mapping = aes(fill = Series_Complete_18PlusPop_Pct/100,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_gradient(low = "#F3F1F8",
                      high = "#674EA7",
                      labels = scales::label_percent(accuracy = 1.)) +
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
          mapping = aes(fill = Series_Complete_Pop_Pct/100,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_gradient(low = "#F3F1F8",
                      high = "#674EA7",
                      labels = scales::label_percent(accuracy = 1.)) +
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
                            mday(date)))
  

cdc_cases_merged <- merge(cdc_cases,
                          il_counties_clean,
                          by = "GEOID")

cdc_cases_map <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = cases_per_100K_7_day_count_change/7,
                        geometry = geometry),
          #crs = 4326,
          #crs = "NAD83",
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_gradient(low = "#F7EDE3",
                      high = "#B45F06",
                      labels = scales::comma) +
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
