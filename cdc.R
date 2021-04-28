cdc_county_vaccine_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
cdc_county_vaccine <- rio::import(cdc_county_vaccine_url,
                                  format = "json")
cdc_county_vaccine <- cdc_county_vaccine$vaccination_county_condensed_data 
cdc_county_vaccine <- cdc_county_vaccine %>%
  filter(StateName == "Illinois") %>%
  filter(County != "Unknown County") %>%
  mutate(GEOID = FIPS)


cdc_vaccines_geo_merged <- merge(cdc_county_vaccine,
                                 il_counties_clean,
                                 by = "GEOID")  

# pct 65+ map
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
                      labels = scales::percent) +
  labs(title = "Percent Fully Vaccinated 65+",
       caption =  "Source: CDC",
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


# pct total pop vaccinated map
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
                      labels = scales::percent) +
  labs(title = "Percent Fully Vaccinated",
       caption =  "Source: CDC",
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

cdc_total_vax + 
  labs(title = "Percent Fully Vaccinated",
       subtitle = "Total Population",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "left",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12, 
                                  hjust = .6)) +
  cdc_total_vax_65 +
  labs(title = NULL,
       subtitle = "65 and older",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 12, 
                                     hjust = .6))
  
ggsave("map/CDC_vax_combined.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("CDC_vax_combined.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

