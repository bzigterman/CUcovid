# get shapefiles ----
library(usmap)
# uscounties <- us_map(regions = "counties") %>%
#   mutate(GEOID = fips)

# uscounties <- get_acs(geography = "county", 
#                       variables = "B19013_001", geometry = TRUE)


# get vaccine data and merge with shapefiles ----
usa_county_vaccine_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
usa_county_vaccine <- rio::import(usa_county_vaccine_url,
                                  format = "json")
usa_county_vaccine <- usa_county_vaccine$vaccination_county_condensed_data 
usa_county_vaccine <- usa_county_vaccine %>%
  #filter(StateName == "Illinois") %>%
  filter(County != "Unknown County") %>%
  #filter(StateName != "Alaska") %>%
  #filter(StateName != "Hawaii") %>%
  filter(StateName != "Puerto Rico") %>%
  mutate(GEOID = FIPS) %>%
  mutate(fips = FIPS) %>%
  mutate(date = ymd(as_date(Date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(total_class = 
           cut(x = Series_Complete_Pop_Pct,
               breaks = c(0,20,40,60,80,100),
               labels = c("0-20%","20-40%","40-60%","60-80%","80-100%"),
               #dig.lab = 2,
               include.lowest = TRUE
               #labels = c(paste(
               #labels = quantile(Series_Complete_Pop_Pct)/100)
           )) #%>%
  #drop_na(total_class)

  
# usa_vaccines_geo_merged <- merge(usa_county_vaccine,
#                                  uscounties,
#                                  by = "GEOID")  


# make vaccine map ----
# usa_total_vax <- ggplot(data = usa_vaccines_geo_merged) +
#   geom_sf(data = usa_vaccines_geo_merged,
#           mapping = aes(fill = Series_Complete_Pop_Pct/100,
#                         geometry = geometry),
#           #crs = 4326,
#           #crs = "NAD83",
#           size = .01,
#           colour = "grey90") +
#   coord_sf(crs = st_crs(4326)) +
#   #coord_map(projection = "mercator") +
#   # scale_fill_brewer(
#   #   palette = "Purples",
#   #   direction = 1) +
#   scale_fill_gradient(low = "#F3F1F8",
#                       high = "#674EA7",
#                       labels = scales::label_percent(accuracy = 1.)) +
#   labs(title = "Percent Fully Vaccinated",
#        caption =  paste("Source: CDC. Data last updated",
#                         tail(usa_vaccines_geo_merged$short_date,1)),
#        fill = NULL)+
#   #theme_minimal() +
#   theme(text = element_text(family = "Barlow"),
#         axis.text = element_blank(),
#         axis.line.x = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         #panel.grid.major.x = element_line(colour = "grey93"),
#         legend.position = "right",
#         panel.grid.major = element_blank(),  
#         #legend.position = c(.1,.9),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.key.size = unit(.5, "cm"),
#         panel.background = element_blank(),
#         #legend.text = element_text(size = 13),
#         plot.caption = element_text(colour = "grey40"),
#         plot.title = element_text(size = 16, family = "Oswald")) 
# usa_total_vax
# ggsave("map/usa_vax_total.png", width = 8, height = 8*(628/1200), dpi = 320)
# ggsave("usa_vax_total.png", 
#        path = "../bzigterman.github.io/images/",
#        width = 8, height = 8*(628/1200), dpi = 320)

plot_usmap(data = usa_county_vaccine, values = "total_class",
           size = .01) +
  scale_fill_brewer(
    palette = "Purples",
    direction = 1) +
  # scale_fill_gradient(low = "#F3F1F8",
  #                     high = "#674EA7"#,
  #                     #labels = scales::label_percent(accuracy = 1.)
  # ) +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Data last updated",
                        tail(usa_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
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
        plot.title = element_text(size = 16, family = "Oswald")) 

ggsave("map/usa_vax_total.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("usa_vax_total.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# get case data and merge with shapefiles ----
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
                               


# make case map ----
plot_usmap(data = usa_cases, values = "new_cases_class",
           size = .01) +
  scale_fill_brewer(
    palette = "Oranges",
    direction = 1) +
  labs(title = "Average New Cases per 100,000 Residents",
       caption =  paste("Source: CDC. Data last updated",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
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
        plot.title = element_text(size = 16, family = "Oswald")) 

ggsave("map/usa_cases.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("usa_cases.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# cdc transmission level map ----
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
  theme(text = element_text(family = "Barlow"),
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
        plot.title = element_text(size = 16, family = "Oswald")) 

ggsave("map/usa_transmission.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("usa_transmission.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

