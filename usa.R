# get shapefiles ----
library(usmap)
# uscounties <- us_map(regions = "counties") %>%
#   mutate(GEOID = fips)

# uscounties <- get_acs(geography = "county", 
#                       variables = "B19013_001", geometry = TRUE)


# get data and merge with shapefiles ----
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
               breaks = 5,
               dig.lab = 2,
               include.lowest = TRUE
               #labels = c(paste(
               #labels = quantile(Series_Complete_Pop_Pct)/100)
           )) %>%
  mutate(total_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                 replacement = "", 
                                 total_class)) %>%
  separate(col = total_class_temp, into = c("lwr", "upr")) %>%
  mutate(total_class_new = paste(lwr,"–", upr,"%", sep = "")) %>%
  mutate(adult_class = 
           cut(x = Series_Complete_18PlusPop_Pct,
               breaks = 5,
               dig.lab = 2,
               include.lowest = TRUE
               #labels = c(paste(
               #labels = quantile(Series_Complete_Pop_Pct)/100)
           )) %>%
  mutate(adult_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                 replacement = "", 
                                 adult_class)) %>%
  separate(col = adult_class_temp, into = c("lwr", "upr")) %>%
  mutate(adult_class_new = paste(lwr,"–", upr,"%", sep = "")) %>%
  mutate(senior_class = 
           cut(x = Series_Complete_65PlusPop_Pct,
               breaks = 5,
               dig.lab = 2,
               include.lowest = TRUE
           )) %>%
  mutate(senior_class_temp = gsub(pattern = "\\(|\\[|\\)|\\]", 
                                  replacement = "", 
                                  senior_class)) %>%
  separate(col = senior_class_temp, into = c("lwr", "upr")) %>%
  mutate(senior_class_new = paste(lwr,"–",upr,"%", sep = ""))

# usa_vaccines_geo_merged <- merge(usa_county_vaccine,
#                                  uscounties,
#                                  by = "GEOID")  


# make map ----
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

plot_usmap(data = usa_county_vaccine, values = "Series_Complete_Pop_Pct",
           size = .01) +
  scale_fill_gradient(low = "#F3F1F8",
                      high = "#674EA7"#,
                      #labels = scales::label_percent(accuracy = 1.)
                      ) +
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
