hospital_covid_url <- "https://healthdata.gov/resource/anag-cw7u.json?fips_code=17019"
hhs_covid_stats_url <- "https://healthdata.gov/resource/di4u-7yu6.json"

hospital_covid_stats <- rio::import(hospital_covid_url,
                                   format = "json") %>%
  mutate(Date = ymd_hms(collection_week)) %>%
  filter(total_adult_patients_hospitalized_confirmed_covid_7_day_avg > 0)

carle_stats <- hospital_covid_stats %>%
  filter(ccn == 140091)
osf_stats <- hospital_covid_stats %>%
  filter(ccn == 140113)

ggplot(data = carle_stats,
       aes(x = as.Date(Date), 
           y = as.double(total_adult_patients_hospitalized_confirmed_covid_7_day_avg),
           color = hospital_name,
           fill = hospital_name)) +
  geom_area(size =1,
            alpha = .25) +
  scale_y_continuous(labels = comma,
                     position = "right") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Carle Hospital Patients",
       caption = "U.S. Department of Health & Human Services") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

