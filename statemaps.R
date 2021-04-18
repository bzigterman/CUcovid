zipurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetZip?format=csv"
countysnapshoturl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyTestResults?format=csv"
hospitalizationurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetHospitalUtilizationResults?format=csv"

state_cases_map_csv <- rio::import(countysnapshoturl,
                             format = "csv") 


ratesurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyRates?format=csv"

state_rates_csv <- rio::import(ratesurl,
                               format = "csv") %>%
  filter(County != "Illinois") %>%
  filter(County != "Chicago") %>%
  mutate(GEOID = fips("IL", county = County))
state_rates_merged <- merge(il_counties, state_rates_csv,
                            by = "GEOID")

# total case rate plot ----
state_total_case_rate_map <- ggplot(data = state_rates_merged) + 
  geom_sf(data = state_rates_merged,
          mapping = aes(fill = confirmed_rate_per_100k),
          # color = "grey",
          size = .25) +
  scale_fill_gradient(low = "#F7EDE3",
                      high = "#B45F06",
                      labels = comma
                      # guide = guide_legend(title = NULL)
  ) +
  # geom_sf(data = nearby_cities_sf, size = .5) +
  # geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city), 
  #           size = 2.9, col = "black", family = "Barlow",
  #           nudge_y = .05,
  #           nudge_x = -.02) +
  # geom_text(data = last_cases_nearby_map,
  #           aes(x = latitude, y = longitude, label = CountyName),
  #           size = 2.9,
  #           col = "black",
  #           family = "Barlow") +
  labs(title = "Total Cases per 100,000 Residents",
       caption =  "Source: Illinois Department of Public Health",
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        #legend.position = "none",
        panel.grid.major = element_blank(),  
        legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
state_total_case_rate_map

# total case rate plot ----
state_total_death_rate_map <- ggplot(data = state_rates_merged) + 
  geom_sf(data = state_rates_merged,
          mapping = aes(fill = deaths_rate_per_10k),
          # color = "grey",
          size = .25) +
  scale_fill_gradient(low = "#FADBDB",
                      high = "#d90000",
                      labels = comma) +
  labs(title = "Total Deaths per 100,000 Residents",
       caption =  "Source: Illinois Department of Public Health",
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        #legend.position = "none",
        panel.grid.major = element_blank(),  
        legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
state_total_death_rate_map

