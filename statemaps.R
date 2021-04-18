zipurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetZip?format=csv"
countysnapshoturl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyTestResults?format=csv"
hospitalizationurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetHospitalUtilizationResults?format=csv"
countysnapshotvaxurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDVaccine/getVaccineAdministrationCurrent"

# ilzips <- get_acs(state = "IL", geography = "zcta", 
#                   variables = "B19013_001", geometry = TRUE)

state_cases_map_csv <- rio::import(countysnapshoturl,
                             format = "csv") 
state_vax_map_csv <- rio::import(countysnapshotvaxurl,
                                   format = "json") 
state_vax_map_csv <- state_vax_map_csv$VaccineAdministration

ratesurl <- "https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyRates?format=csv"

state_rates_csv <- rio::import(ratesurl,
                               format = "csv") %>%
  filter(County != "Illinois") %>%
  filter(County != "Chicago") %>%
  mutate(GEOID = fips("IL", county = County))
state_rates_merged <- merge(il_counties, state_rates_csv,
                            by = "GEOID")

state_vax_clean <- state_vax_map_csv %>%
  filter(CountyName != "Unknown") %>%
  filter(CountyName != "Chicago") %>%
  filter(CountyName != "Illinois") %>%
  filter(CountyName != "Out Of State") %>%
  mutate(GEOID = fips("IL", county = CountyName))
state_vax_merged <- merge(il_counties, state_vax_clean,
                          by = "GEOID")

il_zip_cases <- rio::import(zipurl,
                            format = "csv") 

# chicago_outline <- st_read(dsn = "Chicago",package = "sf")
# chicago_outline <- system.file(dsn = "Chicago",package = "sf")
# chicago_outline <- read_sf(dsn = "Chicago/geo_export_fed7fb1c-918b-490b-9aa3-4d19f22a3ca9.shp",package = "sf")
# ?read_sf
# chicago_outline <-rio::import("https://data.cityofchicago.org/resource/qqq8-j68g.json",
#             format = "json") 
# 
# chicago_outline <- chicago_outline %>%
#   mutate(NAME = name) %>%
#   mutate(geometry = the_geom)
# 
# il_counties_chicago <- full_join(chicago_outline, il_counties)
# ?full_join
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

# fully vaccinated plot ----
state_fully_vax_map <- ggplot(data = state_vax_merged) + 
  geom_sf(data = state_vax_merged,
          mapping = aes(fill = PctVaccinatedPopulation),
          # color = "grey",
          size = .25) +
  scale_fill_gradient(low = "#EEEBF5",
                      high = "#674EA7",
                      labels = percent) +
  labs(title = "Percent Fully Vaccinated",
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
state_fully_vax_map

state_total_death_rate_map + state_fully_vax_map
