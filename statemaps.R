
# getshapefiles ready ----

# chicago_outline <- st_read(dsn = "Chicago",package = "sf")
# ?st_read
# ?st_drivers
# #chicago_outline <- system.file(dsn = "Chicago",package = "sf")
# chicago_outline <- st_read(system.file("Chicago",package = "sf"),
#                            sf_column_name = thegeom)
chicago_outline <- read_sf(dsn = "Chicago")

#?read_sf
# chicago_outline <-rio::import("https://data.cityofchicago.org/resource/qqq8-j68g.json",
#             format = "json") 
# 

il_counties_clean <- il_counties %>%
  mutate(variable = NULL) %>%
  mutate(estimate = NULL) %>%
  mutate(moe = NULL)


chicago_outline <- chicago_outline %>%
  transmute(NAME = name) %>%
  mutate(GEOID = "606060606") %>%
  mutate(objectid = NULL) %>%
  mutate(shape_area = NULL) %>%
  mutate(shape_len = NULL) %>%
  st_transform("NAD83")

cook_county <- il_counties_clean %>%
  filter(GEOID == 17031) 

cook_county_no_chicago <- st_difference(cook_county, chicago_outline) %>%
  mutate(NAME.1 = NULL) %>%
  mutate(GEOID.1 = NULL)

cook_with_chicago <- rbind(cook_county_no_chicago,chicago_outline)

il_counties_no_cook <- il_counties_clean %>%
  filter(GEOID != 17031) 

# 
il_counties_with_chicago <- rbind(cook_with_chicago, il_counties_no_cook)
#plot(il_counties_with_chicago)
#?st_combine
# ?full_join
# ?st_crs
# st_crs(il_counties)
# st_crs(chicago_outline)
# plot(il_counties_chicago)
# ?st_join

# get data ----

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

state_rates_with_chicago <- rio::import(ratesurl,
                                        format = "csv") %>%
  filter(County == "Chicago") %>%
  mutate(GEOID = "606060606") %>%
  full_join(state_rates_csv)

state_rates_merged <- merge(il_counties_with_chicago, 
                            state_rates_with_chicago,
                            by = "GEOID")

state_vax_clean <- state_vax_map_csv %>%
  filter(CountyName != "Unknown") %>%
  filter(CountyName != "Chicago") %>%
  filter(CountyName != "Illinois") %>%
  filter(CountyName != "Out Of State") %>%
  mutate(GEOID = fips("IL", county = CountyName))

state_vax_with_chicago <- state_vax_map_csv %>%
  filter(CountyName == "Chicago") %>%
  mutate(GEOID = "606060606") %>%
  full_join(state_vax_clean)


state_vax_merged <- merge(il_counties_with_chicago, 
                          state_vax_with_chicago,
                          by = "GEOID")

il_zip_cases <- rio::import(zipurl,
                            format = "csv") 


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
        #legend.position = c(.1,.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
state_total_case_rate_map

# total death rate plot ----
state_total_death_rate_map <- ggplot(data = state_rates_merged) + 
  geom_sf(data = state_rates_merged,
          mapping = aes(fill = deaths_rate_per_10k),
          # color = "grey",
          size = .25) +
  # scale_fill_gradientn(colours = plasma(100,direction = 1),
  #                      labels = scales::comma) +
  scale_fill_gradient(low = "#FADBDB",
                      high = "#d90000",
                      labels = scales::comma) +
  labs(title = "Total Deaths per 100,000 Residents",
      # subtitle="Per 100,000 residents",
       caption =  "Source: Illinois Department of Public Health",
       fill = NULL)+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "left",
        panel.grid.major = element_blank(),  
        #legend.position = c(.1,.9),
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
   # scale_fill_gradientn(colours = c("#e5c7ff",
   #                                  "#9e72c4",
   #                                  "#703e9c",
   #                                  "#6b2e8f",
   #                                  "#4d166e"),
   #                      labels = scales::percent) +
  scale_fill_gradient(low = "#EEEBF5",
                      high = "#674EA7",
                      labels = scales::percent) +
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
state_fully_vax_map
#library(viridisLite)
state_total_death_rate_map + 
  labs(title = "Total Deaths per 100,000 Residents",
      # subtitle="Per 100,000 residents",
       caption =  "") +
  theme(plot.title = element_text(size = 10, family = "Oswald"),
        legend.position = "left") +
  state_fully_vax_map +
  theme(plot.title = element_text(size = 10, family = "Oswald"),
        legend.position = "right") +
  
ggsave("map/IL_deaths_vax_map.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("IL_deaths_vax_map.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)
