library(maps)
library(sf)
library(tidyverse)
library(usmap)
library(tmap)
library(tidycensus)
library(ggmap)
library(ggspatial)
library(patchwork)
options(tigris_use_cache = TRUE)



# il_counties <- st_read(system.file("map/tl_2016_17_cousub.shp", package="sf"))
# il_counties <- system.file("map/tl_2016_17_cousub.shp", package="sf")
# il_counties <- read_sf(dsn = "map")
# ?st_drivers
# ?read_sf
# 
# plot(st_geometry(il_counties))
# plot(il_counties)
# plot(il_counties, max.plot = 14)
# plot(il_counties$COUNTYFP)
# plot(il_counties["COUNTYFP"])

# ggplot() + geom_sf(data = il_counties)
# qtm(il_counties)
# ?geom_sf
# names(il_counties)



# champaign_county <- get_acs(state = "IL", county = "Champaign", geography = "tract", 
#                   variables = "B19013_001", geometry = TRUE)
# champaign_county %>%
#   ggplot(aes(fill = estimate)) + 
#   geom_sf(color = NA) + 
#   #coord_sf(crs = 26911) + 
#   scale_fill_viridis_c(option = "magma") 

# get county shapefile ----
il_counties <- get_acs(state = "IL", geography = "county", 
                       variables = "B19013_001", geometry = TRUE)

# il_counties %>%
#   ggplot(aes(fill = estimate)) + 
#   geom_sf(color = NA) 
  #coord_sf(crs = 26911) + 
  #scale_fill_viridis_c(option = "magma") 

#st_write(il_counties, "il_counties.shp")

# names(last_vax_nearby)
# names(il_counties)

# combine vaccine data and shapefile ----
last_vax_nearby_map <- last_vax_nearby %>%
  mutate(GEOID = fips("IL", county = CountyName))

nearby_vax_merged <- merge(il_counties, last_vax_nearby_map,
                           by = "GEOID") #%>%
  # mutate(full_pct_cat = cut(PctVaccinatedPopulation, 
  #                           breaks=c(-Inf, 0.2, 0.22,.24, Inf), 
  #                           labels=c("Less than 20%",
  #                                    "20-22%",
  #                                    "22-24%",
  #                                    "Greater than 24%")))
         


         



#names(nearby_vax_merged)

# make nearby cities shapefile ----
nearby_cities <- read_csv("map/nearby_cities.csv")
nearby_cities_sf <- st_as_sf(nearby_cities, coords = c("lng", "lat"), remove = FALSE, 
                     crs = 4326, agr = "constant")

# ggplot(data = nearby_cities_sf) +
#   #geom_sf() + 
#   geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city), 
#             size = 3.9, col = "black", fontface = "bold") 

# plot nearby fully vaccinated ----
pct_fully_vax_nearby <- ggplot(data = nearby_vax_merged) + 
  geom_sf(data = nearby_vax_merged,
          mapping = aes(fill = PctVaccinatedPopulation),
          # color = "grey",
          size = .25) +
  # scale_fill_manual(values = colorRampPalette(c("#EEEBF5","#674EA7"))(4),
  #                   #low = "#EEEBF5",
  #                   # high = "#674EA7",
  #                   #labels = percent,
  #                   guide = guide_legend(title = NULL)) +
  scale_fill_gradient(low = "#EEEBF5",
                      high = "#674EA7",
                      labels = percent,
                      #guide = guide_legend(title = "% Fully Vaccinated")
                      ) +
  geom_sf(data = nearby_cities_sf, size = .5) +
  geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city),
            size = 2.9, col = "black", family = "Barlow",
            nudge_y = .05,
            nudge_x = -.02) +
  labs(title = "Percent of Population Fully Vaccinated",
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
        legend.position = c(.1,.885),
        #legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        #legend.key.size = .9,
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
pct_fully_vax_nearby

ggsave("vax/pct_fully_vax_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("map/pct_fully_vax_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("pct_fully_vax_nearby.png", 
       path = "../bzigterman.github.io/images/",
       width = 5, height = 5, dpi = 320)

# plot nearby with at least one dose ----
pct_partial_vax_nearby <- ggplot(data = nearby_vax_merged) + 
  geom_sf(data = nearby_vax_merged,
          mapping = aes(fill = PercentDose1),
          # color = "grey",
          size = .25) +
  scale_fill_gradient(low = "#EEEBF5",
                      high = "#674EA7",
                      labels = percent,
                      #guide = guide_legend(title = NULL)
                      ) +
  geom_sf(data = nearby_cities_sf, size = .5) +
  geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city), 
            size = 2.9, col = "black", family = "Barlow",
            nudge_y = .05,
            nudge_x = -.02) +
  labs(title = "Percent of Population With At Least One Dose",
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
        legend.position = c(.1,.885),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5, "cm"),
        panel.background = element_blank(),
        #legend.text = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 16, family = "Oswald")) 
pct_partial_vax_nearby

#ggsave("vax/pct_partial_vax_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("map/pct_partial_vax_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("pct_partial_vax_nearby.png", 
       path = "../bzigterman.github.io/images/",
       width = 5, height = 5, dpi = 320)



# nearbyggmap <- get_stamenmap(bbox = c(left = -89.6732, 
#                                       bottom = 39.35, 
#                                       right = -87.5, 
#                                       top = 41.1),
#                              zoom = 9,
#                              maptype = "terrain-background")
# ggmap(nearbyggmap)
# 
# ggmap(nearbyggmap) +
#   geom_sf(data = nearby_vax_merged,
#           aes(fill = PctVaccinatedPopulation),
#           inherit.aes = FALSE,
#           color = NA) + 
#   scale_fill_gradient(low = "#d8cee8",
#                       high = "#674EA7")
  #scale_fill_brewer(palette = "OrRd")

 # coord_sf(crs = st_crs(4326))



# ggplot() +
#   geom_sf(data = nearby_vax_merged, aes(fill = PctVaccinatedPopulation)) +
#   #geom_sf(data = nearby_cities_sf) + 
#   geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city), 
#             size = 3.9, col = "black", fontface = "bold")
# 
#   ggplot(aes(fill = PctVaccinatedPopulation)) + 
#   geom_sf(color = NA) +
#   scale_fill_gradient(low = "#d8cee8",
#                       high = "#674EA7",
#                       labels = percent,
#                       guide = guide_legend(title = NULL)) +
#   labs(title = "Percent of Total Population Vaccinated",
#        caption =  "Source: Illinois Department of Public Health")+
#   theme_minimal() +
#   theme(text = element_text(family = "Barlow"),
#         axis.text = element_blank(),
#         axis.line.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         #panel.grid.major.x = element_line(colour = "grey93"),
#         #legend.position = "none",
#         panel.grid.major = element_blank(),  
#         legend.position = c(.1,.9),
#         plot.caption = element_text(colour = "grey40"),
#         plot.title = element_text(size = 22, family = "Oswald")) 


# combine case data and shapefile ---- 
#last_cases_nearby
last_cases_nearby_map <- last_cases_nearby %>%
  mutate(GEOID = fips("IL", county = CountyName))

nearby_cases_merged <- merge(il_counties, last_cases_nearby_map,
                           by = "GEOID")

# plot nearby cases ----
case_rate_nearby <- ggplot(data = nearby_cases_merged) + 
  geom_sf(data = nearby_cases_merged,
          mapping = aes(fill = new_case_rate),
          # color = "grey",
          size = .25) +
  scale_fill_gradient(low = "#F7EDE3",
                      high = "#B45F06",
                      # labels = percent,
                      # guide = guide_legend(title = NULL)
                      ) +
  geom_sf(data = nearby_cities_sf, size = .5) +
  geom_text(data = nearby_cities_sf, aes(x = lng, y = lat, label = city), 
            size = 2.9, col = "black", family = "Barlow",
            nudge_y = .05,
            nudge_x = -.02) +
  # geom_text(data = last_cases_nearby_map,
  #           aes(x = latitude, y = longitude, label = CountyName),
  #           size = 2.9,
  #           col = "black",
  #           family = "Barlow") +
  labs(title = "New Cases per 100,000 Residents",
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
case_rate_nearby

ggsave("region/case_rate_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("map/case_rate_nearby.png", width = 5, height = 5, dpi = 320)
ggsave("case_rate_nearby.png", 
       path = "../bzigterman.github.io/images/",
       width = 5, height = 5, dpi = 320)

pct_fully_vax_nearby + 
  theme(plot.title = element_text(size = 14, family = "Oswald"),
       # legend.text = element_text(size = 10),
        legend.position = "left") +
  case_rate_nearby +
  theme(plot.title = element_text(size = 14, family = "Oswald"),
      #  legend.text = element_text(size = 10),
        legend.position = "right") 

ggsave("map/combined_case_vax.png", 
       width = 8, 
       height = 8*(628/1200), 
       dpi = 320)
