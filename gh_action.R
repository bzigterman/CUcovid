library(tidyverse)
library(zoo)
library(scales)
library(rio)
library(cowplot)
library(httr)
library(usmap)
library(RColorBrewer)
library(lubridate)
library(sf)
library(patchwork)
options(tigris_use_cache = TRUE)

# maps ----
## us----
### vaccine data ----
usa_county_vaccine_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
usa_county_vaccine <- rio::import(usa_county_vaccine_url,
                                  format = "json")
usa_county_vaccine <- usa_county_vaccine$vaccination_county_condensed_data 
usa_county_vaccine <- usa_county_vaccine %>%
  filter(County != "Unknown County") %>%
  filter(StateName != "Puerto Rico") %>%
  mutate(GEOID = FIPS) %>%
  mutate(fips = FIPS) %>%
  mutate(date = ymd(as_date(Date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(total_class = 
           cut(x = Series_Complete_Pop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–90%","90–100%"),
               include.lowest = TRUE)) 

### vaccine map ----
us_vax_map <- plot_usmap(data = usa_county_vaccine, values = "total_class",
                         linewidth = .01) +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_county_vaccine$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
us_vax_map
#ggsave("gh_action/usa_vax_total.png", 
#      width = 8, height = 8*(628/1200), dpi = 320)


us_vax_map_mobile <- plot_usmap(data = usa_county_vaccine, values = "total_class",
                                linewidth = .01) +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_county_vaccine$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40"),
    plot.title = element_text(size = 18)
  ) 
us_vax_map_mobile

vax_freq <- usa_county_vaccine %>%
  count(total_class) %>%
  group_by(total_class) #%>%
#mutate(total_class_n = paste(total_class,"\n",n,sep=""))

scale <- ggplot(vax_freq, aes(x = total_class,
                              y = n,
                              color = total_class,
                              label = n,
                              fill = total_class)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 3,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80")  + 
  scale_color_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(levels(vax_freq$total_class))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),  
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.margin = margin(b = 150,
                         t= 5,
                         r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale

scale_mobile <- ggplot(vax_freq, aes(x = total_class,
                                     y = n,
                                     color = total_class,
                                     label = n,
                                     fill = total_class)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 5,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80")  + 
  scale_color_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(levels(vax_freq$total_class))) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),  
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    #plot.margin = margin(b = 150,
    #                    t= 5,
    #                   r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale_mobile

plot_grid(us_vax_map, scale,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_vax_total.png", bg = "white", 
       width = 8, height = 8*(628/1200), dpi = 320)

plot_grid(us_vax_map_mobile, scale_mobile,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_vax_total_mobile.png", bg = "white", 
       width = 8, height = 8*(628/1200), dpi = 320)

### us case data ----
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
  mutate(
    new_cases_class = cut(x = as.numeric(cases_per_100K_7_day_count_change)/7,
                          breaks = c(0,5,15,25,35,50,100,Inf),
                          labels = c("0–5","5–15","15–25",
                                     "25–35","35–50","50–100","100+"),
                          include.lowest = TRUE,
                          ordered_result = TRUE))



### transmission map ----
us_community_levels <- usa_cases %>%
  mutate(community_level = case_when(
    CCL_community_burden_level_integer == 0 ~ "Low",
    CCL_community_burden_level_integer == 1 ~ "Medium",
    CCL_community_burden_level_integer == 2 ~ "High")) %>%
  mutate(date = ymd(as_date(CCL_report_date))) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date)))

community_level_map <- plot_usmap(data = us_community_levels, values = "community_level",
                                  linewidth = .01) +
  scale_fill_brewer(limits = c("Low","Medium","High"),
                    palette = "Spectral",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Levels",
       caption =  paste("Source: CDC. Latest data:",
                        tail(us_community_levels$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
community_level_map

community_level_freq <- us_community_levels %>%
  count(community_level) %>%
  group_by(community_level)

scale <- ggplot(community_level_freq, aes(x = community_level,
                                          y = n,
                                          label = n,
                                          color = community_level,
                                          fill = community_level)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 3,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(limits = c("Low","Medium","High"),
                    palette = "Spectral",
                    direction = -1,
                    na.value = "grey80")  + 
  scale_color_brewer(limits = c("Low","Medium","High"),
                     palette = "Spectral",
                     direction = -1,
                     na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(c("Low","Medium","High"))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.margin = margin(b = 230,
                         t= 5,
                         r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale

plot_grid(community_level_map, scale,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_community_levels.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)

community_level_map_mobile <- plot_usmap(data = us_community_levels, 
                                         values = "community_level",
                                         linewidth = .01) +
  scale_fill_brewer(limits = c("Low","Medium","High"),
                    palette = "Spectral",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Levels",
       caption =  paste("Source: CDC. Latest data:",
                        tail(us_community_levels$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40"),
    plot.title = element_text(size = 18)
  ) 
community_level_map_mobile

scale_mobile <- ggplot(community_level_freq, aes(x = community_level,
                                                 y = n,
                                                 label = n,
                                                 color = community_level,
                                                 fill = community_level)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 5,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(limits = c("Low","Medium","High"),
                    palette = "Spectral",
                    direction = -1,
                    na.value = "grey80")  + 
  scale_color_brewer(limits = c("Low","Medium","High"),
                     palette = "Spectral",
                     direction = -1,
                     na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(c("Low","Medium","High"))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    #plot.margin = margin(b = 230,
    #                    t= 5,
    #                   r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale_mobile

plot_grid(community_level_map_mobile, scale_mobile,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_community_level_mobile.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)

### community level map ----
transmission_map <- plot_usmap(data = usa_cases, values = "community_transmission_level",
                               linewidth = .01) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "RdYlGn",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
transmission_map

transmission_freq <- usa_cases %>%
  count(community_transmission_level) %>%
  group_by(community_transmission_level)

scale <- ggplot(transmission_freq, aes(x = community_transmission_level,
                                       y = n,
                                       label = n,
                                       color = community_transmission_level,
                                       fill = community_transmission_level)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 3,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "RdYlGn",
                    direction = -1,
                    na.value = "grey80")  + 
  scale_color_brewer(limits = c("low","moderate","substantial","high"),
                     palette = "RdYlGn",
                     direction = -1,
                     na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(c("low","moderate","substantial","high"))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.margin = margin(b = 230,
                         t= 5,
                         r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale

plot_grid(transmission_map, scale,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_transmission.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)

transmission_map_mobile <- plot_usmap(data = usa_cases, 
                                      values = "community_transmission_level",
                                      linewidth = .01) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "RdYlGn",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40"),
    plot.title = element_text(size = 18)
  ) 
transmission_map_mobile

scale_mobile <- ggplot(transmission_freq, aes(x = community_transmission_level,
                                              y = n,
                                              label = n,
                                              color = community_transmission_level,
                                              fill = community_transmission_level)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 5,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "RdYlGn",
                    direction = -1,
                    na.value = "grey80")  + 
  scale_color_brewer(limits = c("low","moderate","substantial","high"),
                     palette = "RdYlGn",
                     direction = -1,
                     na.value = "grey80")  +  
  coord_flip() +
  scale_x_discrete(limits = rev(c("low","moderate","substantial","high"))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    #plot.margin = margin(b = 230,
    #                    t= 5,
    #                   r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale_mobile

plot_grid(transmission_map_mobile, scale_mobile,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_transmission_mobile.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)

### new cases map ----
cases_map <- plot_usmap(data = usa_cases, 
                        values = "new_cases_class",
                        linewidth = .01) +
  scale_fill_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Average New Cases per 100,000 Residents",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", 
                                   color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
cases_map

cases_freq <- usa_cases %>%
  count(new_cases_class) %>%
  group_by(new_cases_class)

scale <- ggplot(cases_freq, aes(x = new_cases_class,
                                y = n,
                                color = new_cases_class,
                                fill = new_cases_class,
                                label = n)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 3,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  scale_color_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(cases_freq$new_cases_class))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.margin = margin(b = 190,
                         t= 5,
                         r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale

plot_grid(cases_map, scale,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_new_cases.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)


cases_map_mobile <- plot_usmap(data = usa_cases, 
                               values = "new_cases_class",
                               linewidth = .01) +
  scale_fill_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Average New Cases per 100,000 Residents",
       caption =  paste("Source: CDC. Latest data:",
                        tail(usa_cases$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "none",
    legend.key.size = unit(.5, "cm"),
    plot.background = element_rect(fill = "white", 
                                   color = "white"),
    plot.caption = element_text(colour = "grey40"),
    plot.title = element_text(size = 18)
  ) 
cases_map_mobile

cases_freq <- usa_cases %>%
  count(new_cases_class) %>%
  group_by(new_cases_class)

scale_mobile <- ggplot(cases_freq, aes(x = new_cases_class,
                                       y = n,
                                       color = new_cases_class,
                                       fill = new_cases_class,
                                       label = n)) +
  geom_col() +
  geom_text(aes(y = 0),
            color = "black",
            size = 5,
            hjust = 0) + 
  theme_minimal() +
  scale_fill_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  scale_color_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(cases_freq$new_cases_class))) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    #plot.margin = margin(b = 190,
    #                    t= 5,
    #                   r = 5),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(colour = "grey40")
  ) 
scale_mobile

plot_grid(cases_map_mobile, scale_mobile,
          ncol = 2,
          rel_widths = c(4.4,2))

ggsave("gh_action/usa_new_cases_mobile.png", bg = "white",
       width = 8, height = 8*(628/1200), dpi = 320)

## illinois ----
illinoispop <- 12741080

### shapefiles ----
rdsurl <- "https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/il_counties.rds"
il_counties_clean <- rio::import(rdsurl)

### CDC data ----
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
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–90%","90–100%"),
               include.lowest = TRUE
           )) %>%
  mutate(adult_class = 
           cut(x = Series_Complete_18PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–90%","90–100%"),
               include.lowest = TRUE
           )) %>%
  mutate(senior_class = 
           cut(x = Series_Complete_65PlusPop_Pct,
               breaks = c(0,10,20,30,40,50,60,70,80,90,100),
               labels = c("0–10%","10–20%","20–30%","30–40%","40–50%",
                          "50–60%","60–70%","70–80%","80–90%","90–100%"),
               include.lowest = TRUE
           ))
cdc_vaccines_geo_merged <- merge(cdc_county_vaccine,
                                 il_counties_clean,
                                 by = "GEOID")  

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
           cut(x = as.numeric(cases_per_100K_7_day_count_change)/7,
               breaks = c(0,5,15,25,35,50,100,Inf),
               labels = c("0–5","5–15","15–25","25–35",
                          "35–50","50–100","100+"),
               include.lowest = TRUE,
               ordered_result = TRUE
           ))

cdc_cases_merged <- merge(cdc_cases,
                          il_counties_clean,
                          by = "GEOID")


il_levels <- us_community_levels %>%
  filter(State_name == "Illinois")

il_levels_geo <- merge(il_levels,
                       il_counties_clean,
                       by = "GEOID")

### levels map ----
cdc_levels <- ggplot(data = il_levels_geo) +
  geom_sf(data = il_levels_geo,
          mapping = aes(fill = community_level,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(limits = c("Low","Medium","High"),
                    palette = "Spectral",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Levels",
       caption =  "Source: CDC",
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_levels 


### cases map ----
cdc_cases_map <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = new_cases_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    limits = c("0–5","5–15","15–25",
               "25–35","35–50","50–100","100+"),
    palette = "Oranges",
    direction = 1,
    na.value = "grey80") +
  labs(title = "New Cases per 100,000 Residents",
       subtitle = "Average over past seven days",
       caption =  paste("Source: CDC. Latest data:",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_cases_map

### transmission level ----
cdc_transmission <- ggplot(data = cdc_cases_merged) +
  geom_sf(data = cdc_cases_merged,
          mapping = aes(fill = community_transmission_level,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(limits = c("low","moderate","substantial","high"),
                    palette = "RdYlGn",
                    direction = -1,
                    na.value = "grey80") +
  labs(title = "Community Transmission Levels",
       caption =  paste("Source: CDC. Latest data:",
                        tail(cdc_cases_merged$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_transmission 

### combined cases and vax map ----
#cdc_levels +
 # labs(title = "Community Levels",
  #     caption =  NULL,
   #    fill = NULL)+
#  theme(plot.title = element_text(size = 10)) +
  cdc_transmission +
  labs(title = "Community Transmission Levels",
       #caption =  NULL,
       fill = NULL)+
  theme(plot.title = element_text(size = 10)
  ) 

ggsave("gh_action/IL_cases_transmission.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

### combined il vax ----
#### pct 65+ map ----
cdc_total_vax_65 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = senior_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Percent Fully Vaccinated 65+",
       caption =  paste("Source: CDC. Latest data:",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_total_vax_65

#### pct 18+ ----
cdc_total_vax_18 <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = adult_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Percent Fully Vaccinated 18+",
       caption =  paste("Source: CDC. Latest data:",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_total_vax_18

#### pct total pop vaccinated map ----
cdc_total_vax <- ggplot(data = cdc_vaccines_geo_merged) +
  geom_sf(data = cdc_vaccines_geo_merged,
          mapping = aes(fill = total_class,
                        geometry = geometry),
          size = .25) +
  coord_sf(crs = st_crs(4326)) +
  scale_fill_brewer(
    limits = c("0–10%","10–20%","20–30%","30–40%","40–50%",
               "50–60%","60–70%","70–80%","80–90%","90–100%"),
    palette = "BrBG",
    direction = 1,
    na.value = "grey80") +
  labs(title = "Percent Fully Vaccinated",
       caption =  paste("Source: CDC. Latest data:",
                        tail(cdc_vaccines_geo_merged$short_date,1)),
       fill = NULL)+
  theme(
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid.major = element_blank(),  
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(.5, "cm"),
    panel.background = element_blank(),
    plot.caption = element_text(colour = "grey40")
  ) 
cdc_total_vax

### combined vax maps ----
cdc_total_vax + 
  labs(title = "Percent Fully Vaccinated",
       subtitle = "Total Population",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6)) +
  cdc_total_vax_18 +
  labs(title = NULL,
       subtitle = "18 and older",
       caption =  NULL,
       fill = NULL) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6)) +
  cdc_total_vax_65 +
  labs(title = NULL,
       subtitle = "65 and older",
       fill = NULL) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 11, 
                                     hjust = .6))

ggsave("gh_action/IL_vax_combined.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

#facets ----
## Champaign ----
### cdc ----
cdc_champaign_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_fips_17019_external"
cdc_champaign_data <- rio::import(
  cdc_champaign_url,
  format = "json")$integrated_county_timeseries_external_data

cdc_champaign_cases <- cdc_champaign_data |> 
  select(date, cases_7_day_count_change, deaths_7_day_count_change,
         percent_positive_7_day) |> 
  arrange(date) |>
  mutate(date = ymd(date)) %>%
  mutate(Date = ymd(date)) |> 
  mutate(avg_new_cases = cases_7_day_count_change/7) |> 
  mutate(avg_new_cases = if_else(avg_new_cases == 0,
                                 NA,
                                 avg_new_cases)) |> 
  fill(avg_new_cases, .direction = "down") 

idph_cases_champaign <- cdc_champaign_cases 

idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

### hhs hospitalizations ----
hospitalizations_url <- "https://healthdata.gov/resource/anag-cw7u.json?zip=61801"
hospitalizations <- rio::import(hospitalizations_url,
                                format = "json") %>% 
  mutate(Date = ymd(ymd_hms(collection_week))) %>%
  mutate(total_adult = as.double(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum)) %>%
  mutate(total_pediatric = as.double(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum)) %>%
  select(Date,hospital_name,total_adult,total_pediatric) %>%
  pivot_longer(cols = c(total_adult,total_pediatric),
               names_to = "names",
               values_to = "values") %>%
  filter(values >= 0) 

hospitalizations_by_date <- hospitalizations %>%
  group_by(Date,hospital_name) %>%
  summarise(total = sum(values)) %>%
  group_by(Date) %>%
  summarise(sum_hospitalized = sum(total)) %>%
  mutate(avg_hospitalized = sum_hospitalized/7) %>%
  mutate(CountyName = "Champaign")

### cdc ----
cdc_champaign_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_fips_17019_external"
cdc_champaign_data <- rio::import(
  cdc_champaign_url,
  format = "json")$integrated_county_timeseries_external_data

cdc_champaign_hosp <- cdc_champaign_data %>%
  select(date, percent_adult_inpatient_beds_used_confirmed_covid,
         percent_adult_icu_beds_used_confirmed_covid) %>%
  arrange(date) %>%
  mutate(date = ymd(date)) %>%
  mutate(Date = ymd(date))

### combined ----

idph_cases_vax_hosp <- full_join(idph_cases_champaign, idph_vax_champaign) %>%
  full_join(hospitalizations_by_date) %>%
  full_join(cdc_champaign_hosp) %>%
  select(Date, AdministeredCountRollAvg,
          avg_new_cases, avg_hospitalized,
         percent_adult_inpatient_beds_used_confirmed_covid,
         percent_adult_icu_beds_used_confirmed_covid) %>%
  arrange(Date)

idph_cases_vax_hosp_longer <- idph_cases_vax_hosp %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(
    names, 
    "avg_new_cases" = "Average New Cases",
    "monthlydead" = "Deaths in the Past Month",
    "AdministeredCountRollAvg" = "Average New Vaccine Doses",
    "avg_hospitalized" = "Average Hospitalized",
    "percent_adult_inpatient_beds_used_confirmed_covid" = "Pct. Hosp. Beds Used",
    "percent_adult_icu_beds_used_confirmed_covid" = "Pct. ICU Beds Used"))%>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date))) %>%
  drop_na()



avg_new_cases <- round(tail(idph_cases_champaign$avg_new_cases,1))
pct_fully_vaccinated <- round(100*tail(idph_vax_champaign$PctFullyVaccinatedPopulation,1), digits = 1)
avg_new_vaccine_doses <- tail(idph_vax_champaign$AdministeredCountRollAvg,1)
avg_hospitalized <- round(tail(hospitalizations_by_date$avg_hospitalized,1))

### plot ----

ggplot(idph_cases_vax_hosp_longer,
       aes(x = as.Date(Date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(caption = paste("Source: CDC and HHS. Latest data:",
                       tail(idph_cases_vax_hosp_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  expand_limits(y = 0) +
 scale_colour_manual(guide = "none",
                      values = c("#B45F06","black","#35978f","#d90000",
                                 "#d90000","#d90000")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 && 
    avg_hospitalized >= 0 &&
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/Champaign_facet.png", 
         width = 8, height = 8*(628/1200), dpi = 320)
}

idph_cases_vax_hosp_longer <- idph_cases_vax_hosp %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(
    names, 
    "avg_new_cases" = "Avg. New Cases",
    "AdministeredCountRollAvg" = "Avg. New Vaccine Doses",
    "avg_hospitalized" = "Avg. Hospitalized",
    "percent_adult_inpatient_beds_used_confirmed_covid" = "% Hosp. Beds Used",
    "percent_adult_icu_beds_used_confirmed_covid" = "% ICU Beds Used"))%>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date))) %>%
  drop_na()


ggplot(idph_cases_vax_hosp_longer,
       aes(x = as.Date(Date),
           y = values,
           colour = names)) +
  geom_line(size = .3) +
  facet_wrap(~ names, scales = "free_y",
             ncol = 1,
             dir = "v") +
  labs(caption = paste("Source: CDC, HHS and IDPH. Latest data:",
                       tail(idph_cases_vax_hosp_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","black","#35978f","#d90000",
                                 "#d90000","#d90000")) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 6),
    axis.text.y = element_text(size = 5),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 5),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40",
                                size = 4))

if (avg_new_cases >= 0 && 
    avg_hospitalized >= 0 &&
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/Champaign_facet_mobile.png", 
         width = 2, height = 8*(628/1200), dpi = 320)
}

## Ilinois ----
cdc_il_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=us_trend_by_IL"

cdc_il_data_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=us_trend_by_IL"
cdc_il_data <- rio::import(cdc_il_data_url, format = "json")$us_trend_by_Geography
cdc_il_new_deaths <- cdc_il_data %>%
  select(date,new_death) %>%
  mutate(date = mdy(date)) %>%
  mutate(avg_new_deaths = new_death) %>%
  select(date,avg_new_deaths)

cdc_il_new_cases <- cdc_il_data %>%
  select(date,New_case, percent_positive_7_day) %>%
  mutate(date = mdy(date)) %>%
  mutate(avg_new_cases = New_case) %>%
  select(date,avg_new_cases, percent_positive_7_day)

cdc_il_hosp <- cdc_il_data %>%
  select(date,weekly_inpatient_beds_used_clean) %>%
  mutate(date = mdy(date)) %>%
  mutate(hosp_patients = weekly_inpatient_beds_used_clean) %>%
  select(date,hosp_patients)

cdc_il_vax <- cdc_il_data %>%
  select(date,Administered_Weekly) %>%
  mutate(date = mdy(date)) %>%
  mutate(daily_vaccinations = Administered_Weekly) %>%
  select(date,daily_vaccinations)


cdc_il_joined <- full_join(cdc_il_new_cases, cdc_il_vax) %>%
  full_join(cdc_il_hosp) %>%
  full_join(cdc_il_new_deaths) %>%
  select(date,
         avg_new_deaths, avg_new_cases, hosp_patients, percent_positive_7_day)

cdc_il_longer <- cdc_il_joined %>%
  pivot_longer(!date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(names, 
                               "avg_new_cases" = "Average New Cases",
                               "hosp_patients" = "Hospitalized",     
                               "avg_new_deaths" = "Average New Deaths",
                               "percent_positive_7_day" = "Test Positivity"))  %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

### plot ----

ggplot(cdc_il_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(caption = paste("Source: CDC. Latest data:",
                       tail(cdc_il_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","#d90000","black","#35978f")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 && 
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {  
  ggsave("gh_action/IL_facet.png", 
         width = 8, height = 8*(628/1200), dpi = 320)
}

ggplot(cdc_il_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y",
             ncol = 1) +
  labs(caption = paste("Source: CDC. Latest data:",
                       tail(cdc_il_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","#d90000","black","#35978f")) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 && 
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {  
  ggsave("gh_action/IL_facet_mobile.png", 
         width = 3, height = 8*(628/1200), dpi = 320)
}


## us facet ----

### get data ----
cdc_usa_data_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=us_trend_by_USA"
cdc_usa_data <- rio::import(cdc_usa_data_url, format = "json")$us_trend_by_Geography
cdc_new_deaths <- cdc_usa_data %>%
  select(date,new_death) %>%
  mutate(date = mdy(date)) %>%
  mutate(avg_new_deaths = new_death) %>%
  select(date,avg_new_deaths)

cdc_new_cases <- cdc_usa_data %>%
  select(date,New_case, percent_positive_7_day) %>%
  mutate(date = mdy(date)) %>%
  mutate(avg_new_cases = New_case) %>%
  select(date,avg_new_cases, percent_positive_7_day)

cdc_hosp <- cdc_usa_data %>%
  select(date,weekly_inpatient_beds_used_clean) %>%
  mutate(date = mdy(date)) %>%
  mutate(hosp_patients = weekly_inpatient_beds_used_clean) %>%
  select(date,hosp_patients)

cdc_vax <- cdc_usa_data %>%
  select(date,Administered_Weekly) %>%
  mutate(date = mdy(date)) %>%
  mutate(daily_vaccinations = Administered_Weekly) %>%
  select(date,daily_vaccinations)

#### combined
us_data <- full_join(cdc_new_cases, cdc_new_deaths) %>%
  full_join(cdc_hosp) %>%
  full_join(cdc_vax)
#us_data$people_fully_vaccinated <- as.double(us_data$people_fully_vaccinated)
us_data_longer <- us_data %>%
  select(date, hosp_patients, avg_new_cases, avg_new_deaths,
         percent_positive_7_day) %>%
  pivot_longer(!date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(names, 
                               "avg_new_cases" = "Average New Cases",
                               "hosp_patients" = "Hospitalized",
                               "avg_new_deaths" = "Average New Deaths",
                               "percent_positive_7_day" = "Test Positivity"))  %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date)))


### plot ----
ggplot(us_data_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(caption = paste("Source: CDC. Latest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","#d90000","black","#35978f")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("gh_action/US_facet.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

ggplot(us_data_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y",
             ncol = 1) +
  labs(caption = paste("Source: CDC. Latest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","#d90000","black","#35978f")) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

ggsave("gh_action/US_facet_mobile.png", 
       width = 3, height = 8*(628/1200), dpi = 320)

## world  ----

### get data ----
#### cases ----
owid_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/cases_deaths/new_cases.csv"
owid_new_cases_world <- rio::import(owid_url, format = "csv") |> 
  select(date,World) |> 
  mutate(new_cases = World) |> 
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) |> 
  mutate(pct_change_new_cases = 
           ((avg_new_cases - lag(avg_new_cases,14))/lag(avg_new_cases,14))) %>%
  mutate(Date = ymd(date)) %>%
  mutate(date = as_date(Date)) %>%
  mutate(location = "World") |> 
  select(!World)

jhu_new_cases <- owid_new_cases_world


#### deaths ----
owid_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/cases_deaths/new_deaths.csv"
owid_new_deaths_world <- rio::import(owid_url, format = "csv") |> 
  select(date,World) |> 
  mutate(new_deaths = World) |> 
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) |> 
  mutate(pct_change_new_deaths = 
           ((avg_new_deaths - lag(avg_new_deaths,14))/lag(avg_new_deaths,14))) %>%
  mutate(Date = ymd(date)) %>%
  mutate(date = as_date(Date)) %>%
  mutate(location = "World") |> 
  select(!World)

jhu_new_deaths <- owid_new_deaths_world

#### vaccines ----
owid_vaccines_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv"
owid_vaccines <- rio::import(owid_vaccines_url, format = "csv") %>%
  filter(iso_code == "OWID_WRL") %>%
  select(date, people_fully_vaccinated,daily_vaccinations)

#### combined
us_data <- full_join(jhu_new_cases, jhu_new_deaths) 
us_data_longer <- us_data %>%
  select(date, avg_new_cases, avg_new_deaths) %>%
  pivot_longer(!date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(names, 
                               "avg_new_cases" = "Average New Cases",
                               "avg_new_deaths" = "Average New Deaths"))  %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date)))

### plot ----
ggplot(us_data_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(caption = paste("Source: Our World in Data. Latest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","black")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("gh_action/world_facet.png", 
       width = 8, height = (8*(628/1200))/1.5, dpi = 320)

ggplot(us_data_longer,
       aes(x = as.Date(date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y",
             ncol = 1) +
  labs(caption = paste("Source: Our World in Data.\nLatest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","black")) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

ggsave("gh_action/world_facet_mobile.png", 
       width = 3, height = (8*(628/1200))/1.5, dpi = 320)

# acceleration charts ----
## cases ----
### get data ----
#### Champaign ----
### cdc ----
cdc_champaign_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_fips_17019_external"
cdc_champaign_data <- rio::import(
  cdc_champaign_url,
  format = "json")$integrated_county_timeseries_external_data

cdc_champaign_cases <- cdc_champaign_data |> 
  select(date, cases_7_day_count_change, deaths_7_day_count_change,
         percent_positive_7_day) |> 
  arrange(date) |>
  mutate(date = ymd(date)) %>%
  mutate(Date = ymd(date)) |> 
  mutate(avg_new_cases = cases_7_day_count_change/7) |> 
  mutate(avg_new_cases = if_else(avg_new_cases == 0,
                                 NA,
                                 avg_new_cases)) |> 
  fill(avg_new_cases, .direction = "down") 

idph_cases_champaign <- cdc_champaign_cases 

idph_cases_champaign <- idph_cases_champaign %>%
  mutate(date = as_date(Date)) %>%
  mutate(pct_change_new_cases = 
           ((avg_new_cases - lag(avg_new_cases,14))/lag(avg_new_cases,14))) %>%
  mutate(location = "Champaign County")

#### IL  -----

cdc_IL_case_acceleration <- cdc_il_new_cases %>%
  mutate(pct_change_new_cases = 
           ((avg_new_cases - lag(avg_new_cases,2))/lag(avg_new_cases,2))) %>%
  mutate(Date = date) %>%
  mutate(location = "Illinois") 

#### USA  ----

cdc_new_cases_acceleration <- cdc_new_cases %>%
  mutate(pct_change_new_cases = 
           ((avg_new_cases - lag(avg_new_cases,2))/lag(avg_new_cases,2))) %>%
  mutate(Date = date) %>%
  mutate(location = "United States")

#### World  ----
jhu_new_cases_world <- jhu_new_cases %>%
  mutate(pct_change_new_cases = 
           ((avg_new_cases - lag(avg_new_cases,14))/lag(avg_new_cases,14))) %>%
  mutate(Date = ymd(date)) %>%
  mutate(date = as_date(Date)) %>%
  mutate(location = "World")

### merge data ----
combined_cases <- full_join(idph_cases_champaign, cdc_IL_case_acceleration) %>%
  full_join(cdc_new_cases_acceleration) %>%
  full_join(jhu_new_cases_world) %>%
  select(location, Date,pct_change_new_cases)

### plot ----
ggplot(combined_cases, 
       aes(x = as.Date(Date), y = pct_change_new_cases)) +
  geom_line(color = "grey95") +
  geom_point(aes(color = pct_change_new_cases >0),
             size = .1) +
  geom_hline(yintercept = 0,
             color = "grey25",
             size = .1) +
  facet_wrap(~ location, ncol = 1,
             strip.position = "left") +
  labs(title = "14-Day Change in Average New Cases",
       caption = paste("Source: CDC.\nLatest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01)),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     position = "right") +
  scale_colour_manual(guide = "none",
                      values = c("#199fa8","#b32704")) +
  coord_cartesian(ylim = c(-1,2.5)) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 && 
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/new_cases_change_facet.png", 
         width = 8, height = 8, dpi = 320)
}

ggplot(combined_cases, 
       aes(x = as.Date(Date), y = pct_change_new_cases)) +
  geom_line(color = "grey95") +
  geom_point(aes(color = pct_change_new_cases >0),
             size = .1) +
  geom_hline(yintercept = 0,
             color = "grey25",
             size = .1) +
  facet_wrap(~ location, ncol = 1) +
  labs(title = "14-Day Change in Average New Cases",
       caption = paste("Source: CDC and OWID.\nLatest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01)),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     position = "right") +
  scale_colour_manual(guide = "none",
                      values = c("#199fa8","#b32704")) +
  coord_cartesian(ylim = c(-1,2.5)) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))


if (avg_new_cases >= 0 && 
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/new_cases_change_facet_mobile.png", 
         width = 4, height = 8, dpi = 320)
}

## death  ----
### get data ----
#### IL  -----
cdc_IL_death_acceleration <- cdc_il_new_deaths %>%
  mutate(pct_change_new_deaths = 
           ((avg_new_deaths - lag(avg_new_deaths,2))/lag(avg_new_deaths,2))) %>%
  mutate(Date = date) %>%
  mutate(location = "Illinois") 


#### USA  ----
cdc_new_deaths_acceleration <- cdc_new_deaths %>%
  mutate(pct_change_new_deaths = 
           ((avg_new_deaths - lag(avg_new_deaths,2))/lag(avg_new_deaths,2))) %>%
  mutate(Date = date) %>%
  mutate(location = "United States")


#### World ----
jhu_new_deaths_world <- jhu_new_deaths %>%
  mutate(pct_change_new_deaths = 
           ((avg_new_deaths - lag(avg_new_deaths,14))/lag(avg_new_deaths,14))) %>%
  mutate(Date = ymd(date)) %>%
  mutate(date = as_date(Date)) %>%
  mutate(location = "World")

### merge data ----
combined_deaths <- full_join(cdc_IL_death_acceleration, cdc_new_deaths_acceleration) %>%
  full_join(jhu_new_deaths_world) %>%
  select(location, Date,pct_change_new_deaths)

### plot ----
ggplot(combined_deaths, 
       aes(x = as.Date(Date), y = pct_change_new_deaths)) +
  geom_line(color = "grey95") +
  geom_point(aes(color = pct_change_new_deaths >0),
             size = .1) +
  geom_hline(yintercept = 0,
             color = "grey25",
             size = .1) +
  facet_wrap(~ location, ncol = 1,
             strip.position = "left") +
  labs(title = "14-Day Change in Average New Deaths",
       caption = paste("Source: CDC and OWID.\nLatest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01)),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     position = "right") +
  scale_colour_manual(guide = "none",
                      values = c("#199fa8","#b32704")) +
  coord_cartesian(ylim = c(-1,2)) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 &&
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/new_deaths_change_facet.png",
         width = 8, height = 6, dpi = 320)
}

ggplot(combined_deaths, 
       aes(x = as.Date(Date), y = pct_change_new_deaths)) +
  geom_line(color = "grey95") +
  geom_point(aes(color = pct_change_new_deaths >0),
             size = .1) +
  geom_hline(yintercept = 0,
             color = "grey25",
             size = .1) +
  facet_wrap(~ location, ncol = 1) +
  labs(title = "14-Day Change in Average New Deaths",
       caption = paste("Source: CDC and OWID.\nLatest data:",
                       tail(us_data_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01)),
               labels = label_date_short()) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     position = "right") +
  scale_colour_manual(guide = "none",
                      values = c("#199fa8","#b32704")) +
  coord_cartesian(ylim = c(-1,2)) +
  theme(#axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93"),
    #strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.caption = element_text(colour = "grey40"))

if (avg_new_cases >= 0 &&
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0) {
  ggsave("gh_action/new_deaths_change_facet_mobile.png",
         width = 4, height = 6, dpi = 320)
}


# vax comparison chart ----

## set population variables ----
champaignpop <- 209983
vermilionpop <- 76806
fordpop <- 13264
edgarpop <- 17360
douglaspop <- 19479
piattpop <- 16396
iroquoispop <- 27604
dewittpop <- 15769
maconpop <- 104712
moultriepop <- 14717
illinoispop <- 12741080
colespop <- 50885
mcleanpop <- 172828



## import and clean data ----

vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                             format = "csv") %>%
  mutate(population = champaignpop) %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_vermilion <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Vermilion",
                             format = "csv") %>%
  mutate(population = vermilionpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated))  

vax_ford <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Ford",
                        format = "csv") %>%
  mutate(population = fordpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_edgar <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Edgar",
                         format = "csv") %>%
  mutate(population = edgarpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_douglas <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Douglas",
                           format = "csv") %>%
  mutate(population = douglaspop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_piatt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Piatt",
                         format = "csv") %>%
  mutate(population = piattpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_iroquois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Iroquois",
                            format = "csv") %>%
  mutate(population = iroquoispop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_dewitt <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=De%20Witt",
                          format = "csv") %>%
  mutate(population = dewittpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_macon <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Macon",
                         format = "csv") %>%
  mutate(population = maconpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_moultrie <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Moultrie",
                            format = "csv") %>%
  mutate(population = moultriepop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_coles <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Coles",
                         format = "csv") %>%
  mutate(population = colespop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_mclean <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=McLean",
                          format = "csv") %>%
  mutate(population = mcleanpop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_illinois <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                            format = "csv") %>%
  mutate(population = illinoispop)  %>%
  mutate(PersonsDose1 = PersonsWithOneDose) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 


vax_nearby <- full_join(vax_champaign, vax_vermilion) %>%
  full_join(vax_ford) %>%
  full_join(vax_edgar) %>%
  full_join(vax_douglas) %>%
  full_join(vax_piatt) %>%
  full_join(vax_iroquois) %>%
  full_join(vax_dewitt) %>%
  full_join(vax_macon) %>%
  full_join(vax_moultrie) %>%
  full_join(vax_mclean) %>%
  full_join(vax_coles) %>%
  full_join(vax_illinois) %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PercentDose1 = PctVaccinatedOneDosePopulation) %>%
  mutate(PercentOnlyDose1 = PercentDose1 - PctFullyVaccinatedPopulation) %>%
  mutate(New_doses_per_100K = (AdministeredCountRollAvg/population)*100000) %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date)))


## plot ----

last_vax_nearby <- vax_nearby %>%
  filter(Date == tail(Date, 1)) %>%
  arrange(desc(PctFullyVaccinatedPopulation))

ggplot(last_vax_nearby, aes(y = reorder(CountyName,
                                        PctFullyVaccinatedPopulation))) +
  geom_segment(aes(x = PctFullyVaccinatedPopulation, # first line segment to dose2
                   yend = CountyName), 
               xend = 0, 
               colour = "#674EA7",
               size = 7) + 
  geom_text(data = last_vax_nearby,
            aes(x = PctFullyVaccinatedPopulation,
                label = percent(PctFullyVaccinatedPopulation, .1)),
            hjust = 1.1,
            size = 3.5,
            colour = "white") +
  scale_x_continuous(labels = percent,
                     limits = c(0,1),
                     expand = expansion(mult = c(0,.05))) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  labs(title = "Percent of Total Population Fully Vaccinated in Nearby Counties",
       caption =  paste("Source: IDPH. Latest data:",
                        tail(last_vax_nearby$short_date,1)))+
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey93"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),  
        plot.caption = element_text(colour = "grey40")) 

ggsave("gh_action/nearby_fully_vaccinated.png", 
       width = 8, height = 8*(628/1200), dpi = 320)
