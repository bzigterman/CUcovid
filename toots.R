library(tidyverse)
library(zoo)
library(scales)
library(rio)
library(httr)
library(RColorBrewer)
library(lubridate)
library(patchwork)
library(rtoot)
library(png)

# obtain mastodon token ----
token <- Sys.getenv("RTOOT_DEFAULT_TOKEN")
verify_envvar(verbose = TRUE)

# compile tweet text ----
## get data ----
## Champaign ----
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
  mutate(avg_hospitalized = sum_hospitalized/7) 

iwss_download_url <- "https://iwss.uillinois.edu/wastewater-treatment-plant/download/159/"
iwss_download <- content(GET(iwss_download_url))

iwss <- iwss_download %>%
  mutate(Date = ymd(sample_collect_date)) %>%
  filter(method == 1) |> 
  select(Date, sars_cov_2, method) |> 
  mutate(sars_cov_2_avg = zoo::rollmean(sars_cov_2,
                                        k = 4,
                                        fill = NA,
                                        align = "right")) 

combined <- full_join(hospitalizations_by_date,iwss) |> 
  arrange(Date) |> 
  fill(avg_hospitalized,.direction = "down") |> 
  fill(sars_cov_2_avg,.direction = "down") |> 
  select(Date, avg_hospitalized,sars_cov_2_avg) |> 
  pivot_longer(!Date) |> 
  mutate(name = recode_factor(name, 
                               "avg_hospitalized" = "Avg. Hospitalized",
                               "sars_cov_2_avg" = "Wastewater Moving Avg."))

## set variables ----
latest_data <- tail(combined, n = 1)$Date
champaign_avg_hospitalized <- format(round(signif(tail(hospitalizations_by_date$avg_hospitalized,1),3)),big.mark=",")
champaign_month_ago_hospitalized <- 
  format(round(signif(tail(lag(hospitalizations_by_date$avg_hospitalized,2),1),3)),big.mark=",")

## make text ----

champaign_county_text <- paste(
  "More charts: https://bzigterman.com/projects/covid",
sep = ""
)
champaign_county_text
typeof(champaign_county_text)

# plot ----
p <- ggplot(combined,
            aes(x = as.Date(Date),
                y = value,
                colour = name)) +
  geom_line() +
  facet_wrap(~ name, scales = "free_y", ncol = 1) +
  labs(caption = paste("Sources: CDC and IWSS")) +
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
                      values = c("#B45F06","#52958F","#d90000","#d90000")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))
p
# save to a temp file
file <- tempfile( fileext = ".png")
ggsave( file, plot = p, device = "png", dpi = 320, width = 5, height = 5)

# post ----
if (champaign_avg_new_cases >= 0 && 
    champaign_month_ago_cases >= 0 && 
    latest_data >= now(tzone = "America/Chicago") - days(x = 7)
) {
  post_toot(status = champaign_county_text,
            media = file,
            alt_text = "Line charts with recent COVID-19 statistics for Champaign County, Illinois")
}



