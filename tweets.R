library(tidyverse)
library(zoo)
library(scales)
library(rio)
library(httr)
library(RColorBrewer)
library(lubridate)
library(patchwork)
library(rtweet)
library(png)

# obtain twitter token ----
token <- rtweet::create_token(
  app = "CUcovidtweets",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# compile tweet text ----
## get data ----
nyt_data <- full_join(rio::import("https://github.com/nytimes/covid-19-data/raw/master/us-counties-2020.csv",
                                  format = "csv"),rio::import("https://github.com/nytimes/covid-19-data/raw/master/us-counties-2021.csv",
                                                              format = "csv")) |> 
  full_join(rio::import("https://github.com/nytimes/covid-19-data/raw/master/us-counties-2022.csv",
                        format = "csv")) |>
  full_join(rio::import("https://github.com/nytimes/covid-19-data/raw/master/us-counties-2023.csv",
                        format = "csv")) |> 
  filter(fips == 17019)
nyt_champaign <- nyt_data |> 
  mutate(Date = ymd(date)) |> 
  mutate(new_casess = cases - lag(cases, 1)) |> 
  mutate(
    add = Reduce(function(prev, this) min(this+prev, 0),
                 new_casess, init = 0, accumulate = TRUE, right = TRUE)[-1], 
    new_cases = pmax(new_casess + add, 0)
  ) %>%
  select(-add) |> select (-new_casess) |>
  mutate(new_deathss = deaths-lag(deaths,1))|> 
  mutate(
    add = Reduce(function(prev, this) min(this+prev, 0),
                 new_deathss, init = 0, accumulate = TRUE, right = TRUE)[-1], 
    new_deaths = pmax(new_deathss + add, 0)
  ) %>%
  select(-add) |> select (-new_deathss) |> select(-date) |> 
  mutate(avg_new_cases = rollapply(new_cases, width = 7, FUN = mean, na.rm = TRUE, fill = NA, align = "right")) %>%
  mutate(monthlydead = rollmean(new_deaths, k = 31, 
                                fill = NA, align = "right")*31)

idph_cases_champaign <- nyt_champaign 

## hhs hospitalizations ----
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

## cdc hospitalizations ----
cdc_champaign_url <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_fips_17019_external"
cdc_champaign_data <- rio::import(
  cdc_champaign_url,
  format = "json")$integrated_county_timeseries_external_data

cdc_champaign_hosp <- cdc_champaign_data %>%
  select(date, percent_adult_inpatient_beds_used_confirmed_covid,
         percent_adult_icu_beds_used_confirmed_covid) %>%
  arrange(date) %>%
  mutate(date = ymd(date)) %>%
  mutate(Date = ymd(date)) %>%
  filter(date > ymd("2020-07-14"))

## combine data ----
idph_cases_vax_hosp <- full_join(idph_cases_champaign, hospitalizations_by_date) %>%
  full_join(cdc_champaign_hosp) %>%
  select(Date, #AdministeredCountRollAvg,
         monthlydead, avg_new_cases, avg_hospitalized,
         percent_adult_inpatient_beds_used_confirmed_covid) %>%
  arrange(Date)

idph_cases_vax_hosp_longer <- idph_cases_vax_hosp %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode_factor(names, 
                        "avg_new_cases" = "Average New Cases",
                        "monthlydead" = "Deaths in the Past Month",
                        "avg_hospitalized" = "Average Hospitalized",
                        "percent_adult_inpatient_beds_used_confirmed_covid" = "Percent of Hospital Beds Used by COVID-19 Patients"))  %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date))) %>%
  drop_na()




# idph_cases_vax <- full_join(idph_cases_champaign, idph_vax_champaign) %>%
#   select(Date, PersonsFullyVaccinated, AdministeredCountRollAvg,
#          monthlydead, avg_new_cases)
# 
# idph_cases_vax_longer <- idph_cases_vax %>%
#   pivot_longer(!Date,
#                values_to = "values",
#                names_to = "names") %>%
#   mutate(names = recode(names,
#                         "PersonsFullyVaccinated" = "3. People Fully Vaccinated",
#                         "avg_new_cases" = "1. Average New Cases",
#                         "monthlydead" = "2. Deaths in Past Month",
#                         "AdministeredCountRollAvg" = "4. Average New Vaccine Doses"))  %>%
#   mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
#                             mday(Date)))




## set variables ----
champaign_dead_last_month <- format(round(signif(tail(idph_cases_champaign$monthlydead,1),3)),big.mark=",")
champaign_avg_new_cases <- format(round(signif(tail(idph_cases_champaign$avg_new_cases,1),3)),big.mark=",")
champaign_weekday <- wday(tail(idph_cases_champaign$Date,1), label = TRUE, abbr = FALSE)
short_today <- wday(today()-days(1), label = TRUE, abbr = FALSE)
champaign_month_ago_deaths <- format(round(signif(tail(lag(idph_cases_champaign$monthlydead, 14),1),3)),big.mark=",")
champaign_month_ago_cases <- format(round(signif(tail(lag(idph_cases_champaign$avg_new_cases, 14),1),3)),big.mark=",")
champaign_case_pct_change <- round(100*(tail(idph_cases_champaign$avg_new_cases,1)-tail(lag(idph_cases_champaign$avg_new_cases, 14),1))/tail(lag(idph_cases_champaign$avg_new_cases, 14),1), digits = 0)
champaign_death_pct_change <- round(100*(tail(idph_cases_champaign$monthlydead,1)-tail(lag(idph_cases_champaign$monthlydead, 14),1))/tail(lag(idph_cases_champaign$monthlydead, 14),1), digits = 0)

champaign_case_pct_change_text <- 
  if (champaign_case_pct_change > 0) { 
    paste("+",champaign_case_pct_change,"%↑", sep = "")
  } else if (champaign_case_pct_change == 0) {
    paste("", sep = "")
  } else { 
    paste("",champaign_case_pct_change,"%↓", sep = "")
  }
champaign_death_pct_change_text <- 
  if (champaign_death_pct_change > 0) { 
    paste("+",champaign_death_pct_change,"%↑", sep = "")
  } else if (champaign_death_pct_change == 0) {  
    paste("", sep = "")
  } else { 
    paste("",champaign_death_pct_change,"%↓", sep = "")
  }

## make text ----

champaign_county_text <- paste(
  "As of ",champaign_weekday," (vs. two weeks ago):

",
"— Average new cases: ",champaign_avg_new_cases," (vs. ",champaign_month_ago_cases,") ",champaign_case_pct_change_text,"
",
"— Deaths in the past month: ",champaign_dead_last_month," (vs. ",champaign_month_ago_deaths,")
",
"

More charts: https://bzigterman.com/projects/covid",
sep = ""
)
champaign_county_text

# tweet plot ----
p <- ggplot(idph_cases_vax_hosp_longer,
            aes(x = as.Date(Date),
                y = values,
                colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(#title = "Metrics Since Vaccinations Began Dec. 16",
    caption = paste("Sources: CDC, HHS, NYT. Latest data:",
                    tail(idph_cases_vax_hosp_longer$short_date,1))) +
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
                      values = c("#B45F06","black","#d90000","#d90000")) +
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
ggsave( file, plot = p, device = "png", dpi = 320, width = 8, height = 8*(628/1200))

# post the tweet ----
if (champaign_avg_new_cases >= 0 && 
    champaign_dead_last_month >= 0 && 
    champaign_month_ago_cases >= 0 && 
    champaign_month_ago_deaths >= 0 && 
    champaign_weekday == short_today
) {
  rtweet::post_tweet( 
    status = champaign_county_text,
    media = file,
    media_alt_text = "Line charts with today's COVID-19 statistics for Champaign County, Illinois",
    token = token
  )
}
