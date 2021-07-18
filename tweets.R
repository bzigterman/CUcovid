library(tidyverse)
library(zoo)
library(scales)
library(rio)
library(httr)
#library(usmap)
library(RColorBrewer)
library(lubridate)
# #library(extrafont)
# #library(sf)
library(patchwork)
library(rtweet)
library(png)
#font_import(prompt=FALSE)
#loadfonts()
#options(tigris_use_cache = TRUE)

# obtain twitter token ----
token <- rtweet::create_token(
  app = "CUcovidtweets",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)


# idph champaign county cases ----
champaignpop <- 209983

idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                                    format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(population = champaignpop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(monthlydead = rollmean(new_deaths, k = 31, 
                                fill = NA, align = "right")*31)  %>%
  mutate(Date = ymd_hms(reportDate)) 

idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

idph_cases_vax <- full_join(idph_cases_champaign, idph_vax_champaign) %>%
  select(Date, PersonsFullyVaccinated, AdministeredCountRollAvg,
         monthlydead, avg_new_cases)

idph_cases_vax_longer <- idph_cases_vax %>%
  pivot_longer(!Date,
               values_to = "values",
               names_to = "names") %>%
  mutate(names = recode(names, 
                        "PersonsFullyVaccinated" = "3. People Fully Vaccinated",
                        "avg_new_cases" = "1. Average New Cases",
                        "monthlydead" = "2. Deaths in Past Month",
                        "AdministeredCountRollAvg" = "4. Average New Vaccine Doses"))  %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date)))


## text ----
dead_last_month <- tail(idph_cases_champaign$monthlydead,1)
avg_new_cases <- round(tail(idph_cases_champaign$avg_new_cases,1))
pct_fully_vaccinated <- round(100*tail(idph_vax_champaign$PctVaccinatedPopulation,1))
avg_new_vaccine_doses <- tail(idph_vax_champaign$AdministeredCountRollAvg,1)
short_date <- tail(idph_cases_vax_longer$short_date,1)

tweet_text <- paste(
  "As of ",short_date,": \n",
  dead_last_month," people with COVID-19 have died in the last month. \n",
  avg_new_cases," new cases have been detected on average each day. \n",
  avg_new_vaccine_doses," new vaccine doses were administered. \n",
  pct_fully_vaccinated,"% of Champaign County is fully vaccinated.",
  sep = ""
)

## plot ----
p <- ggplot(idph_cases_vax_longer,
       aes(x = as.Date(Date),
           y = values,
           colour = names)) +
  geom_line() +
  facet_wrap(~ names, scales = "free_y") +
  labs(#title = "Metrics Since Vaccinations Began Dec. 16",
    caption = paste("Source: IDPH. Data updated",
                    tail(idph_cases_vax_longer$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#B45F06","#d90000","#674EA7","#674EA7")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

# save to a temp file
file <- tempfile( fileext = ".png")
ggsave( file, plot = p, device = "png", dpi = 320, width = 8, height = 8*(628/1200))



# post it ----
rtweet::post_tweet( 
  status = tweet_text,
  media = file,
  # media_alt_text = "Charts showing average new cases, deaths in the past month, the number of people fully vaccinated and average new vaccine doses for Champaign County, Illinois",
  token = token
)

