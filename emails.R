library(httr)
library(rio)
library(tidyverse)



# get data
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



## text ----
dead_last_month <- tail(idph_cases_champaign$monthlydead,1)
avg_new_cases <- round(tail(idph_cases_champaign$avg_new_cases,1))
pct_fully_vaccinated <- round(100*tail(idph_vax_champaign$PctVaccinatedPopulation,1), digits = 1)
avg_new_vaccine_doses <- tail(idph_vax_champaign$AdministeredCountRollAvg,1)
short_date <- tail(idph_cases_vax_longer$short_date,1)
weekday <- wday(tail(idph_cases_vax_longer$Date,1), label = TRUE, abbr = FALSE)
month_ago_deaths <- tail(lag(idph_cases_champaign$monthlydead, 31),1)
month_ago_cases <- round(tail(lag(idph_cases_champaign$avg_new_cases, 31),1))
month_ago_vaccinated <- round(100*tail(lag(idph_vax_champaign$PctVaccinatedPopulation,31),1), digits = 1)
month_ago_new_doses <- tail(lag(idph_vax_champaign$AdministeredCountRollAvg,31),1)

tweet_text <- paste(
  "As of ",weekday," in Champaign County (vs. a month ago):\n\n",
  "— Average new cases: ",avg_new_cases," (vs. ",month_ago_cases,")\n",
  "— Deaths in the past month: ",dead_last_month," (vs. ",month_ago_deaths,")\n",
  "— Percent of Champaign County fully vaccinated: ",pct_fully_vaccinated,"% (vs. ",month_ago_vaccinated,"%)\n",
  "— Average new vaccine doses: ",avg_new_vaccine_doses," (vs. ",month_ago_new_doses,")",
  "\n\n",
  sep = ""
)



web_text <- "

## Champaign County

![Champaign County Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/Champaign_facet.png)

Charts for Champaign County are posted weekdays on Twitter [@ChampaignCovid](https://twitter.com/ChampaignCovid).

## Illinois

![Illinois Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_facet.png)

![Illinois CDC_vax_combined map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_vax_combined.png)

![IL CDC_cases_transmission_IL map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_cases_transmission.png)

Community transmission levels are calculated by the CDC based on new cases per capita in the past week and test positivity.

## United States

![USA Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/US_facet.png)

![USA fully vaccinated map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/usa_vax_total.png)

![USA transmission levels map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/usa_transmission.png)

## World

![World Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/world_facet.png)

## Case Acceleration

![Case Acceleration](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/new_cases_change_facet.png)

Sources: the [Champaign-Urbana Public Health District](https://www.c-uphd.org/champaign-urbana-illinois-coronavirus-information.html), the [University of Illinois](https://go.illinois.edu/COVIDTestingData), the [Illinois Department of Public Health](http://www.dph.illinois.gov/covid19), the [CDC](https://covid.cdc.gov/covid-data-tracker/), [Our World in Data](https://github.com/owid/covid-19-data/tree/master/public/data) and the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19).
"

email_text <- paste(
  tweet_text,
  web_text
)


if (avg_new_cases >= 0 && 
    dead_last_month >= 0 && 
    pct_fully_vaccinated >= 0 &&
    pct_fully_vaccinated <= 100 &&
    avg_new_vaccine_doses >= 0 &&
    month_ago_cases >= 0 && 
    month_ago_deaths >= 0 && 
    month_ago_vaccinated >= 0 &&
    month_ago_vaccinated <= 100 &&
    month_ago_new_doses >= 0
) {
  POST(
    url = "https://api.buttondown.email/v1/emails",
    add_headers(Authorization = Sys.getenv("BUTTONDOWN_API_KEY")),
    body = list(
      body = email_text,
      email_type = "public",
      subject = "COVID-19 Charts"
    )
  )
}
