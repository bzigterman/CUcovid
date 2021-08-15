library(httr)

email_text <- "During the COVID-19 pandemic, I've been making charts with data from the [Champaign-Urbana Public Health District](https://www.c-uphd.org/champaign-urbana-illinois-coronavirus-information.html), the [University of Illinois](https://go.illinois.edu/COVIDTestingData), the [Illinois Department of Public Health](http://www.dph.illinois.gov/covid19), the [CDC](https://covid.cdc.gov/covid-data-tracker/), [Our World in Data](https://github.com/owid/covid-19-data/tree/master/public/data) and the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19).

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
"

POST(
  url = "https://api.buttondown.email/v1/emails",
  add_headers(Authorization = Sys.getenv("BUTTONDOWN_API_KEY")),
  body = list(
    body = email_text,
    email_type = "public",
    subject = "COVID-19 Charts"
  )
)
