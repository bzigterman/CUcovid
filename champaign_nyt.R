url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties.csv"
uscounties <- rio::import(url,
                          format = "csv") 
champaign_nyt <- uscounties %>%
  filter(county == "Champaign") %>%
  filter(state == "Illinois")
champaign_nyt_longer <- champaign_nyt %>%
  select(date, cases_avg, deaths_avg) %>%
  pivot_longer(!date,
               values_to = "values",
               names_to = "names")
