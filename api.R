library(tidyverse)
library(httr)
library(jsonlite)

url <- "https://idph.illinois.gov/DPHPublicInformation/"

GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign")
GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical")
rates <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetCountyRates")
content(rates)
rates <- fromJSON(content(rates, "text"))
rates <- fromJSON(content(rates, "text"))
rates
path <- "api/COVIDExport/GetCountyRates"

GET(url, path = "api/COVID/GetHospitalizationResults")
GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetHospitalizationResults")
hospitalization <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetHospitalizationResults")
content(hospitalization)
hospitalization <- fromJSON(content(hospitalization, "text"))
regionhosp <- hospitalization$regionValues
ltc <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetLTCData")
content(ltc)
ltc <- fromJSON(content(ltc, "text"))
ltc <- ltc$FacilityValues

?fromJSON
?GET

# todos ----w
# [ ] cdc api