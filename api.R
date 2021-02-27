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

uifile <- rio::import("https://uofi.app.box.com/s/nrrnx2fn6sqt4hhuh2ckq2u6z5o4zmqg/file/780293011768", format = "csv")


# todos ----w
# [ ] cdc api