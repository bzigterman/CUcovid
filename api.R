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

# GET(url, path = "api/COVID/GetHospitalizationResults")
# GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetHospitalizationResults")
# hospitalization <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetHospitalizationResults")
# content(hospitalization)
# hospitalization <- fromJSON(content(hospitalization, "text"))
# regionhosp <- hospitalization$regionValues
# ltc <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetLTCData")
# content(ltc)
# ltc <- fromJSON(content(ltc, "text"))
# ltc <- ltc$FacilityValues

region6 <- c("Iroquois", "Ford", "De Witt", "Piatt", "Champaign", "Vermilion", "Macon", "Moultrie", "Douglas", "Edgar", "Shelby", "Coles", "Cumberland", "Clark", "Fayette", "Effingham", "Jasper", "Crawford", "Clay", "Richland", "Lawrence") 

region6pop <- rates %>%
  mutate(region6counties = ifelse(County %in%region6, County,"")) %>%
  filter(region6counties != "") %>%
  select(County,total_population)


champaignapi <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVIDVaccine/getVaccineAdministration?CountyName=Champaign")

champaignapi <- fromJSON(content(champaignapi,"text"))
champaignapi <- champaignapi$VaccineAdministration

region6vaxapi <- GET(url = "https://idph.illinois.gov/DPHPublicInformation/api/COVIDVaccine/getVaccineAdministration",
                     query = list(CountyName = "Champaign")
)
region6[[3]]



# todos ----w
# [ ] cdc api