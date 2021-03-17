library(rio)
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(clipr)
library(ggrepel)

# get data ----

bigcounties <- c("Chicago","Cook","DuPage","Lake","Will","Kane","McHenry","Winnebago","Madison","St. Clair","Champaign","Sangamon","Peoria","McLean","Rock Island")

vax_chicago <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Chicago",
                             format = "csv") %>%
  mutate(population = chicagopop) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_cook <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Cook",
                             format = "csv") %>%
  mutate(population = cookpop) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 

vax_dupage <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=DuPage",
                             format = "csv") %>%
  mutate(population = dupagepop) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 



vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                             format = "csv") %>%
  mutate(population = champaignpop) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) 
