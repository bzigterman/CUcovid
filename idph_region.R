library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(clipr)

idph_region6 <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=6&daysIncluded=0",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(ReportDate)) 

#idph_region6noUI <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetResurgenceData?format=csv&regionID=14&daysIncluded=0",
 #                           format = "csv")# %>%
  #mutate(Date = mdy_hms(ReportDate)) 
