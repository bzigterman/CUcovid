library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)

# get the data from google sheet
regioncovid <- read_sheet("1UUGDwV5qahPos-bhWUfzf4Y1WYXEh-I0JBOJaoGMrJs",
                          sheet = 1)
