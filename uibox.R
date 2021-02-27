library(tidyverse)
library(rio)
library(boxr)
library(httr)
vignette("boxr")
box_auth()
?box_dl
box_dl(file_id = "780293011768",
       file_name = "illinois_shield_covid_data-2021-02-25.csv")
?box_read_csv
box_read_csv(file_id = "15570872085")
box_dl(file_id = "ib24tvzc7jo88fw81bkuzr17gfadbdc6")
?box_ls
box_ls(dir_id = "0")
GET("https://api.box.com/2.0/files/780293011768/content/")
