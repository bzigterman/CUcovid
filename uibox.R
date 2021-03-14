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
box_read_csv(file_id = "781566755894")
box_dl(file_id = "781566755894")
?box_ls
box_ls(dir_id = "0")
GET("https://api.box.com/2.0/files/781566755894/content/")
#https://uofi.app.box.com/s/nrrnx2fn6sqt4hhuh2ckq2u6z5o4zmqg/file/781566755894
box_collab_get(dir_id = "nrrnx2fn6sqt4hhuh2ckq2u6z5o4zmqg",
               file_id = "781566755894")
box_dl(file_id = "781566755894")
box_dl(file_id = "781566755894")
box_collab_get(file_id = "781566755894")
GET("https://api.box.com/2.0/shared_items?shared_link=https://uofi.app.box.com/s/nrrnx2fn6sqt4hhuh2ckq2u6z5o4zmqg/file/781566755894")
GET("https://api.box.com/2.0/shared_items", add_headers(BoxApi = "https://uofi.app.box.com/s/nrrnx2fn6sqt4hhuh2ckq2u6z5o4zmqg/file/781566755894"))
box_auth()
