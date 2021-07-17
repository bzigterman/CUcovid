# library(tidyverse)
# library(zoo)
# library(scales)
# library(rio)
# library(httr)
#library(usmap)
# library(RColorBrewer)
# library(lubridate)
# #library(extrafont)
# #library(sf)
# library(patchwork)
library(rtweet)
#font_import(prompt=FALSE)
#loadfonts()
#options(tigris_use_cache = TRUE)

# obtain twitter token ----
token <- rtweet::create_token(
  app = "CUcovidtweets",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# post it ----
rtweet::post_tweet( 
  status = "Key COVID-19 Metrics for Champaign County, Illinois:",
  media = "https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/Champaign_facet.png",
  token = token
)
