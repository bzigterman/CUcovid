library(httr)
library(rio)
library(tidyverse)
library(lubridate)
library(zoo)

# get text from website ----
web_text <- content(GET("https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/projects/covid.md"),
                    as = "text")

email_text <- str_sub(web_text, start = 64)

subject_text <- paste(
  "Champaign County COVID-19 Metrics for ",
  wday(today(),label = TRUE, abbr = FALSE),", ",
  month(today(), label = TRUE,
        abbr = FALSE)," ",day(today()),", ",year(today()),
  sep = "")

# send email ----
POST(
  url = "https://api.buttondown.email/v1/emails",
  add_headers(Authorization = Sys.getenv("BUTTONDOWN_API_KEY")),
  body = list(
    body = email_text,
    email_type = "public",
    subject = subject_text
  )
)

