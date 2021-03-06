library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(scales)
library(zoo)
library(clipr)
library(patchwork)

# import and clean vaccine data ----
illinoispop <- 12741080

vax_IL <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(PercentDose1 = PersonsDose1/illinoispop) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated))

write.csv(vax_IL,"idph/vax_IL.csv", row.names = FALSE)

counties_vax <- GET("https://idph.illinois.gov/DPHPublicInformation/api/COVIDVaccine/getVaccineAdministrationCurrent")
counties_vax <- fromJSON(content(counties_vax, "text"))
counties_vax <- counties_vax$VaccineAdministration
counties_vax <- counties_vax %>%
  select(CountyName, PctVaccinatedPopulation)
write.csv(counties_vax,"vax/current_counties_vax.csv", row.names = FALSE)


pivoted_vax_IL <- vax_IL %>%
  select(Date, Dose1Change, Dose2Change) %>%
  pivot_longer(!Date, names_to = "Dose", values_to = "doses")

# chart of new doses ----
ILNewVaccines <- ggplot(pivoted_vax_IL, 
       aes(x = as.Date(Date), y = doses)) +
  geom_col(aes(fill = Dose)) +
  geom_line(data = vax_IL,
            aes(y = AdministeredCountRollAvg),
            colour = "black",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  scale_fill_manual(values = c("#d8cee8","#7D67B4"),
                    labels = c("First", "Final")) +
  #guides(fill = guide_legend(title = NULL)) +
  labs(title = "New Vaccine Doses Administered in Illinois",
       subtitle =  "With seven-day moving average", 
       caption = "Source: Illinois Department of Public Health")+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald"),
        plot.caption = element_text(colour = "grey40"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        legend.position = c(.15,.625),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13)) 
ILNewVaccines

ggsave("vax/ILNewVaccines.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("ILNewVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# first and second dose comparison chart ----
ILvax <- ggplot(vax_IL, aes(x = as.Date(Date)))  +
  geom_area(aes(y = PercentDose1),
            fill = "#d8cee8",
            colour = "#A897CC",
            alpha = .6) +
  geom_area(aes(y = PctVaccinatedPopulation),
            colour = "#674EA7",
            fill = "#674EA7",
            alpha = .6) +
  # annotate("text",
  #          x = as.Date("2021-03-04"),
  #          y = .13,
  #          label = "At least \none dose",
  #          colour = "black",
  #          family = "Barlow",
  #          size = 6,
  #          angle = 20) +
  # annotate("text",
  #          x = as.Date("2021-03-12"),
  #          y = .05,
  #          label = "Fully \nvaccinated",
  #          colour = "white",
  #          family = "Barlow",
  #          size = 6,
  #          angle = 20) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = c(0,
                                paste(percent(max(vax_IL$PctVaccinatedPopulation),.1),"fully\nvaccinated"),
                                paste(percent(max(vax_IL$PercentDose1),.1),"with\nat least\none dose")), 
                     position = "right",
                     expand = expansion(mult = c(0,.05)),
                     breaks = c(0,
                                max(vax_IL$PctVaccinatedPopulation),
                                max(vax_IL$PercentDose1))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Percent of Illinois Vaccinated",
       caption = "Source: Illinois Department of Public Health") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 13),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ILvax

ggsave("vax/ILvax.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("ILVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# import state case data ----

idph_cases_IL <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Illinois",
                                    format = "json") 
idph_cases_IL <- idph_cases_IL$values %>%
  mutate(population = illinoispop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right")) %>%
  mutate(Date = ymd_hms(reportDate))

# state new cases ----
ggplot(idph_cases_IL, aes(x = as.Date(Date), y = new_cases)) +
  geom_col(fill = "#B45F06",
           alpha = .25) +
  geom_line(aes(y = avg_new_cases),
            colour = "#B45F06",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "New Cases in Illinois",
       subtitle =  "With seven-day moving average",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("region/IL_new_cases.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("IL_new_cases.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# state new deaths ----
state_deaths <- ggplot(idph_cases_IL, aes(x = as.Date(Date), y = new_deaths)) +
  geom_col(fill = "#d90000",
           alpha = .25) +
  geom_line(aes(y = avg_new_deaths),
            colour = "#d90000",
            size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  labs(title = "New Deaths in Illinois",
       subtitle =  "With seven-day moving average",
       caption = "Source: Illinois Department of Public Health")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
state_deaths
ggsave("region/IL_new_deaths.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("IL_new_deaths.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 320)

# library(plotly)
# ggplotly(state_deaths)
# facet state new cases and deaths  ----
idph_cases_deaths_IL <- idph_cases_IL %>%
  select(Date, avg_new_cases, avg_new_deaths) %>%
  mutate("New Cases" =  avg_new_cases) %>%
  mutate("New Deaths" =  avg_new_deaths) %>%
  pivot_longer(cols = c("New Cases","New Deaths"),
               values_to = "Number",
               names_to = "New")

# mutate("New Cases" =  avgnewcases) %>%
#   mutate("New Tests" =  avgnewtests) %>%
#   pivot_longer(cols = c("New Tests","New Cases"),
               

ggplot(idph_cases_deaths_IL,
       #cases_and_vax, 
       aes(x = as.Date(Date),
           y = Number,
           colour = New)) +
  geom_line() +
  facet_wrap(~ New, scales = "free_y",
             ncol = 1) +
  labs(#title = "COVID-19 at the University of Illinois",
    #subtitle = "With moving seven-day average",
    caption = "Source: Illinois Department of Public Health.\nNote: Moving seven-day average.") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     #   limits = c(0,60000),
                     position = "right",
                     expand = expansion(mult = c(0,.05))
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(guide = FALSE,
                      values = c("#B45F06",
                                 "#d90000","#674EA7","#674EA7")) +
  #theme_minimal() +
  #theme_classic() +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 10),
        #  axis.line.x = element_line(colour = "black"),
        #panel.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 8),
        #axis.line.x = element_line(colour = "grey"),
        #axis.ticks = 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        #panel.grid = element_blank(),
        #panel.border = element_rect(colour = "grey", fill = NA),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 18, family = "Oswald"))

ggsave("region/state_Cases_Tests.png", width = 5, height = 8*(628/1200), dpi = 320)
ggsave("state_Cases_Tests.png", 
       path = "../bzigterman.github.io/images/",
       width = 5, height = 8*(628/1200), dpi = 320)
