library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
#library(dplyr)
#library(ggplot2)
library(scales)
library(zoo)
library(clipr)

# import and clean data ----
idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(PercentDose1 = PersonsDose1/209983) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated)) %>%
  mutate(Text = paste(comma(AdministeredCount, accuracy = 1),
                      " total doses administered in the past day, up ", 
                      comma(AdministeredCountChange, accuracy = 1), 
                      " from the day before. \n",
                      comma(PersonsDose1, accuracy = 1),
                      " have received at least one dose, up ",
                      comma(Dose1Change, accuracy = 1),
                      " from the day before. ",
                      percent(PercentDose1,
                              accuracy = .1),
                      " of the population has received at least one dose. \n",
                      comma(PersonsFullyVaccinated, accuracy = 1),
                      " have been fully vaccinated, up ",
                      comma(Dose2Change, accuracy = 1),
                      " from the day before. ",
                      percent(PctVaccinatedPopulation,
                              accuracy = .1),
                      " of the population has been fully vaccinated.",
                      sep = ""))

write.csv(idph_vax_champaign,"idph/vax_champaign.csv", row.names = FALSE)
write_clip(tail(idph_vax_champaign$Text, n = 1)) # paste text to clipboard

# new vaccines administered chart copy ----
pivoted_vax_champaign <- idph_vax_champaign %>%
  select(Date, Dose1Change, Dose2Change) %>%
  pivot_longer(!Date, names_to = "Dose", values_to = "doses")

#?pivot_longer
         
ggplot(pivoted_vax_champaign, 
       aes(x = as.Date(Date), y = doses)) +
  geom_col(aes(fill = Dose)) +
  geom_line(data = idph_vax_champaign,
            aes(y = AdministeredCountRollAvg),
            colour = "black",
            size = 1.4) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  scale_fill_manual(values = c("#d8cee8","#7D67B4"),
                    labels = c("First", "Final")) +
  #guides(fill = guide_legend(title = NULL)) +
  labs(title = "New Vaccine Doses Administered in Champaign County",
       subtitle =  "With seven-day moving average", 
       caption = "Source: Illinois Department of Public Health")+
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 22, family = "Oswald"),
        plot.caption = element_text(colour = "grey40"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        legend.position = c(.15,.664),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13)) 

ggsave("vax/NewVaccines.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("NewVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 150)

# first and second dose comparison chart ----
ggplot(idph_vax_champaign, aes(x = as.Date(Date)))  +
  geom_area(aes(y = PercentDose1),
            #colour = "#BEB3D9",
            fill = "#d8cee8",
            alpha = 1) +
  geom_area(aes(y = PctVaccinatedPopulation),
            colour = "#674EA7",
            fill = "#674EA7",
            alpha = 1) +
  annotate("text",
           x = as.Date("2021-02-14"),
           y = .12,
           label = "At least \none dose",
           # label = paste("Partially \nvaccinated:\n",
           #               percent(tail(idph_vax_champaign, 1)$PercentDose1,
           #                       accuracy = .1)),
           colour = "black",
           family = "Barlow",
           # fontface = "bold",
           # hjust = 0
           size = 6) +
  annotate("text",
           x = as.Date("2021-03-10"),
           y = .07,
           label = "Fully \nvaccinated",
           # label = paste("Fully \nvaccinated:\n",
           #               percent(tail(idph_vax_champaign, 1)$PctVaccinatedPopulation,
           #                       accuracy = .1)),
           colour = "white",
           family = "Barlow",
           # fontface = "bold",
           # hjust = 1
           size = 6) +
  # geom_text(aes(label = paste("Partially vaccinated:",
  #                              percent(PercentDose1, accuracy = .1))),
  #            data = tail(idph_vax_champaign, 1),
  #            size = 4,
  #            hjust = 1.8,
  #           vjust = 16.5,
  #           colour = "black",
  #            family = "Barlow") +
  # geom_text(data = tail(idph_vax_champaign, 1),
  #            aes(x = as.Date(Date), 
  #                y = PersonsFullyVaccinated,
#                label = paste("Fully vaccinated:",
#                              percent(PctVaccinatedPopulation, accuracy = .1))),
#            size = 4,
#            hjust = 1.1,
#            vjust = 15,
#           colour = "white",
#            family = "Barlow") +
xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = label_percent(accuracy = .1), 
                     position = "right",
                     expand = expansion(mult = c(0,.05)),
                     breaks = c(0,
                                max(idph_vax_champaign$PctVaccinatedPopulation),
                                max(idph_vax_champaign$PercentDose1))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Percent of Champaign County Vaccinated",
       # subtitle = "With percent of total population.",
       caption = "Source: Illinois Department of Public Health") +
  #theme_minimal() +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid = element_blank(),
        #panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/chamvax.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("VaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 150)

# first dose projection ----
avgdose1change <- mean(tail(idph_vax_champaign$Dose1Change,7), na.rm = TRUE)
xmin <- max(as.Date(idph_vax_champaign$Date))
xmax <- as.Date("2023-04-01")
ymin <- max(idph_vax_champaign$PersonsDose1)
projected <- data.frame(Date = seq(xmin, xmax, by = "day"))
projected$PersonsDose1 <- seq(ymin,
                              by = avgdose1change,
                              length.out = nrow(projected))
projected <- projected %>%
  filter(PersonsDose1 <209983) %>%
  mutate(PercentDose1 = PersonsDose1/209983)

# points
current_vax <- idph_vax_champaign %>%
  tail(1)
half_vax <- projected %>%
  filter(PercentDose1 >.5) %>%
  head(1)
eighty_vax <- projected %>%
  filter(PercentDose1 >.8) %>%
  head(1)
full_vax <- projected %>%
  filter(PercentDose1 >.99) %>%
  tail(1)

#write_clip(paste("In the past week, an average of ",comma(avgdose1change)," new Champaign County residents received their first dose of the two-dose COVID-19 vaccines each day.\n\nIf that pace continued, half of Champaign County residents could be partially vaccinated by ",month(half_vax$Date, label = TRUE, abbr = FALSE)," ",mday(half_vax$Date),", and the entire county would be partially vaccinated around ",month(full_vax$Date, label = TRUE, abbr = FALSE)," ",mday(full_vax$Date),".\n\nOf course, the pace is expected to vary as more vaccines are approved, shipments vary, people hesitate and eligibility expands.\n\nAlready, ",percent(current_vax$PercentDose1)," of Champaign County residents have been partially vaccinated, according to the Illinois Department of Public Health.\n\nBy comparison, half the country is expected to receive its first COVID-19 dose around xx, according to The New York Times. https://www.nytimes.com/interactive/2020/us/covid-19-vaccine-doses.html",sep="")) # paste text to clipboard

# first dose projection plot ----
ggplot(idph_vax_champaign, aes(x = as.Date(Date),
                               y = PercentDose1)) +
  geom_line(colour = "#800080", # actual line 
            size = 1.5) +
  geom_line(data = projected, # projected line
            aes(x = as.Date(Date),
                y = PercentDose1),
            linetype = "dashed",
            size = 1.5, 
            colour = "#008040") +
  geom_point(data = current_vax, # current point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#800080",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = current_vax, # current label
             aes(x = as.Date(Date),
                 y = PercentDose1,
                 label = paste(percent(PercentDose1),
                               "have been partially vaccinated")),
            family = "Barlow",
            hjust = -.05,
            vjust = 1.5,
            size = 4.4) +
  geom_point(data = half_vax, # half point projection point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = half_vax, # half point projection label
            aes(x = as.Date(Date),
                y = PercentDose1,
                label = paste("At current pace:\n50% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -0.3,
            size = 4.4) +
  geom_point(data = eighty_vax, # eighty pct point projection point
             aes(x = as.Date(Date),
                 y = PercentDose1),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = eighty_vax, # eighty pct point projection label
            aes(x = as.Date(Date),
                y = PercentDose1,
                label = paste("At current pace:\n80% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -0.3,
            size = 4.4) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     limits = c(0,1),
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "When a Percent of Champaign County Residents Might Be Partially Vaccinated",
         subtitle = "Based on average new first doses administered over the past week",
       caption = "Source: Illinois Department of Public Health") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 18, family = "Oswald")) 

ggsave("vax/VaccineProjection.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("VaccineProjection.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 150)

# fully vaccinated projection ----

avgdose2change <- mean(tail(idph_vax_champaign$Dose2Change,7), na.rm = TRUE)
xmin <- max(as.Date(idph_vax_champaign$Date))
xmax <- as.Date("2023-04-01")
ymin <- max(idph_vax_champaign$PersonsFullyVaccinated)
projected_full <- data.frame(Date = seq(xmin, xmax, by = "day"))
projected_full$PersonsFullyVaccinated <- seq(ymin,
                              by = avgdose2change,
                              length.out = nrow(projected_full))
projected_full <- projected_full %>%
  filter(PersonsFullyVaccinated <209983) %>%
  mutate(PctVaccinatedPopulation = PersonsFullyVaccinated/209983)

# points
current_vax <- idph_vax_champaign %>%
  tail(1)
half_vax <- projected_full %>%
  filter(PctVaccinatedPopulation >.5) %>%
  head(1)
eighty_vax <- projected_full %>%
  filter(PctVaccinatedPopulation >.8) %>%
  head(1)
full_vax <- projected_full %>%
  filter(PctVaccinatedPopulation >.99) %>%
  tail(1)

# write_clip(paste("In the past week, an average of ",comma(avgdose1change)," new Champaign County residents received their first dose of the COVID-19 vaccine each day.\n\nIf that pace continued, half of Champaign County residents could recive their first dose by ",month(half_vax$Date, label = TRUE, abbr = FALSE)," ",mday(half_vax$Date),", and the entire county would be vaccinated around ",month(full_vax$Date, label = TRUE, abbr = FALSE)," ",mday(full_vax$Date),".\n\nOf course, the pace is expected to vary as more vaccines are approved, shipments vary, people hesitate and eligibility expands.\n\nAlready, ",percent(current_vax$PercentDose1)," of Champaign County residents have received their first dose, according to the Illinois Department of Public Health.\n\nBy comparison, half the country is expected to receive its first COVID-19 dose around xx, according to The New York Times. https://www.nytimes.com/interactive/2020/us/covid-19-vaccine-doses.html",sep="")) # paste text to clipboard

# first dose projection plot 
ggplot(idph_vax_champaign, aes(x = as.Date(Date),
                               y = PctVaccinatedPopulation)) +
  geom_line(colour = "#800080", # actual line 
            size = 1.5) +
  geom_line(data = projected_full, # projected line
            aes(x = as.Date(Date),
                y = PctVaccinatedPopulation),
            linetype = "dashed",
            size = 1.5, 
            colour = "#008040") +
  geom_point(data = current_vax, # current point
             aes(x = as.Date(Date),
                 y = PctVaccinatedPopulation),
             shape = 21,
             colour = "#800080",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = current_vax, # current label
            aes(x = as.Date(Date),
                y = PctVaccinatedPopulation,
                label = paste(percent(PctVaccinatedPopulation),
                              "are already fully vaccinated")),
            family = "Barlow",
            hjust = -.05,
            vjust = 1.5,
            size = 4.4) +
  geom_point(data = half_vax, # half point projection point
             aes(x = as.Date(Date),
                 y = PctVaccinatedPopulation),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = half_vax, # half point projection label
            aes(x = as.Date(Date),
                y = PctVaccinatedPopulation,
                label = paste("At current pace:\n50% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -0.3,
            size = 4.4) +
  geom_point(data = eighty_vax, # eighty pct point projection point
             aes(x = as.Date(Date),
                 y = PctVaccinatedPopulation),
             shape = 21,
             colour = "#008040",
             size = 2.5,
             fill = "white",
             stroke = 1.5) +
  geom_text(data = eighty_vax, # eighty pct point projection label
            aes(x = as.Date(Date),
                y = PctVaccinatedPopulation,
                label = paste("At current pace:\n80% by",
                              month(Date, label = TRUE, abbr = FALSE),
                              mday(Date))),
            family = "Barlow",
            hjust = 1.1,
            vjust = -0.3,
            size = 4.4) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     limits = c(0,1),
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "When a Percent of Champaign County Residents Might Be Fully Vaccinated",
       subtitle = "Based on average new final doses administered over the past week",
       caption = "Source: Illinois Department of Public Health") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 18, family = "Oswald")) 

ggsave("vax/FullVaccineProjection.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("FullVaccineProjection.png",
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 150)


# chart combining vaccines and cases ----
champaignpop <- 209983

idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                                    format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(population = champaignpop)  %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                   fill = NA, align = "right"))  %>%
  mutate(Date = ymd_hms(reportDate)) %>%
  mutate(new_case_rate = (100000*avg_new_cases)/population) %>%
  mutate(new_deaths_rate = (1000000*avg_new_deaths)/population)

cases_and_vax <- full_join(CUcovid, idph_vax_champaign) %>%
  select(Active, Date, AdministeredCount, Hospitalized,
         Quarantined, weeklydead, AdministeredCountRollAvg) %>%
  pivot_longer(!Date, names_to = "vax_case", values_to = "count") %>%
  mutate(vax_case = recode(vax_case, 
                           "AdministeredCount" = "6. Total Vaccine Doses",
                           "Active" = "1. Active Cases",
                           "Quarantined" = "2. Currently Quarantined",
                           "Hospitalized" = "3. Currently Hospitalized",
                           "weeklydead" = "4. Deaths in Past Week",
                           "AdministeredCountRollAvg" = "5. Average New Vaccine Doses"))

ggplot(filter(cases_and_vax, Date > as.Date("2020-12-15")),
       #cases_and_vax, 
              aes(x = as.Date(Date),
                               y = count,
                  colour = vax_case)) +
  geom_line() +
  facet_wrap(~ vax_case, scales = "free_y") +
  labs(title = "COVID-19 Metrics for Champaign County Since Vaccinations Began Dec. 16",
       caption = "Sources: Illinois Department of Public Health, Champaign-Urbana Public Health District") +
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
                      values = c("#B45F06","#009e89","#ff5f1a",
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
        #panel.grid = element_blank(),
        #panel.border = element_rect(colour = "grey", fill = NA),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 18, family = "Oswald"))
ggsave("vax/vaccinefacets.png", width = 8, height = 4.5, dpi = 320)
ggsave("vaccinefacets.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 4.5, dpi = 150)


# todo ----
# - [x] save the charts, prob replace the manual-made ones
# - [x] save the data to idph folder
# - [x] generate text
# - [ ] second dose projection
# - [ ] use pivot longer to make projections