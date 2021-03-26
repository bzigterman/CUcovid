library(rio)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(scales)
library(zoo)
library(clipr)
library(patchwork)

# import and clean data ----
illinoispop <- 12741080

vax_IL <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) %>%
  mutate(PersonsDose1 = AdministeredCount - PersonsFullyVaccinated) %>%
  mutate(PercentDose1 = PersonsDose1/illinoispop) %>%
  mutate(Dose1Change = PersonsDose1 - lag(PersonsDose1)) %>%
  mutate(Dose2Change = PersonsFullyVaccinated - lag(PersonsFullyVaccinated))

write.csv(vax_IL,"idph/vax_IL.csv", row.names = FALSE)

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
       width = 8, height = 8*(628/1200), dpi = 150)

# first and second dose comparison chart ----
ILvax <- ggplot(vax_IL, aes(x = as.Date(Date)))  +
  geom_area(aes(y = PercentDose1),
            fill = "#d8cee8",
            alpha = 1) +
  geom_area(aes(y = PctVaccinatedPopulation),
            colour = "#674EA7",
            fill = "#674EA7",
            alpha = 1) +
  annotate("text",
           x = as.Date("2021-03-04"),
           y = .13,
           label = "At least \none dose",
           colour = "black",
           family = "Barlow",
           size = 6,
           angle = 20) +
  annotate("text",
           x = as.Date("2021-03-12"),
           y = .05,
           label = "Fully \nvaccinated",
           colour = "white",
           family = "Barlow",
           size = 6,
           angle = 20) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = label_percent(accuracy = .1), 
                     position = "right",
                     expand = expansion(mult = c(0,.05)),
                     breaks = c(0,
                                max(vax_IL$PctVaccinatedPopulation),
                                max(vax_IL$PercentDose1))) +
  scale_x_date(expand = c(0,0)) +
  labs(title = "Percent of Illinois Vaccinated",
       caption = "Source: Illinois Department of Public Health") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey40"),
        plot.title = element_text(size = 22, family = "Oswald")) 
ILvax

ggsave("vax/ILvax.png", width = 8, height = 8*(628/1200), dpi = 320)
ggsave("ILVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 8*(628/1200), dpi = 150)

# combined chart ----
ILNewVaccines + ILvax
ggsave("vax/combinedILvax.png", width = 8, height = 8*(628/1200), dpi = 320)
