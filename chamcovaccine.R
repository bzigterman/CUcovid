library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)

vax <- read_sheet("1HbhKu6Uby8b6YKGIFhTYQRiRX5Y2T8aS2DBqG7cz_eE")
write.csv(vax,"data/vax.csv", row.names = FALSE)

# new plot trying to get just the last point labeled with tail
vaxplot <- ggplot(vax, aes(x = as.Date(date),
                           y = dose1)) 
vaxplot +
  geom_area(colour = "#674EA7",
            fill = "#674EA7",
            alpha = .3) +
  geom_area(aes(x = as.Date(date), y = dose2),
            colour = "#674EA7",
            fill = "#674EA7",
            alpha = 1) +
  geom_label(aes(label = paste("First dose:",
                               percent(percent_dose1, accuracy = .1))),
             data = tail(vax, 1),
             size = 4,
             hjust = 1.3,
             family = "Barlow") +
  geom_label(data = tail(vax, 1),
             aes(x = as.Date(date), 
                 y = dose2,
                 label = paste("Fully vaccinated:",
                               percent(percent_dose2, accuracy = .1))),
             size = 4,
             hjust = 1.2,
             vjust = 0,
             family = "Barlow") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("People Vaccinated in Champaign County",
          "With percent of total population. Source: IDPH") +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald")) 

ggsave("vax/chamvax.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# chart of new doses administered
ggplot(vax, aes(x = as.Date(date), y = new_doses)) +
  geom_col(fill = "#674EA7",
           alpha = .25) +
  geom_line(aes(y = rollmean(new_doses, 
                             7, 
                             fill = TRUE, 
                             align = "right")),
            colour = "#674EA7",
            size = 1.5) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = comma, 
                     position = "right",
                     expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = FALSE) +
  ggtitle("New Vaccine Doses Administered in Champaign County",
          "With seven-day moving average. Source: IDPH")+
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 22, family = "Oswald")) 
ggsave("vax/NewVaccines.png", width = 8, height = 32/7, dpi = 320)
ggsave("NewVaccinesWeb.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

