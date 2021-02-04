library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)

# grab data from google sheet and save to csv
vax_proj <- read_sheet("1HbhKu6Uby8b6YKGIFhTYQRiRX5Y2T8aS2DBqG7cz_eE",
                  sheet = 2)
write.csv(vax_proj,"data/vax_proj.csv", row.names = FALSE)
vax_proj_first_dose <- vax_proj %>%
  select(date,
         actual_proj,
         first_dose,
         new_dose1,
         avg_daily_first_dose,
         pct_dose1) %>%
  filter(first_dose != is.null(first_dose))


# chart of first dose
ggplot(vax_proj_first_dose, aes(x = as.Date(date), y = pct_dose1, 
                     colour = actual_proj,
                     linetype = actual_proj)) +
  geom_line(size = 1.5) +
  #annotate("text", x = 
  ggtitle("When a Percent of Champaign County Residents Might Receive First Vaccine Dose",
          "Based on average new first doses administered over the past week")+
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  scale_colour_manual(values = c("#800080", "#008040"),
                      guide = guide_legend(title = NULL)) +
  scale_linetype_discrete(guide = guide_legend(title = NULL)) +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, family = "Oswald"),
        legend.position = c(.15,.5),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 

ggsave("vax/VaccineProjection.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccineProjection.png", 
        path = "../bzigterman.github.io/images/",
        width = 8, height = 32/7, dpi = 150)
 

# chart of second dose
ggplot(vax_proj, aes(x = as.Date(date), y = pct_dose2, 
                     colour = actual_proj,
                    linetype = actual_proj)) +
  geom_line(size = 1.5) +
 # geom_line(aes(x = as.Date(date), y = pct_dose2,
#                colour = `Percent Receiving First Dose`,
 #               linetype = `Percent Receiving First Dose`)) +
  ggtitle("When a Percent of Champaign County Residents Might Receive Second Vaccine Dose",
          "Based on average new second doses administered over the past week")+
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  scale_colour_manual(values = c("#800080", "#008040"),
                      guide = guide_legend(title = NULL)) +
  #scale_linetype_manual(guide = guide_legend(title = NULL)) +
  scale_linetype_discrete(guide = guide_legend(title = NULL)) +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, family = "Oswald"),
        legend.position = c(.15,.5),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 

ggsave("vax/VaccineProjection2.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccineProjection2.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# combined
ggplot(vax_proj_first_dose, 
       aes(x = as.Date(date), y = pct_dose1, 
           colour = actual_proj,
           linetype = actual_proj)) +
  geom_line(size = 1.5) +
  geom_line(data = vax_proj %>% filter(first_dose != is.null(first_dose)),
            aes(x = as.Date(date), y = pct_dose2, 
                colour = actual_proj,
                linetype = actual_proj),
            size = 1.5) + 
  annotate("text", x = as.Date("2021-05-01"), y = .8, 
           label = "First Dose", family = "Barlow", size = 5) +
  annotate("text", x = as.Date("2021-06-01"), y = .17, 
           label = "Second Dose", family = "Barlow", size = 5) +
  ggtitle("When a Percent of Champaign County Residents Might Receive Vaccine Dose",
          "Based on average new doses administered over the past week")+
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  scale_colour_manual(values = c("#800080", "#008040"),
                      guide = guide_legend(title = NULL)) +
  scale_linetype_discrete(guide = guide_legend(title = NULL)) +
  theme(text = element_text(family = "Barlow"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, family = "Oswald"),
        legend.position = c(.15,.5),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12)) 

ggsave("vax/VaccineProjectionCombined.png", width = 8, height = 32/7, dpi = 320)
ggsave("VaccineProjectionCombined.png", 
       path = "../bzigterman.github.io/images/",
       width = 8, height = 32/7, dpi = 150)

# todo
## annotate what the two lines are in combined chart
## annotate where 50% is in first chart
## change date format in second dose chart

