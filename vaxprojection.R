library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)

# grab data from google sheet and save to csv
vax_proj <- read_sheet("1HbhKu6Uby8b6YKGIFhTYQRiRX5Y2T8aS2DBqG7cz_eE",
                  sheet = 2)
write.csv(vax_proj,"data/vax_proj.csv", row.names = FALSE)

# chart
ggplot(vax_proj, aes(x = as.Date(date), y = pct, 
                     colour = `Percent Receiving First Dose`,
                     linetype = `Percent Receiving First Dose`)) +
  geom_line(size = 1.5) +
  ggtitle("When a Percent of Champaign County Residents Might Receive First Vaccine Dose",
          "Based on average new first doses administered over the past week")+
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = percent, 
                     position = "right",
                     expand = expansion(mult = c(0,0))) +
  scale_x_date(expand = c(0,0)) +
  scale_colour_manual(values = c("#800080", "#008040")) +
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
 
