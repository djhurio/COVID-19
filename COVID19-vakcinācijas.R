# COVID19 vakcinācijas
# https://data.gov.lv/dati/lv/dataset/covid19-vakcinacijas

options("max.print" = 1e4)

library(data.table)
library(openxlsx)
library(ggplot2)
library(ISOweek)

theme_set(theme_bw())

dat <- read.xlsx(xlsxFile = "https://data.gov.lv/dati/dataset/7171ccdc-3b01-4852-9dc4-c775bc851829/resource/51725018-49f3-40d1-9280-2b13219e026f/download/adp_covid19_vakc_2021_03_22.xlsx",
                 detectDates = TRUE)
setDT(dat)

dat[, nedela := ISOweek(Vakcinācijas.datums)]

dat[, .N, keyby = .(Vakcinācijas.posms, Vakcīnas.kārtas.numurs)]

dat.ned <- dat[, .(vakc.pers.sk = sum(Vakcinēto.personu.skaits)),
               keyby = .(nedela, Vakcinācijas.posms)]

dat.ned

max(dat$Vakcinācijas.datums)

ggplot(data = dat.ned, mapping = aes(x = nedela, y = vakc.pers.sk,
                                            fill = Vakcinācijas.posms)) +
  geom_col(position = position_dodge2(), colour = "black") +
  scale_y_continuous(name = "vakcinēto personu skaits",
                     breaks = seq(0, 100e3, by = 1e3)) +
  scale_x_discrete(name = "nedēļa") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  ggtitle(label = paste0("COVID19 vakcinācijas (", max(dat$Vakcinācijas.datums), ")"),
          subtitle = "https://data.gov.lv/dati/lv/dataset/covid19-vakcinacijas")

ggsave(filename = "COVID19-vakcinacijas.png")
ggsave(filename = "COVID19-vakcinacijas.pdf")
