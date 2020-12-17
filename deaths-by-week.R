# Deaths by week

options("max.print" = 1e4)

library(eurostat)
library(data.table)
library(ggplot2)
library(ISOcodes)

# Country names
cntry <- as.data.table(ISO_3166_1)
cntry[, .(geo = Alpha_2, cntry = Name)]


# Population
# https://ec.europa.eu/eurostat/databrowser/view/demo_gind/

dat.pop <- get_eurostat(id = tolower("DEMO_GIND"))
setDT(dat.pop)

dat.pop
dat.pop[, .N, keyby = .(indic_de)]

dat.pop[, year := year(time)]

dat.pop[indic_de == "JAN", .(year, geo, pop = values)]


# https://ec.europa.eu/eurostat/web/products-datasets/-/demo_r_mwk_ts
dat <- get_eurostat(id = "demo_r_mwk_ts")
setDT(dat)

dat
dat[, .N, keyby = .(sex)]
dat[, .N, keyby = .(unit)]

dat[, .N, keyby = .(geo)]
dat[, .N, keyby = .(time)]

dat[, year := as.integer(substr(time, 1, 4))]
dat[, week := as.integer(substr(time, 6, 7))]

dat[, .N, keyby = .(year)]
dat[, .(n = .N, Y = sum(values)), keyby = .(year)][, P := prop.table(Y)][]

dat[, .N, keyby = .(week)]
dat[, .(n = .N, Y = sum(values)), keyby = .(week)][, P := prop.table(Y)][]

dcast.data.table(data = dat[sex == "T" & week < 99],
                 formula = geo ~ year, fun.aggregate = length)
dat[geo == "DE", .N, keyby = .(week)]

dat[week > 53]
dat[week > 52]

dat[grepl("W99", time)]
dat <- dat[!grepl("W99", time)]

dat[, y2020 := factor(as.integer(year == 2020L), 0:1, c("Y<2020", "Y=2020"))]
dat[, .N, keyby = .(y2020)]


# Merge population total
dat1 <- merge(dat[sex == "T"],
              dat.pop[indic_de == "JAN", .(year, geo, pop = values)],
              by = c("year", "geo"), all.x = T)


dat1[, death.rate := values / pop * 1e6]

dat1[geo != "AD"][order(death.rate)]

dat1[geo == "EL", geo := "GR"]
dat1[geo == "UK", geo := "GB"]

dat2 <- merge(dat1, cntry[, .(geo = Alpha_2, cntry = Name)],
              by = "geo", all.x = T)

dat2[is.na(cntry), .N, keyby = .(geo, cntry)]

pl1 <- ggplot(data = dat2[sex == "T" & geo != "AD"],
              mapping = aes(x = week, y = values, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ cntry, scales = "free_y") +
  ggtitle("Deaths by week (total count)",
          "Datasource: Eurostat [DEMO_R_MWK_TS]") +
  theme_bw()

pl2 <- ggplot(data = dat2[sex == "T" & geo != "AD"],
              mapping = aes(x = week, y = death.rate, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ cntry) +
  ggtitle("Death rate by week (per 1,000,000 individuals)",
          "Datasource: Eurostat [DEMO_R_MWK_TS], [DEMO_GIND]") +
  theme_bw()

pl3 <- ggplot(data = dat2[sex == "T" & geo != "AD"],
              mapping = aes(x = week, y = death.rate, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ cntry, scales = "free_y") +
  ggtitle("Death rate by week (per 1,000,000 individuals)",
          "Datasource: Eurostat [DEMO_R_MWK_TS], [DEMO_GIND]") +
  theme_bw()

ggsave(filename = "dths-by-wk-total.png", plot = pl1, width = 16, height = 9)
ggsave(filename = "dths-by-wk-rate.png", plot = pl2, width = 16, height = 9)
ggsave(filename = "dths-by-wk-rate-free-y.png", plot = pl3, width = 16, height = 9)

cairo_pdf(filename = "dths-by-wk.pdf", width = 16, height = 9, onefile = TRUE)
pl1
pl2
pl3
dev.off()
