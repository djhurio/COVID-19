# Slowdown
# http://www.dannydorling.org/books/SLOWDOWN/Covid19.html

options("max.print" = 1e4)

library(data.table)
library(ggplot2)
library(plotly)

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

theme_set(theme_bw())

dat <- fread("https://data.gov.lv/dati/dataset/f01ada0a-2e77-4a82-8ba2-09cf0cf90db3/resource/d499d2f0-b1ea-4ba2-9600-2c701b03bd4a/download/covid_19_izmeklejumi_rezultati.csv")

names(dat)

dat[, Datums := (as.IDate(x = Datums, format = "%Y.%m.%d."))]

# 14 dienu MA
dat[, y := pracma::movavg(x = ApstiprinataCOVID19InfekcijaSkaits,
                          n = 14, type = "s")]
dat[, .(Datums, ApstiprinataCOVID19InfekcijaSkaits, y)]

dat[, diff := shift(x = y, n = -1) - shift(x = y, n = 1)]
dat[, .(Datums, ApstiprinataCOVID19InfekcijaSkaits, y, diff)]

dat2 <- dat[!is.na(diff), .(Datums, y, diff)]

dat3 <- rbindlist(lapply(dat2$Datums,
                         function(x) dat2[Datums <= x][, dat_ped := x][]))
dat3

p <- ggplot(data = dat3, mapping = aes(x = diff, y = y,
                                       frame = dat_ped, ids = Datums)) +
  geom_path() +
  geom_point(mapping = aes(size = y, colour = diff)) +
  geom_point(mapping = aes(size = y), shape = 1) +
  scale_colour_distiller(type = "div", palette = "RdYlBu")

fig <- ggplotly(p) %>%
  animation_opts(
    frame = 200,
    transition = 100,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "datums "
    )
  )

# fig
# htmlwidgets::saveWidget(widget = fig, file = "slowdown.html")

app <- Dash$new()

app$layout(
  htmlDiv(
    list(
      dccGraph(figure = fig)
    )
  )
)

app$run_server(debug = TRUE, dev_tools_hot_reload = FALSE)
