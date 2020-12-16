## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5.6, fig.height = 4, fig.align = "center",fig.retina = 2,
  out.width = "100%",
  out.extra = 'style="border:0px;"'
)

## ----setup, results=FALSE, message=FALSE, warning=FALSE-----------------------
library(readrba)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

## ----10yearnorun, eval=FALSE--------------------------------------------------
#  browse_rba_series("Australian government 10 year")

## ----10year, echo=FALSE-------------------------------------------------------
browse_rba_series("Australian government 10 year") %>%
  knitr::kable()

## ----bondyield, eval = F------------------------------------------------------
#  bond_yield <- read_rba(series_id = "FCMYGBAG10")

## ---- include = F-------------------------------------------------------------
# filenames <- file.path(tempdir(), paste0("f2", c("a", "b", "c"), ".xls"))
# 
# purrr::map2(.x = c("f2", "f2"),
#             .y = c("current", "historical"),
#             .f = get_rba_urls) %>%
#   purrr::flatten_chr() %>%
#   download.file(url = ., destfile = filenames)
# 
# bond_yield <- read_rba_local(filenames)
# 
# bond_yield <- bond_yield %>%
#   dplyr::filter(series_id == "FCMYGBAG10")
# 
# save(bond_yield, file = file.path("vignettes", "FCMYGBAG10.rda"))

load("FCMYGBAG10.rda")

## ----glimpsebondyield---------------------------------------------------------
glimpse(bond_yield)

## ----vizbondyield-------------------------------------------------------------
bond_yield %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()


## ----yieldbrowse, eval=FALSE--------------------------------------------------
#  browse_rba_tables("government bond")

## ----yieldbrowse2, echo=FALSE-------------------------------------------------
browse_rba_tables("government bond") %>%
  knitr::kable()

## ----variousyields, eval = F--------------------------------------------------
#  f2 <- read_rba("f2")

## ---- include = F-------------------------------------------------------------
# tempf2 <- tempfile(fileext = ".xls")
# get_rba_urls("f2") %>%
#   download.file(destfile = tempf2)
# f2 <- read_rba_local(tempf2)
# save(f2, file = "vignettes/f2.rda", compress = TRUE, compression_level = 9)
load("f2.rda")

## ----uniquef2-----------------------------------------------------------------
unique(f2$series)

## ----filterf2-----------------------------------------------------------------
filtered_f2 <- f2 %>%
  filter(grepl("Australian Government", series) &
           !grepl("Indexed", series))

## ----vizf2--------------------------------------------------------------------
filtered_f2 %>%
  ggplot(aes(x = date, y = value, col = series)) +
  geom_line()


## -----------------------------------------------------------------------------
filtered_f2 %>%
  select(date, series, value) %>%
  spread(key = series, value = value) %>%
  mutate(spread_10_2 = `Australian Government 10 year bond` - `Australian Government 2 year bond`) %>%
  ggplot(aes(x = date, y = spread_10_2)) +
  geom_line()

## ---- eval = F----------------------------------------------------------------
#  forecasts <- rba_forecasts()

## ---- include = F-------------------------------------------------------------
forecasts <- rba_forecasts(refresh = FALSE)

## ---- echo = F----------------------------------------------------------------
dplyr::glimpse(forecasts)

## ----viz-unemp-forecasts------------------------------------------------------
forecasts %>%
  filter(series == "unemp_rate") %>%
  ggplot(aes(x = date, y = value, group = forecast_date)) +
  geom_line()

