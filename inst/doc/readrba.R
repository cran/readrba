## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5.6, 
  fig.height = 4, 
  fig.align = "center",
  fig.retina = 2,
  out.width = "100%",
  out.extra = 'style="border:0px;"'
)

## ----setup, results=FALSE, message=FALSE, warning=FALSE-----------------------
library(readrba)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

theme_set(theme_minimal())

## ----10yearnorun, eval=FALSE--------------------------------------------------
#  browse_rba_series("Australian government 10 year")

## ----10year, echo=FALSE-------------------------------------------------------
browse_rba_series("Australian government 10 year") %>%
  knitr::kable()

## ----bondyield, eval = F------------------------------------------------------
#  bond_yield <- read_rba(series_id = "FCMYGBAG10")

## ----include = F, eval = F----------------------------------------------------
#  bond_yield <- read_rba(series_id = "FCMYGBAG10")
#  
#  save(bond_yield, file = file.path("vignettes", "FCMYGBAG10.rda"))
#  

## ----include = F--------------------------------------------------------------
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

## ----include = F--------------------------------------------------------------
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

## ----eval = F-----------------------------------------------------------------
#  forecasts <- rba_forecasts()

## ----include = F--------------------------------------------------------------
forecasts <- rba_forecasts(refresh = FALSE)

## ----echo = F-----------------------------------------------------------------
dplyr::glimpse(forecasts)

## ----viz-unemp-forecasts------------------------------------------------------
forecasts %>%
  filter(series == "unemp_rate") %>%
  ggplot(aes(x = date, 
             y = value, 
             group = forecast_date, 
             colour = forecast_date)) +
  geom_line()

## ----change-in-forecasts,   fig.height = 6------------------------------------
# We've already created the `forecasts` object, like this: 
# forecasts <- rba_forecasts()

latest_two <- forecasts %>%
  filter(forecast_date %in% c(max(forecast_date),
                              max(forecast_date) - months(3)))
latest_two %>% 
  group_by(series_desc) %>% 
  # Only include series for which we have forecasts in both of the latest SMPs
  filter(length(unique(forecast_date)) >= 2) %>% 
  ungroup() %>% 
  mutate(forecast_date = format(forecast_date, "%b %Y")) %>%
  ggplot(aes(x = date, y = value, col = forecast_date)) +
  geom_line() +
  guides(colour = guide_legend(title = "Forecast issued: ")) +
  facet_wrap(~series_desc, scales = "free_y",
             labeller = label_wrap_gen(17)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        plot.margin = margin(),
        strip.text = element_text(size = 8)) +
  labs(subtitle = paste0("RBA's forecasts issued in ",
                         unique(latest_two$forecast_date) %>%  format("%B %Y") %>%  paste(collapse = " and ")),
       caption = "Source: RBA Statement on Monetary Policy")


