library(tidyverse)
library(fpp3)
library(BVAR)

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

liquor |> 
  autoplot(sales) +
  labs(
    title = "Monthly sales at beer, wine, and liquor stores",
    subtitle = "Data from U.S. Census Bureau's monthly retail trade survey, accessed via FRED",
    x = "Date (Month and year)", 
    y = "Retail sales, in millions of dollars") +
  theme_minimal()

liquor |> 
  gg_season(log(sales)) +
  labs(
    title = "Monthly sales at beer, wine, and liquor stores",
    subtitle = "Data from U.S. Census Bureau's monthly retail trade survey, accessed via FRED",
    x = "Month", 
    y = "Logged retail sales (originally in millions of dollars)")

# The amount of sales is trending up over time. The data are also very clearly seasonal, where the month with the highest sales is December and the month with the lowest sales is January.
# The seasonal swings do grow with the level of the series, as seen in the plot of the data in level, which suggests that modeling a logged version of the data is worth doing because logging evens out those swings across time in the series and allows us to see the real changes in the data better.

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# For the ETS model, the chosen specification was ETS(A,A,A). This means that this model has an additive trend component, an additive seasonal component, and an additive error component
# For the ARIMA model, the chosen specification was ARIMA(1,0,2)(2,1,1)[12] with drift. (I'm not sure what the "[12] w/ drift" part means.) These specifications tell us that the model contains 1 lag of our target variable, sales, 2 lags of the error term, 2 lags of the seasonal target variable, 1 lag of the seasonal error term, and that the model differenced the data seasonally one time. 

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

# The residuals do mostly look like white noise! The bar plot shows them centered on zero, and they don't seem to be autocorrelated. While there are some significant error terms, they don't seem to be connected to each other.

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

# In this TS-CV, if my math is right, around 214 forecast origins will be considered? Or maybe it's 216 instead. Somewhere around there.

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# The date of the first forecast is January 2002, and the date of the last forecast is December 2019.
# The model that forecasts the most accurately is the ARIMA model, and both the RMSE and MAE metrics agree on this.

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

# ARIMA vs ETS: DM stat = -2.9062, p-value = 0.004042. Since the p-value is less than 0.05, the difference between the models is statistically significant.
# ARIMA vs SNAIVE: DM stat = -10.755, p-value < 2.2e-16. This one is also statistically significant.

# It's clear that the ARIMA model forecasts much better than the ETS and SNAIVE models. If I were the liquor store chain, I would use ARIMA!

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# Compared to the models that go into it, combo is less accurate than ARIMA but more accurate than ETS. It's clear that ETS is inaccurate enough to make it worth dropping. 

cor(liquor_errs$arima, liquor_errs$ets)

# Since the difference in forecasting ability between ARIMA and ETS is significant, and their forecast errors are highly correlated (correlation= 0.878), it makes sense that the combination of the two models did not beat ARIMA. Not only is ETS significantly worse than ARIMA, meaning that combining it with ARIMA would only create less accurate point forecasts, the errors are highly correlated, meaning that ETS is not making up for any of the forecast errors ARIMA already has and is only making them worse.

