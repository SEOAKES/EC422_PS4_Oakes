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

