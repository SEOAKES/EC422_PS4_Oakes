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

qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

# We will produce 119 forecasts for each variable. The date of the first forecast is 1960 Q1, and the date of the last forecast is 2019 Q4.

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# It looks like the VAR(1) model forecasts better for each of the eight variable except inflation, where there is only a very small difference between the VAR(1) and VAR(4). The smaller model probably forecasts better because it has fewer parameters and thus fewer chances to make estimation errors that drives up the RMSE.

# lambda = 0.3

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

# lambda = 0.05

set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)
saveRDS(fc_bvar_005, "results/fc_bvar_005.rds")

# Results in a kable

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

# BVAR(4) when lambda = 0.3 forecasts better for all variable when compared to VAR(4). What explains this difference is BVAR shrinking down the effects of each variable by biasing them towards zero, thus shaving away some of the estimation bias that may have come from fitting meaningless noise.
# However, BVAR(4) is still less accurate overall than VAR(1), except for the real investment, inflation, 10-year Treasury yield variables, and the differences between these are relatively smaller except for the real investment variable.
# For the most part, shrinking the variables more aggressively in the BVAR model created forecasts that were more accurate then the VAR(4) model, but the tighter lambda was actually less accurate that the looser lambda and the VAR(1). It seems that just shrinking more aggressively does not create a more accurate model for these data. Though I don't know if I'd say that the winner's RMSE here would be a fair estimate of how accurate it will forecast in the future... elaborate.

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# The difference in forecasting errors for GDP growth between VAR(4) and BVAR(4) when lambda = 0.3 is significant. 

e_var4 <- actual[, "unrate"] - fc_var4[, 6]
e_bvar <- actual[, "unrate"] - fc_bvar_03[, 6]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# However, the difference for unemployment rate growth between the two is NOT significant at the 5% or 1% significance level (it would be at the 10% significance level). This tells me that one model is not consistently better than the other, and that their errors might not be completely correlated. Since there were even more inconsistencies between other models, I'd want to check each one. Additionally, it might not be a bad idea to try to combine the results of a few models and see if they can beat out VAR(1).

md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

# INDPRO was transformed into a growth rate. 
# PAYEMS was also transformed into a growth rate.
# CPIAUCSL was transformed into the change in the growth rate.
# FEDFUNDS was transformed by taking the first difference.
# Why is the transformation different for FEDFUNDS and INDPRO? I guess FEDFUNDS just needed its trend taken out and doesn't have any seasonal swings that would make the data harder to parse. INDPRO, on the other hand, definitely has seasonal swings that would grow with the level of the data, so logging before differencing makes sense.

Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]

# The first three factors explain about 0.150 + 0.074 + 0.071 = 0.295, about 29.5% of the variation in the data.

F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

# The series most correlated with the first two factors are: CUSR0000SAC, DNDGRG3M086SBEA, CUSR0000SA0L2, CPIAUCSL, CUSR0000SA0L5, CPITRNSL, PCEPI, and CPIULFSL.
# For the Quarto document, add what these series are and describe their connection to the plot and what it is the factors are capturing from them.

ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

# I did get the same numbers: RMSE for regular AR(1) = 0.617 and RMSE with factors = 0.589. It would be cheating to estimate the factors once with the full sample because that's information leakage!

direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL")

# The factors did improve the forecast of 12-month CPI inflation! From the AR to the FA, the RMSE changes by (1.89-2.11)/2.11 = -0.104, or about a 10.4% reduction in RMSE. And the difference is significantly significant too, since the p-value from the DM test is so small!

