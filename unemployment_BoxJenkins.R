
install.packages(c("tidyverse", "tseries", "forecast", "ggfortify"))
library(tidyverse)
library(tseries)
library(forecast)
library(ggfortify) # For clean ggplot2 time series plots
library(zoo)

# 1. LOAD DATA 
data <- read_csv("C:/Users/Keato/Downloads/Econ112 WA1/LRHUTTTTIEM156S.csv")%>% 
  mutate(date = as.Date(observation_date)) %>%
  arrange(date)

unemp_ts <- ts(data$LRHUTTTTIEM156S, start = c(1983, 1), frequency = 12)

# 2. IDENTIFICATION STAGE
summary(unemp_ts)

# raw data graph
autoplot(unemp_ts) +
  labs(title = "Figure 1: Ireland Harmonized Unemployment Rate (1983-2026)",
       subtitle = "Raw Data",
       y = "Unemployment Rate (%)", x = "Year") +
  theme_minimal()

# Test for Stationarity (ADF Test)
adf_result <- adf.test(unemp_ts)
print(adf_result)
# this result implies non-stationarity

# Take the first difference (d=1) to achieve stationarity
unemp_diff <- diff(unemp_ts)

# Plot ACF and PACF for identification
ggtsdisplay(unemp_diff, main = "Figure 2: ACF and PACF of Differenced Unemployment Data")

autoplot(unemp_diff) + 
  labs(title = "Figure 3: First-Differenced Ireland Unemployment Rate",
       y = "Change in Rate (%)", x = "Year")


fit_arima_110 <- Arima(unemp_ts, order = c(1,1,0))
fit_arima_011 <- Arima(unemp_ts, order = c(0,1,1))
fit_auto_new  <- auto.arima(unemp_ts, d=1)
fit_seasonal <- auto.arima(unemp_ts, d=1, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
AIC(fit_arima_110, fit_arima_011, fit_auto_new, fit_seasonal)

# 3 DIAGNOSTICS

checkresiduals(fit_seasonal) #checked several models until fit_seasonal gave highest p-value

# 4 FORMATTED OUTPUT FOR REPORT
# DO NOT report raw R output
# Use this to create a clean table in Word/LaTeX
final_table <- data.frame(
  Coefficient = names(coef(fit_seasonal)),
  Estimate = round(as.numeric(coef(fit_seasonal)), 4),
  Std_Error = round(sqrt(diag(vcov(fit_seasonal))), 4)
)
print(final_table)