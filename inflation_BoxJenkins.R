
install.packages(c("tidyverse", "tseries", "forecast", "ggfortify"))
library(tidyverse)
library(tseries)
library(forecast)
library(ggfortify)
library(zoo)


#LOAD DATA
inf_data <- read_csv("C:/Users/Keato/Downloads/Econ112 WA1/CPALTT01IEM659N.csv") %>%
  filter(!is.na(CPALTT01IEM659N)) %>% # Removes the ~554 empty months
  mutate(date = as.Date(observation_date)) %>%
  arrange(date)

inf_rate_ts <- ts(inf_data$CPALTT01IEM659N, start = c(1956, 1), frequency = 4)

ggplot(inf_data, aes(x = date, y = CPALTT01IEM659N)) +
  geom_line() +
  labs(title = "Figure 2: Ireland Annual Inflation Rate (1956-2026)",
       subtitle = "Source: FRED Series CPALTT01IEM659N",
       y = "Annual Growth Rate (%)", x = "Year") +
  theme_minimal()

#ANALYZE DATA
# ESTIMATION & MODEL SELECTION
adf_result <- adf.test(inf_rate_ts)
print(adf_result) #it is stationary

fit_inf_ar1  <- Arima(inf_rate_ts, order = c(1,0,0))
fit_inf_ma1  <- Arima(inf_rate_ts, order = c(0,0,1))
fit_inf_auto <- auto.arima(inf_rate_ts)
fit_inf_simple <- Arima(inf_rate_ts, order = c(1,0,1))
# Generate AIC and BIC Comparison Table
aic_values <- c(AIC(fit_inf_ar1), AIC(fit_inf_ma1), AIC(fit_inf_auto))
bic_values <- c(BIC(fit_inf_ar1), BIC(fit_inf_ma1), BIC(fit_inf_auto))
model_names <- c("ARIMA(1,0,0)", "ARIMA(0,0,1)", "Auto-Selected Model")
inf_model_comp <- data.frame(
  Model = model_names,
  AIC = aic_values,
  BIC = bic_values
)
print(inf_model_comp)
# fit_inf_simple is the best for this analysis


# DIAGNOSTICS
checkresiduals(fit_inf_simple) #p-value checks out

# FORMATTED COEFFICIENTS FOR REPORT
# Use this to create your clean table in Word/LaTeX
inf_final_table <- data.frame(
  Coefficient = names(coef(fit_inf_simple)),
  Estimate = round(as.numeric(coef(fit_inf_simple)), 4),
  Std_Error = round(sqrt(diag(vcov(fit_inf_simple))), 4)
)
print("--- FINAL PARAMETER ESTIMATES ---")
print(inf_final_table)