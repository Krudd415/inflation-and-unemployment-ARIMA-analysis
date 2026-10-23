
## Introduction

The objective of this report is to provide a comprehensive statistical analysis of the

properties of the Unemployment Rate and inflation rate in Ireland to establish an understanding

regarding fiscal and monetary policy. Utilizing the Box-Jenkins methodology, this analysis

identifies the underlying stochastic process that best characterizes the data's movement from

January 1983 through early 2026. As a junior research analyst at the International Monetary

Fund, the primary goal of this report is to evaluate the stability, persistence, and underlying

stochastic properties of two critical economic indicators, the unemployment rate and the inflation

rate. By utilizing the Box-Jenkins methodology, a robust framework time-series analysis, this

paper identifies the optimal ARMA specifications that govern Irish economic dynamics over the

last several decades. The following analysis provides the statistical foundation necessary for

evidence-based forecasting and monetary policy assessment. The analysis of the Irish labor

market utilizes the seasonally adjusted unemployment rate for all persons spanning from 1983

through 2026. The identification process began with an evaluation of the series’ integration

properties. Because the raw unemployment rate exhibited a clear trend, the Augmented

Dickey-Fuller test was used to confirm that the series is non-stationary, needing first-differencing

to achieve stationarity. This transformation ensures that the autoregressive and moving average

components are estimated on a series with a constant mean and variance, satisfying the core

assumptions of the Box-Jenkins process.

To assess price stability, the report examines the Total Consumer Price Index for Ireland,

reported as an annual growth rate. Unlike the unemployment rate, the inflation data is processed

as stationary. The data cleaning process involved addressing a historical shift in reporting


frequency, moving from quarterly observations in the mid-20th century to a consistent monthly

frequency in the modern era. The methodology involved the principle of parsimony, seeking a

model that maximizes explanatory power while minimizing unnecessary complexity.

The findings for Ireland’s unemployment rate indicate a high degree of sticky

unemployment effects, where shocks to employment tend to persist long after the initial

economic disturbance has passed. The preferred specification was identified as an ARMA(4, 1,

1) model. This structure suggests that current changes in the unemployment rate are heavily

influenced by factors spanning the previous four months, alongside a moving average component

that accounts for the immediate value of random shocks. While the model provides a strong

statistical fit, diagnostic checks suggest that the residuals still contain minor correlation, likely

reflecting structural breaks such as the 2008 financial crisis.

The analysis of Irish inflation reveals a series defined by extreme persistence. The

optimal model was identified as an ARMA(1, 0, 1) specification. The autoregressive coefficient

demonstrates that inflationary expectations in Ireland are remarkably sticky, decaying at an

exceptionally slow rate. This parsimonious model successfully cleared the diagnostic stage,

yielding an acceptably high p-value . This result confirms that the residuals are statistically

indistinguishable from white noise, indicating that the ARMA(1, 0, 1) effectively captures the

underlying price dynamics of the Irish economy while avoiding the overestimating.


## Data

Below are 4 figures visualizing data referenced throughout the paper. All data is sourced from

Federal Reserve Bank of St. Louis specifically FRED. All figures reproducible though the

attached R scripts:

*Figure Ireland Harmonized Unemployment Rate Raw Data*


*Figure 2: Ireland Annual Inflation Rate (1956-2026) Source: FRED Series CPALTTO1IEM659N*


## Residuals from ARIMA(1,0,1) with non-zero mean

*Figure 3: First-Differenced Ireland Unemployment Rate*


## Preferred specification

## Unemployment

The initial identification stage involved a visual inspection of the raw harmonized

unemployment series from 1983 to 2026 (Figure 1). While the Augmented Dickey-Fuller test

yielded ambiguous results regarding stationarity, the clear presence of a trend over time and

shifting mean meant a first-differencing transformation was needed to stabilize the series. The

resulting differenced series (Figure 3) demonstrated a constant mean, satisfying the prerequisite

for the needed modeling.

Three primary specifications were estimated for the differenced data: a parsimonious

ARMA(1,1,0), an ARMA(0,1,1), and a complex seasonal model identified via an automated

selection function. The models were compared using the Akaike Information Criterion, which

rewards goodness-of-fit while penalizing expensive parameters.

The preferred specification is an ARMA(4,1,1) with a non-zero mean. This model was

selected because it yielded the lowest AIC, suggesting it captures the highest proportion of the

data's variance among the tested candidates. The inclusion of four autoregressive lags reflects the


high degree of persistence in the Irish labor market, where changes in unemployment levels are

strongly influenced by the last quarter's factors.

The ARIMA coefficients for unemployment are as follows:

| Coefficient Estimate Std_Error |   |
| --- | --- |
| ar1 | ar1 1.2665 0.0621 |
| ar2 | ar2 -0.2786 0.0725 |
| ar3 | ar3 -0.3614 0.0680 |
| ar4 | ar4 0.3234 0.0427 |
| ma1 | ma1 -0.7279 0.0538 |

This table presents the parameter estimates for the ARIMA(4, 1, 1) model, which serves

as the preferred specification for the Irish unemployment rate. Each coefficient is statistically

significant at the 1% level, as evidenced by t-statistics that far exceed the critical threshold of

2.58. The high t-statistics confirm that each lag included in the model provides unique

explanatory power. For example, the ar1 coefficient possesses a t-statistic of approximately

20.39, indicating that the immediate previous month's labor market performance is a near-certain

predictor of current trends. Similarly, the ma1 term t = -13.53 demonstrates that random shocks

to the labor market have a significant and immediate smoothing effect on the series volatility.

The large positive ar1 (1.2665) combined with the significant higher-order lags (ar2, ar3, ar4)

suggests that shocks to unemployment do not dissipate quickly. Instead, they flow through the

economy for at least a four-month cycle. This implies that once unemployment begins to rise it


possesses significant momentum that is difficult to reverse through short-term policy alone. The

negative ma1 (-0.7279) indicates a correctional mechanism. While the AR terms represent the

persistence of the trend, the MA term suggests that the economy partially offsets the impact of

unexpected random shocks in the following period.

## Inflation

The identification of the preferred specification for the annual growth rate of the Irish

CPI followed an intuitive estimation process. Because the data was provided as a growth rate

rather than a raw index, visual inspection and the Augmented Dickey-Fuller test confirmed that

the series was stationary in its levels, requiring no further painstaking differencing, unlike

unemployment.

Three primary candidates were evaluated: a pure autoregressive AR(1) model, a moving

average MA(1) model, and an automated selection algorithm. While a complex Seasonal ARMA

model initially achieved the lowest AIC, the ARMA(1,0,1) was ultimately selected for its robust

performance across both the Akaike Information Criterion (AIC) and Bayesian Information

Criterion (BIC).


The ARMA coefficients for inflation are as follows:

| Coefficient | Estimate Std_Error |
| --- | --- |
| ar1 | ar1 0.9824 0.0068 |
| ma1 | ma1 0.1355 0.0387 |
|   | intercept intercept 4.6097 1.9841 |

This implies that inflation in Ireland has a very long memory. The ar1 value of 0.98 is the

most important part, it means inflation is extremely sticky. If prices are rising today, they are

nearly 98% likely to keep rising at a similar pace tomorrow, it takes a long time to slow down or

change direction. The ma1 value (0.13) shows that while random shocks, like a sudden spike in

global energy prices, do hit the economy, their immediate carryover effect is relatively small.

Finally, the Intercept acts as the baseline for the model, representing the natural average rate that

inflation would eventually settle at if there were no new shocks or momentum pushing it

elsewhere.

## Limitations

## Unemployment

The primary technical weakness of the preferred specification is revealed by the

Ljung-Box test for residual white noise. The test yielded a p-value of 0.007402, which is below


the standard 0.05 threshold. This result mandates a rejection of the null hypothesis that the

residuals are independent, implying that the model has not captured 100% of the correlation in

the data. This remaining correlation likely stems from the high-frequency monthly nature of the

data, which may contain seasonal nuances or high-order lags that even a four-lag AR process

cannot fully resolve.

A significant limitation of applying a linear ARMA model to Irish data from 1983 to

2026 is the assumption of parameter stability. Ireland has experienced multiple distinct economic

regimes during this 43-year period, including the Celtic Tiger expansion, the 2008 global

financial crisis, and the post-pandemic recovery. It is highly probable that the underlying

relationship between past and current unemployment ( coefficient’s ar1 through ar4) has shifted

across these eras. A single linear model assumes these coefficients are constant over time, which

may lead to diminished predictive accuracy during periods of transition.

## Inflation

While the ARMA(1,0,1) specification passes the formal Ljung-Box diagnostic, the

p-value of 0.09 is relatively marginal. This suggests that while the model is technically correct

under standard 5% significance thresholds, the residuals may still hover near the boundary of

correlation. This marginality is likely a result of the extreme length of the time series

(1956–2026). It's easy to recognize that a single linear model attempting to bridge 70 years of

varied economic regimes, including the high-volatility 1970s and the low-inflation 2010s, will


naturally face challenges in achieving a perfect fit. The high autoregressive coefficient ( ar1 =

0.9824) also signals a shifty process, suggesting that while the series is stationary, it possesses

such high volatility that it may behave like a non-stationary random series in the short term.
