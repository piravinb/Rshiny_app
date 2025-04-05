# Rshiny_app

Time Series Analysis Dashboard in R Shiny
📊 Overview
This is an interactive and educational Shiny web application for time series analysis. The dashboard supports graphical exploration, stationarity checks, decomposition, model fitting (ARIMA, ARCH, GARCH), diagnostics, and forecasting.

🚀 Features
Upload and analyze time series data from a CSV file

Perform EDA: handle missing and duplicate values

View ACF/PACF plots for both original and transformed data

Auto-transform data for stationarity (differencing, log transformation)

Conduct Augmented Dickey-Fuller (ADF) test

Model suggestions: AR, MA, ARIMA, ARCH, GARCH

Residual diagnostics including Ljung-Box test

Forecast future values with user-defined horizons

Download results and plots

⚙️ Getting Started
Installation Install required packages in R:

📌 Usage Instructions
Upload a CSV file containing time series data

Select the relevant time series column

Explore the data in the EDA tab

View ACF/PACF plots for raw and transformed data

Let the app apply necessary transformations automatically

Choose a model manually or use automatic suggestions

Forecast future values by setting a forecast horizon

Download the results and visualizations

📖 Interpretation Guide
EDA Missing values → imputed using mean

Duplicate entries → removed

Trend and seasonality → visualized

Stationarity ADF Test:

p < 0.05 → stationary

Otherwise, differencing/log transformation is applied

ACF/PACF Cutoff patterns help identify appropriate AR/MA orders

Model Selection auto.arima() suggests best ARIMA(p, d, q) model

ARCH/GARCH models suggested when volatility clustering is detected

Diagnostics Residuals should resemble white noise

Ljung-Box test:

p > 0.05 → no autocorrelation in residuals

Forecasting Users specify the forecast horizon

Forecasts include 95% confidence intervals

📦 Dependencies
shiny

ggplot2

forecast

tseries

rugarch

zoo

FinTS

TTR

dplyr

👥 Credits
Developed by a 5-member academic team as part of an advanced time series analysis project.
