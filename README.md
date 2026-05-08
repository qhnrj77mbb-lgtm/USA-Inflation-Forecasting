# USA Inflation Forecasting Dashboard

An interactive **R Shiny** application designed to visualize, analyze, and forecast U.S. Inflation (Consumer Price Index) using advanced econometric time-series models.

## Live Demo
**(https://8ze8mf-jorge-henao.shinyapps.io/USA_Inflatin/)**

## Overview
This project provides a dynamic interface to explore how different economic variables influence the future path of inflation. It automates data retrieval from the **Federal Reserve Economic Data (FRED) API** and implements two primary forecasting methodologies:

### Modeling Approaches
1. **ARIMA (Univariate):** A baseline model that predicts future inflation based solely on its own historical patterns and trends.
2. **ARIMAX (Multivariate):** An advanced model that incorporates key macroeconomic exogenous variables:
   - **Unemployment Rate:** Analyzing the Phillips Curve relationship.
   - **Interest Rates (Fed Funds Rate):** Measuring monetary policy impact.
   - **Oil Prices:** Capturing supply-side energy shocks.
   - **M2 Money Stock:** Tracking monetary expansion.

## Key Features
- **Dynamic Model Selection:** Switch between ARIMA and ARIMAX instantly.
- **Custom Regressors:** Select specific exogenous variables to see how they change the prediction.
- **Flexible Horizon:** Forecast from 6 to 48 months ahead.
- **Interactive Timeline:** Adjust the date range to focus on specific economic periods (e.g., 2008 Financial Crisis or 2020 Pandemic).

## Dashboard Preview
<img width="1510" height="863" alt="Screenshot 2026-05-08 at 5 36 40 PM" src="https://github.com/user-attachments/assets/d49dae83-2fba-4de8-86f2-5eadd16c12e4" />

## Tech Stack
- **Language:** R
- **Web Framework:** Shiny & bslib
- **Econometrics:** `forecast` (ARIMA/ARIMAX), `tseries`, `urca`
- **Data Engineering:** `fredr` (API connection), `tidyverse` (dplyr, purrr)
- **Visualization:** `ggplot2` & `plotly`

## Installation & Local Run
To run this dashboard locally, ensure you have an [API Key from FRED](https://stlouisfed.org) and run:

```r
# Install required packages
install.packages(c("shiny", "fredr", "dplyr", "forecast", "ggplot2", "lubridate", "bslib"))

```

---
**Developed by:** Jorge Henao
