# Smart Energy Forecasting using ARIMA, SARIMA & ETS Models



##  Overview
This project focuses on forecasting **electric load demand** and **solar power generation** using advanced time series models. It provides a complete pipeline including preprocessing, decomposition, modeling, forecasting, and evaluation.
The study explores classical statistical models such as **ARIMA, SARIMA, and ETS**, along with benchmark forecasting techniques, to identify the most effective approach for energy prediction.


##  Objectives
- Analyze **trend and seasonal patterns** in energy data  
- Build forecasting models (ARIMA, SARIMA, ETS)  
- Compare models using performance metrics  
- Identify best models for load and solar forecasting  


## Key Concepts
- Time Series Forecasting  
- Trend & Seasonality Analysis  
- STL Decomposition  
- ACF & PACF Analysis  
- ARIMA / SARIMA Models  
- ETS (Error-Trend-Seasonality) Models  
- Residual Diagnostics  


##  Dataset Description
- Source: Time Series Load & Solar Dataset :contentReference[oaicite:0]{index=0}  
- Frequency: Hourly → Converted to Daily  
- Features:
  - `Load Demand`
  - `Solar Generation`
  - `Timestamp`  


##  Methodology

### 🔹 Data Processing
- Data cleaning & preprocessing  
- Aggregation (Hourly → Daily)  

### 🔹 Analysis
- STL Decomposition  
- Seasonality & Trend analysis  
- ACF & PACF plots  

### 🔹 Modeling
- Benchmark Models (Mean, Naive, Drift)  
- ARIMA & SARIMA  
- ETS Models  

### 🔹 Evaluation
- RMSE  
- MAE  
- MAPE  

##  Models Used

### 🔹 ARIMA
- Captures autoregressive & moving average components  
- Works well for load forecasting  

### 🔹 SARIMA
- Extends ARIMA with seasonal components  
- Handles periodic patterns  

### 🔹 ETS
- Uses exponential smoothing  
- Best for strong seasonal data (solar generation)  

##  Results & Insights
-  ARIMA performed best for **load forecasting**  
-  ETS performed best for **solar generation**  
-  Models successfully captured trend & seasonality  
-  Residuals passed statistical tests → good model fit  

---

##  Residual Diagnostics
- Ljung–Box test confirms:
  - No autocorrelation in residuals  
  - Models are statistically reliable  

##  How to Run

###  Clone Repository
```bash
git clone https://github.com/your-username/energy-forecasting.git
