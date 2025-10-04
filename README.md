# Indonesia Mining Export Forecasting with ARIMA Intervention Analysis
Time series forecasting of Indonesia mining export value using ARIMA model with Pulse Intervention analysis. Built with R statistical computing. This project implements time series forecasting for Indonesia's mining export value using ARIMA model with Pulse Intervention analysis to capture the impact of January 2022 intervention event.

## 📊 Project Objectives

- Analyze and forecast Indonesia's mining export value using time series methodology
- Implement ARIMA modeling with Box-Cox transformation for non-stationary data
- Apply intervention analysis to measure the impact of external events (Pulse Intervention - January 2022)
- Compare model performance between standard ARIMA and ARIMA with intervention
- Generate 3-month ahead forecasts with visualization

## 📁 Dataset

**Source**: `satudata.kemendag.go.id`
- **Variable**: Mining Export Value (Million USD)
- **Time Period**: January 2018 - March 2025
- **Frequency**: Monthly data
- **Total Observations**: 87 months

### Data Structure
```
Nilai_Ekspor: Mining export value in Million USD
Time: Monthly observations
```

## 🔧 R Packages Required

Install the following R packages before running the analysis:

```r
# Data manipulation and string processing
install.packages("stringr")
install.packages("dplyr")
install.packages("lubridate")

# Time series analysis
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")

# Visualization
install.packages("ggplot2")
install.packages("gridExtra")
```

## 🔬 Methodology

### 1. Data Preprocessing
- Load and convert data into time series object (ts)
- Check for missing values
- Exploratory data visualization

### 2. Box-Cox Transformation
- Calculate optimal lambda using maximum likelihood estimation
- Apply Box-Cox transformation:
- Transform data to achieve variance stabilization

### 3. Stationarity Testing
- Augmented Dickey-Fuller (ADF) test for unit root detection
- First-order differencing (d=1) to achieve stationarity
- Verify stationarity post-differencing

### 4. Model Identification
- ACF (Autocorrelation Function) plot analysis
- PACF (Partial Autocorrelation Function) plot analysis
- Determine appropriate ARIMA orders (p, d, q)

### 5. Model Estimation & Selection
Evaluate three candidate models:
- **ARIMA(3,1,0)**: AR model with 3 autoregressive terms
- **ARIMA(0,1,3)**: MA model with 3 moving average terms
- **ARIMA(3,1,3)**: Full ARMA model

Selection criteria: Minimum AIC (Akaike Information Criterion)

### 6. Intervention Analysis
- **Intervention Type**: Pulse Intervention (b=0, s=0, r=0)
- **Intervention Point**: January 2022
- **Characteristics**:
  - b=0: No delay effect
  - s=0: Immediate impact
  - r=0: Temporary effect (no persistence)

### 7. Model Diagnostics
- **Ljung-Box test**: Test for residual autocorrelation
- **Kolmogorov-Smirnov test**: Test for residual normality
- Visual diagnostics: Residual plots, ACF plots, histogram

### 8. Model Comparison
- Compare AIC values
- Calculate MAPE (Mean Absolute Percentage Error)
- Evaluate fitting quality on historical data

### 9. Forecasting
- Generate 3-month ahead forecasts
- Inverse Box-Cox transformation to original scale
- Visualization with confidence intervals

## 📈 Results

### Model Performance

| Model | AIC | MAPE |
|-------|-----|------|
| ARIMA(3,1,3) | -142.4375 | 10.71% |
| ARIMA(3,1,3) + Intervention | -171.6241 | 9.56% |

**Best Model**: ARIMA(3,1,3) with Pulse Intervention

### Key Findings
- The intervention analysis successfully captured the January 2022 event impact
- Model with intervention shows improved fit compared to standard ARIMA
- Residuals pass white noise and normality tests
- 3-month forecasts indicate [trend direction from your results]

### Forecast Output
The model provides forecasts for:
- April 2025
- May 2025
- June 2025

## 🚀 How to Run

### Prerequisites
- R version ≥ 4.0.0
- RStudio (recommended)
- All required packages installed

### Step-by-Step Instructions

1. **Clone the repository**
```bash
git clone https://github.com/yourusername/indonesia-mining-export-arima-intervention.git
cd indonesia-mining-export-arima-intervention
```

2. **Prepare your data**
- Ensure `data_ekspor.csv` is in the project root directory
- Data format: CSV with column `Nilai_Ekspor` (export value in Million USD)

3. **Install required packages**
```r
# Run this in R console
source("install_packages.R")  # If you create this script
# OR install manually as listed above
```

4. **Run the analysis**
```r
# Open R or RStudio
# Set working directory to project folder
setwd("path/to/indonesia-mining-export-arima-intervention")

# Run the main script
source("Final Code.R")
```

5. **View outputs**
- Console will display model diagnostics and statistics
- Plots will appear in the plot window
- Key metrics: AIC, MAPE, coefficient tests, diagnostic tests

### Expected Outputs

The script will generate:
- ✅ Time series plots (original and transformed data)
- ✅ ACF and PACF plots
- ✅ Model comparison statistics
- ✅ Diagnostic plots (residuals, ACF of residuals, histogram)
- ✅ Fitting comparison plot
- ✅ 3-month forecast plot with historical data
- ✅ Model evaluation metrics (MAPE, AIC)

## 📊 Visualizations

The analysis produces several key visualizations:

1. **Original Time Series**: Mining export value over time
2. **Transformed Series**: After Box-Cox transformation
3. **Stationary Series**: After differencing
4. **ACF/PACF Plots**: For model identification
5. **Diagnostic Plots**: Residual analysis
6. **Fitted vs Actual**: Model performance visualization
7. **Forecast Plot**: 3-month predictions with historical context

## 📝 Project Structure

```
indonesia-mining-export-arima-intervention/
│
├── Final Code.R              # Main analysis script
├── data_ekspor.csv           # Mining export dataset
├── README.md                 # Project documentation
├── LICENSE                   # License file
│
├── output/                   # (Optional) Save plots here
│   ├── plots/
│   └── results/
│
└── docs/                     # (Optional) Additional documentation
    └── methodology.md
```

## 🔍 Technical Details

### ARIMA Order Selection
- **p**: Determined from PACF plot (significant lags)
- **d**: 1 (first-order differencing for stationarity)
- **q**: Determined from ACF plot (significant lags)

### Intervention Specification
```r
Pulse Intervention: (b=0, s=0, r=0)
- Position: January 2022 (observation 49)
- Dummy variable: 1 at intervention point, 0 elsewhere
```

### Box-Cox Transformation
```r
λ = -0.05
Transformed = (Y^λ - 1) / λ

Inverse transformation:
Original = (λ × Transformed + 1)^(1/λ)
```

## 👤 Author

**Muhammad Dafha Syahrizal**
Email: mdafhasyahrizal@gmail.com

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**Note**: This project is for educational and research purposes. The forecasts should not be used as the sole basis for business or policy decisions without further validation and expert consultation.
