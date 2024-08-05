
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Initialise the columns for daily returns in df
df$AMD_dailyreturns <- NA
df$GSPC_dailyreturns <- NA

# Calculate the respective daily returns following the given formula
for (i in 2:(nrow(df))) {
  df$AMD_dailyreturns[i] <- (df$AMD[i] - df$AMD[i-1])/df$AMD[i-1]
  df$GSPC_dailyreturns[i] <- (df$GSPC[i] - df$GSPC[i-1])/df$GSPC[i-1]
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# Initialise the column for daily risk-free rates in df
df$daily_riskfreerate <- NA

# Calculate the daily risk-free rates following the given formula
df$daily_riskfreerate[1] <- (1 + df$RF[1]/100)^(1/360) - 1
for (i in 2:(nrow(df))) {
  df$daily_riskfreerate[i] <- (1 + df$RF[i]/100)^(1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Initialise the columns for excess returns in df
df$AMD_excessreturns <- NA
df$GSPC_excessreturns <- NA

# Calculate the respective excess returns by following the instructions
for (i in 2:(nrow(df))) {
  df$AMD_excessreturns[i] <- df$AMD_dailyreturns[i] - df$daily_riskfreerate[i]
  df$GSPC_excessreturns[i] <- df$GSPC_dailyreturns[i] - df$daily_riskfreerate[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# Perform linear regression analysis
Model <- lm(AMD_excessreturns~GSPC_excessreturns,data=df)
summary(Model)

# Set beta as the slope of the CAPM regression line
beta <- summary(Model)$coefficient[2,1]
print(beta)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The beta, according to the summary, is 1.57 to two decimal places. Because beta > 1, AMD has more systematic risk than the overall market, therefore AMD is more volatile than the market. This implies higher investment risk. Additionally, with higher systematic risk comes greater potential returns for investing in AMD.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Plot GSPC excess returns against AMD excess returns
plot <- ggplot(df,aes(x=GSPC_excesreturns,y=AMD_excessreturns)) +
  geom_point() + # Plots the scatterplot
  geom_smooth(method='lm',col="red") # PLots the CAPM regression line
  labs(title = "Excess returns")
plot
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**
The 90% prediction interval is (-50%,86%).

```r
# Intialise variables
GSPC_annualexpectedreturn <- 0.133
Current_riskfreerate <- 0.05
beta <- summary(Model)$coefficient[2,1]

# Calculate the number of observations
n <- length(df$GSPC_dailyreturns)

# Calculate the forecasted value 
X_f <- GSPC_annualexpectedreturn/252 - Current_riskfreerate

# Calculate the mean of GSPC excess returns
mean_GSPCexcessreturns <- mean(as.numeric(df$GSPC_excessreturns), na.rm=TRUE)

# Calculate the standard error
se <- sqrt(sum(residuals(Model)^2)/(n-1-1))

# Calculate the Sum of Squares of GSPC excess returns
SSX <- sum((as.numeric(df$GSPC_excessreturns) - mean_GSPCexcessreturns)^2, 
           na.rm=TRUE)

# Calculate the daily standard error of forecast
daily_sf <- se * sqrt(1 + 1/n + (X_f - mean_GSPCexcessreturns)^2/SSX)

# Calculate the annual standard error following the hint
annual_se <- daily_sf * sqrt(252)

# Calculate the annual standard error of forecast
annual_sf <- annual_se * sqrt(1 + 1/n + (X_f - mean_GSPCexcessreturns)^2/SSX)

# Set the significance level to produce a 90% prediction interval, two-sided
alpha <- 0.10

# Set the t-value
t_value <- qt(1 - alpha/2,df=n-2)

# Calculate the estimated annual AMD returns
Estimated_annualAMDreturns <- Current_riskfreerate + beta * 
  (GSPC_annualexpectedreturn - Current_riskfreerate)

# Produce the 90$ prediction interval, two-sided
lower_bound <- Estimated_annualAMDreturns - t_value * annual_sf
upper_bound <- Estimated_annualAMDreturns + t_value * annual_sf
cat(round(lower_bound, 2), round(upper_bound, 2))
```
