
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Set up the first condition (if previous price = 0)
if (previous_price==0) {
    amd_df$trade_type[1] <- "buy" # setting the trade_type
    amd_df$costs_proceeds[1] <- amd_df[1,2] * -share_size 
    # setting the cost_proceeds, ensuring the result is negative
    accumulated_shares <- accumulated_shares + share_size 
    # redefining accumulated_shares
    amd_df$accumulated_shares[1] <- accumulated_shares 
    # assigning 100 shares to accumulated_shares
}else { # If you do not buy, there are 0 costs_proceeds and 0 accumulated shares
  amd_df$trade_type[1] <- ""
  amd_df$costs_proceeds[1] <- 0
  amd_df$accumulated_shares[1] <- 0
}

# Set up the second condition (if the price of the current day is less than that of the previous day)

# This part of the code follows similarly to the previous part of the code under the first condition
for (i in 2:(nrow(amd_df)-1)) {
  if (amd_df$close[i] < amd_df$close[i-1]) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- amd_df$close[i] * -share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }else {
  amd_df$trade_type[i] <- ""
  amd_df$costs_proceeds[i] <- 0
  amd_df$accumulated_shares[i] <- accumulated_shares
  }
}

# On the last day of trading
amd_df$trade_type[nrow(amd_df)] <- "sell"
amd_df$costs_proceeds[nrow(amd_df)] <- amd_df$close[nrow(amd_df)] * amd_df$accumulated_shares[nrow(amd_df)-1] 
# calculating the total costs_proceed by multiplying the share price with the total accumulated shares
# when selling shares, costs_proceeds will now be positive 
# buying shares warrants a negative value, selling shares warrants a positive value
amd_df$accumulated_shares[nrow(amd_df)] <- 0 
# selling all shares means there are 0 accumulated shares
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# The trading period will be defined as 2023-05-17 to 2024-05-17.

start_date <- "2023-05-17"
end_date <- "2024-05-17"
start_period <- which(amd_df$date == start_date)
end_period <- which(amd_df$date == end_date)
amd_df <- amd_df[c(start_period:end_period), ] 
# redefining amd_df for the trading period
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Before commencing Step 4, please ensure you have run the code in the following order: Step 1, Step 2, Step 3, Step 2 to calculate the correct results for the first strategy.

# If you have run Step 5, just run Step 4 to calculate the correct results for the profit-taking strategy.

# Total Profit/Loss calculation
total_profit_or_loss <- sum(amd_df$costs_proceeds)
print(total_profit_or_loss) # to show the value in console

# Invested capital
invested_capital <- 0 
# initialising invested capital as 0

for (i in 1:nrow(amd_df)) {
  if (amd_df$trade_type[i] == "buy") {
    invested_capital <- invested_capital - amd_df$costs_proceeds[i] 
    # taking the negative sum by substracting costs_proceeds from invested_capital
  }else {
    invested_capital <- invested_capital
  }
}
print(invested_capital)

#ROI calculation
ROI <- (total_profit_or_loss/invested_capital) * 100
print(ROI)
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Profit-taking strategy

# Before commencing Step 5, please ensure you have run the code in the following steps: 
# Step 1, Step 2, Step 3, Step 2

# Selling half of the shares when the price increases by 20% above of the average purchase price
for (i in 2:(nrow(amd_df)-1)) {
  if(amd_df$close[i] > mean(amd_df$close[1]:amd_df$close[i-1]) * 1.2) {
    amd_df$trade_type[i] <- "sell"
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] * 0.5 # selling half of the shares
    if (amd_df$trade_type[i+1] == "buy") {
      amd_df$accumulated_shares[i+1] <- amd_df$accumulated_shares[i] + share_size
    }else {
      amd_df$accumulated_shares[i+1] <- amd_df$accumulated_shares[i]
    }
  }else { # when the price of the current day is below the price of the previous day
    if (amd_df$close[i] < amd_df$close[i-1]) {
      amd_df$trade_type[i] <- "buy"
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    }else {
      amd_df$trade_type[i]<-""
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
    }
  }
}

for (i in 1:(nrow(amd_df)-1)) {
  if (amd_df$trade_type[i] == "buy") {
    amd_df$costs_proceeds[i] <- amd_df$close[i] * -share_size
  }else {
    if (amd_df$trade_type[i] == "sell") {
      amd_df$costs_proceeds[i] <- amd_df$close[i] * (amd_df$accumulated_shares[i-1] - amd_df$accumulated_shares[i]) 
      # when selling shares, costs_proceeds will now be positive 
      # buying shares warrants a negative value, selling shares warrants a positive value
    }else {
      amd_df$costs_proceeds[i] <- 0
    }
  }
}

# On the last day of trading, as done before with the previous strategy 
amd_df$trade_type[nrow(amd_df)] <- "sell"
amd_df$costs_proceeds[nrow(amd_df)] <- amd_df$close[nrow(amd_df)] * amd_df$accumulated_shares[nrow(amd_df)-1] 
# when selling shares, costs_proceeds will now be positive 
# buying shares warrants a negative value, selling shares warrants a positive value
amd_df$accumulated_shares[nrow(amd_df)] <- 0
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# To calculate the P/L and ROI following the profit-taking strategy, please run Step 4 after running Step 5.

# From 2023-05-17 to 2024-05-17: Before the profit-taking strategy, the P/L and ROI was $377,010 and 22.9% (1dp) respectively. After the Profit-taking strategy, the P/L and ROI was $326,709.90 and 29.8% (1dp) respectively. Therefore, the P/L did not improve and the ROI did improve over the chosen period. 

# 2023: The share prices fluctuated in the $90-120 range, resulting in only shares being bought as the share price never soared above 120% of the average purchase price. 

# 2024: The share price was on a steady incline, increasing above $120 to reach $211.383 on 2024-03-07 before gradually plummeting down to the $150-160 range in May. The initial surge in share price derived from investors' increased interest in artificial intelligence (AI) development, resulting in further investment into AMD's AI chips. The later decline in share price reflected investors' disappointment in AMD for failing to meet their forecast of $4 billion in AI chip sales. Nonetheless, thanks to the initial incline, 2024-01-08 was when the profit-taking strategy initiated as a result of the soaring share prices. Therefore, through the profit-taking strategy, the ROI increased by 6.9% as we took advantage of high share prices to sell our shares at a greater profit as opposed to our first strategy. Despite the higher ROI, P/L was still lower by $50,300.10 likely due to the first strategy taking advantage of the generally higher share price on 2024-05-17, selling the accumulated shares for a greater profit. However, using the profit-taking strategy, I believe that extending the trading period beyond 2024-05-17 will yield a higher P/L than $337,010 due to the greater observed ROI of 29.8%. 
```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




