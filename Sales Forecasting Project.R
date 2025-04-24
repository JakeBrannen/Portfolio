
# Sales Forecasting Project

library(tidyverse)
library(caret)
library(GGally)
library(yardstick)
library(ggplot2)
library(reshape2)

sales <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/sales.csv", 
                    header = TRUE)

head(sales)

summary(sales)

sales <- sales %>% replace(is.na(.), 0)

sales %>% group_by(Region, Customer.Name) %>% summarise(Sales = Sales) %>% arrange(desc(Sales))

# -------------------------------------------------

# Sales trend over time

sales$Order.Date <- dmy(sales$Order.Date)

sales_by_date <- aggregate(Sales ~ Order.Date, sales, sum)

ggplot(sales_by_date, aes(x = Order.Date, y = Sales)) + geom_line(color = 'slateblue') +
  labs(title = 'Sales Trend Over Time', x = 'Date', y = 'Sales')

# -------------------------------------------------

library(dplyr)
library(xgboost)
library(Metrics)
library(caret)

# Function to create lagged features
create_lagged_features <- function(data, lag = 1) {
  lagged_data <- data
  for (i in 1:lag) {
    lagged_data[[paste0("lag_", i)]] <- dplyr::lag(lagged_data$Sales, i)
  }
  return(lagged_data)
}

# Assuming `data` is a data.frame with columns "Order Date" and "Sales"
lag <- 5
sales_with_lags <- create_lagged_features(sales[, c('Order.Date', "Sales")], lag)

# Drop NA rows caused by lagging
sales_with_lags <- na.omit(sales_with_lags)

# Define X and y
X <- sales_with_lags %>% select(-Order.Date, -Sales)
Y <- sales_with_lags$Sales

# Train/test split without shuffling (to preserve time order)
split_index <- floor(0.8 * nrow(X))
X_train <- X[1:split_index, ]
X_test <- X[(split_index + 1):nrow(X), ]
Y_train <- Y[1:split_index]
Y_test <- Y[(split_index + 1):length(Y)]

# Convert to matrix format for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))

# Train the model
model_xgb <- xgboost(
  data = dtrain,
  objective = "reg:squarederror",
  nrounds = 100,
  eta = 0.1,
  max_depth = 5,
  verbose = 0
)

# Make predictions
predictions_xgb <- predict(model_xgb, dtest)

# Calculate RMSE
rmse_xgb <- rmse(Y_test, predictions_xgb)

# Print RMSE
cat(sprintf("RMSE: %.2f\n", rmse_xgb))

# -----------------------------------------

library(zoo)

plot_data <- data.frame(
  Date = index(Y_test),  # or y_test$Date if it's a column
  Actual_Sales = as.numeric(Y_test),
  Predicted_Sales = as.numeric(predictions_xgb)
)

plot_data_melt <- melt(plot_data, id.vars = "Date", 
                       variable.name = "Type", 
                       value.name = "Sales")


ggplot(plot_data_melt, aes(x = Date, y = Sales, color = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "Sales Forecasting using XGBoost",
       x = "Date",
       y = "Sales") +
  theme_minimal() +
  scale_color_manual(values = c("Actual_Sales" = "slateblue", "Predicted_Sales" = "orange")) +
  theme(legend.title = element_blank())

# --------------------------------------------------

# Different Method to see monthly sales

sales <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/sales.csv", 
                  header = TRUE)

sales$Order.Date <- dmy(sales$Order.Date)

sales_date <- sales

sales_date <- separate(sales_date, col = Order.Date, into = c('Year','Month', 'Day') )

sales_date <- sales_date %>% select(Year, Month, Sales)

sales_date$Month <- as.numeric(sales_date$Month)


monthly_sales <- aggregate(Sales ~ Year + Month, data = sales_date, FUN = sum)

n <- nrow(monthly_sales)
test_size <- 0.3
test_index <- floor(n * (1 - test_size))

set.seed(2025)  # For reproducibility

train_data <- monthly_sales[1:test_index, ] %>% sample_frac(1)  # Shuffle train data
test_data  <- monthly_sales[(test_index + 1):n, ] %>% sample_frac(1)


# -------------------------------------------


x_train <- train_data[, c("Year", "Month")]
y_train <- train_data[, c('Sales')]

x_test <- test_data[, c("Year", "Month")]
y_test <- test_data[, c('Sales')]


train_df <- data.frame(Sales = y_train, x_train)

# Refit the model (optional, only if you haven't already done it this way)
train_df <- data.frame(Sales = y_train, x_train)


# Fit the linear model
model <- lm(Sales ~ Year + Month, data = train_df)

# Predict safely
y_pre <- predict(model, newdata = x_test)


# ----------------------------------------
results_df <- x_test %>%
  mutate(Actual_Sales = y_test, Predicted_Sales = y_pre) %>%
  mutate(Month_Year = paste(Year, sprintf("%02d", as.numeric(Month)), sep = "-"))

# Reshape to long format for plotting
plot_data <- results_df %>%
  select(Month_Year, Actual_Sales, Predicted_Sales) %>%
  pivot_longer(cols = c("Actual_Sales", "Predicted_Sales"),
               names_to = "Type", values_to = "Sales")

# Plot grouped bar chart
ggplot(plot_data, aes(x = Month_Year, y = Sales, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.35) +
  labs(title = "Actual vs Projected Sales", x = "Month-Year", y = "Sales") +
  scale_fill_manual(values = c("Actual_Sales" = "slateblue", "Predicted_Sales" = "springgreen")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------------


