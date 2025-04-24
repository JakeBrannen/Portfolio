
# House Price Prediction

library(tidyverse)
library(caret)
library(GGally)
library(yardstick)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(mice)
library(ggcorrplot)
library(patchwork)

house <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/HousePricePrediction.csv", 
                  header = TRUE)

summary(house)

# ------------- Cleaning the Data -----------------

house$BsmtFinSF2 <- house$BsmtFinSF2 %>% replace(is.na(.), 0)

house$TotalBsmtSF <- house$TotalBsmtSF %>% replace(is.na(.), 0)

house_impute <- house[, !colnames(house) %in% c(
                        'MSSubClass','MSZoning','LotArea','LotConfig',
                        'BldgType','OverallCond','YearBuilt','YearRemodAdd',
                        'Exterior1st','BsmtFinSF2','TotalBsmtSF')]
imp <- mice(house_impute, seed = 2025)
house_imputed <- complete(imp)

house[, !colnames(house) %in% c(
  'MSSubClass','MSZoning','LotArea','LotConfig','BldgType','OverallCond', 
  'YearBuilt','YearRemodAdd','Exterior1st','BsmtFinSF2','TotalBsmtSF')] <- house_imputed

summary(house)

# -------------- Categorize the datatypes ------------------

object_cols <- names(house)[sapply(house, function(x) is.character(x) || is.factor(x))]
cat("Categorical variables:", length(object_cols), "\n")

# Integer variables
int_cols <- names(house)[sapply(house, is.integer)]
cat("Integer variables:", length(int_cols), "\n")

# Float variables (numeric but not integer)
float_cols <- names(house)[sapply(house, function(x) is.numeric(x) && !is.integer(x))]
cat("Float variables:", length(float_cols), "\n")


# -------------- Look for Correlation -----------------

house_num <- house %>% select(where(is.numeric))

cormat <- round(cor(house_num),2)

melted_cormat <- melt(cormat)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

# ------------- Visualize the parameters --------------------

unique_values <- sapply(house[object_cols], function(col) length(unique(col)))

# Convert to data frame for plotting
unique_df <- data.frame(
  Feature = names(unique_values),
  UniqueCount = as.vector(unique_values)
)

ggplot(unique_df, aes(x = Feature, y = UniqueCount, fill = Feature)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "No. Unique values of Categorical Features",
       x = "Categorical Features",
       y = "Count of Unique Values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_list <- lapply(object_cols, function(col) {
  data <- house %>%
    group_by(.data[[col]]) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(desc(Count))
  
  ggplot(data, aes_string(x = col, y = "Count", fill = col)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = col, x = NULL, y = "Count") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title = element_text(size = 10)
    )
})

# Arrange plots in 4 columns (like 11x4 layout = 44 plots max)
wrap_plots(plot_list, ncol = 4) + plot_annotation(title = "Categorical Features: Distribution")

# ------------- Convert Categorical into Binary ----------------------


library(fastDummies)

OH_cols <- dummy_cols(house,
                      select_columns = object_cols,
                      remove_first_dummy = FALSE,    # Keep all categories (like sparse_output=False)
                      remove_selected_columns = TRUE # Drop original categorical columns
)

df_final <- OH_cols

# ----------------- Split into Train and Testing Sets -------------------

set.seed(2025)

x <- df_final %>% select(-SalePrice)

y <- df_final$SalePrice

trainindex <- createDataPartition(house$SalePrice, p = 0.8, list = FALSE)

x_train <- x[trainindex, ]
x_test <- x[-trainindex, ]
y_train <- y[trainindex]
y_test <- y[-trainindex]

x_train <- x_train %>% select(-Exterior1st_, -Exterior1st_ImStucc)
x_test <- x_test %>% select(-Exterior1st_, -Exterior1st_ImStucc)

# ------------------ Different Model Types ---------------------

library(e1071)

# SVM
model_svr <- svm(x = x_train, y = y_train, type = "eps-regression")
pred_svr <- predict(model_svr, x_test)
mape_svr <- mean(abs((y_test - pred_svr) / y_test)) * 100

# Random Forest
model_rf <- randomForest(x = x_train, y = y_train, ntree = 200)
pred_rf <- predict(model_rf, x_test)
mape_rf <- mean(abs((y_test - pred_rf) / y_test)) * 100

# Linear Regression
model_lr <- lm(y_train ~ ., data = x_train)
pred_lr <- predict(model_lr, x_test)
mape_lr <- mean(abs((y_test - pred_lr) / y_test)) * 100

# Print MAPE
cat("MAPE (SVM):", round(mape_svr, 2), "%\n")
cat("MAPE (Random Forest):", round(mape_rf, 2), "%\n")
cat("MAPE (Linear Regression):", round(mape_lr, 2), "%\n")

# Based on MAPE Random Forrest has the lowest error %, so we will be using RF

# -------------- Using the model ----------------

predict_sale_price_rf <- function(new_data) {
  # One-hot encode categorical columns
  new_encoded <- dummy_cols(new_data, select_columns = object_cols,
                            remove_selected_columns = TRUE,
                            remove_first_dummy = FALSE)
  
  # Align columns to training set
  missing_cols <- setdiff(names(x_train), names(new_encoded))
  for (col in missing_cols) {
    new_encoded[[col]] <- 0
  }
  new_encoded <- new_encoded[, names(x_train)]
  
  # Predict
  return(predict(model_rf, new_encoded))
}

# ----------- Check the Sale Price -----------------------

sample_input <- house[1, ]
sample_input$SalePrice <- NULL

# Predict
predicted_price <- predict_sale_price_rf(sample_input)
cat("Predicted SalePrice for new data:", predicted_price, "\n") 





