
# STA 4990

# Project Code

# ----- Relevant packages ----- 

library(dplyr)
library(mice) # Use for NA's
library(caret)
library(yardstick)

# ----- Data Sets -----

titanic_train_kaggle <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/titanic/train.csv", 
                                 header = TRUE)

titanic_test_kaggle <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/titanic/test.csv",
                                header = TRUE)

titanic_submission_kaggle <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/titanic/gender_submission.csv",
                                header = TRUE)

convert_char_to_factor <- function(df) {
  # Identify character columns
  char_columns <- sapply(df, is.character)
  # Convert character columns to factors
  df[char_columns] <- lapply(df[char_columns], factor)
  return(df)
}

titanic_train_kaggle <- convert_char_to_factor(titanic_train_kaggle)
titanic_test_kaggle <- convert_char_to_factor(titanic_test_kaggle)
titanic_submission_kaggle <- convert_char_to_factor(titanic_submission_kaggle)

# -----


set.seed(49901)

trainIndex <- createDataPartition(titanic_train_kaggle$Survived, p = 0.65, 
                                  list = FALSE)

titanic_train <- titanic_train_kaggle[trainIndex,]  # your training set
titanic_test <- titanic_train_kaggle[-trainIndex,]  # hold out test set


# ----- Impute Data -----


titanic_train_impute <- titanic_train[,
                                      !colnames(titanic_train) %in% c(
                                        "PassengerId", "Survived","Name","Sex",
                                        "Sibsp", "Parch", "Ticket", "Fare", "Cabin",
                                        "Embarked")]
imp <- mice(titanic_train_impute, seed = 49901)
titanic_train_imputed <- complete(imp)

titanic_train[, !colnames(titanic_train) %in% c(
                              "PassengerId", "Survived","Name","Sex",
                              "Sibsp", "Parch", "Ticket", "Fare", "Cabin",
                              "Embarked")] <- titanic_train_imputed

summary(titanic_train)

titanic_test_impute <- titanic_test[,
                                      !colnames(titanic_test) %in% c(
                                        "PassengerId", "Survived","Name","Sex",
                                        "Sibsp", "Parch", "Ticket", "Fare", "Cabin",
                                        "Embarked")]
imp <- mice(titanic_test_impute, seed = 49901)
titanic_test_imputed <- complete(imp)

titanic_test[, !colnames(titanic_train) %in% c(
  "PassengerId", "Survived","Name","Sex",
  "Sibsp", "Parch", "Ticket", "Fare", "Cabin",
  "Embarked")] <- titanic_test_imputed

summary(titanic_test)

titanic_train_kaggle_impute <- titanic_train_kaggle[,
                                                  !colnames(titanic_train_kaggle) %in% c(
                                                    "PassengerId", "Survived",
                                                    "Name","Sex","Sibsp", 
                                                    "Parch", "Ticket", 
                                                    "Cabin","Embarked")]

# Perform imputation
imp <- mice(titanic_train_kaggle_impute, seed = 49901)
titanic_train_kaggle_imputed <- complete(imp)

# Use "titanic_test_kaggle_imputed" to fill in the missing data columns
titanic_train_kaggle[,
                    !colnames(titanic_train_kaggle) %in% c(
                      "PassengerId", "Survived",
                      "Name","Sex","Sibsp", 
                      "Parch", "Ticket", 
                      "Cabin","Embarked")] <- titanic_train_kaggle_imputed

summary(titanic_test_kaggle)

titanic_test_kaggle_impute <- titanic_test_kaggle[,
                                                  !colnames(titanic_test_kaggle) %in% c(
                                                    "PassengerId", "Survived",
                                                    "Name","Sex","Sibsp", 
                                                    "Parch", "Ticket", 
                                                    "Cabin","Embarked")]

# Perform imputation
imp <- mice(titanic_test_kaggle_impute, seed = 49901)
titanic_test_kaggle_imputed <- complete(imp)

# Use "titanic_test_kaggle_imputed" to fill in the missing data columns
titanic_test_kaggle[,
                    !colnames(titanic_test_kaggle) %in% c(
                      "PassengerId", "Survived",
                      "Name","Sex","Sibsp", 
                      "Parch", "Ticket", 
                      "Cabin","Embarked")] <- titanic_test_kaggle_imputed

summary(titanic_test_kaggle)

# -----
titanic_train$Survived <- as.factor(titanic_train$Survived)


titanic_train <- titanic_train %>% select(-PassengerId, -Name, -Ticket, -Embarked, -Cabin)

titanic_test$Survived <- as.factor(titanic_test$Survived)

titanic_test <- titanic_test %>% select(-PassengerId, -Name, -Ticket, -Embarked, -Cabin)

titanic_test

titanic_train_kaggle <- titanic_train_kaggle %>% select(-PassengerId, -Name, -Ticket, -Embarked, -Cabin)

titanic_test_kaggle <- titanic_test_kaggle %>% select(-Name, -Ticket, -Embarked, -Cabin)


# -----


library(GGally)
# Some pairwise plots for numeric variables


#ggpairs(titanic_train[,c('Age','Sex','Fare','Parch',
#                         'SibSp', 'Pclass',
#                         'Survived')], aes(color = Survived))

titanic_train_xform <- titanic_train

titanic_train_xform$logFare <- log(titanic_train_xform$Fare)
titanic_train_xform$logParch <- log(titanic_train_xform$Parch)
titanic_train_xform$logSibSp <- log(titanic_train_xform$SibSp)

#ggpairs(titanic_train_xform[,c('Age','Sex','logFare','logParch',
#                         'logSibSp', 'Pclass',
#                         'Survived')], aes(color = Survived))


# ----- Feature engineering -----

library(tidyverse)


# ----- Lasso and Ridge -----

K <- 3
control <- trainControl(method = "cv", number = K)

lambda_grid <- 10^seq(-4, -1, by = 0.1)  
set.seed(4821)
# Use a "-" to remove variables you don't want to use.  .*. means to use all interactions, but we want to remove PassengerId and Cabin.
fit_lasso_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid),
  trControl = control
)

plot(fit_lasso_interaction, xTrans = log10)


K <- 3
control <- trainControl(method = "cv", number = K)

lambda_grid <- 10^seq(-2,1, length = 50)  
set.seed(4821)
# Use a "-" to remove variables you don't want to use.  .*. means to use all interactions, but we want to remove PassengerId and Cabin.
fit_ridge_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = lambda_grid),
  trControl = control
)

plot(fit_ridge_interaction, xTrans = log10)




K <- 3
control <- trainControl(method = "cv", number = K)

sqrt(nrow(titanic_train))
k_grid <- seq(10, 35, length = 100)
set.seed(4821)
fit_knn <- train(
  Survived ~ .,
  data = titanic_train,
  method = "knn",
  tuneGrid = expand.grid(k = k_grid),
  trControl = control,
  preProcess = c("center","scale")
)

plot(fit_knn)


lasso_lambda <- fit_lasso_interaction$bestTune$lambda
lasso_lambda

ridge_lambda <- fit_ridge_interaction$bestTune$lambda
ridge_lambda

knn_k <- fit_knn$bestTune$k
knn_k

# ----- Fitting models with K-fold -----


K = 10
control <- trainControl(method = "cv", number = K)

k_fold_cv_seed <- 2345780

set.seed(k_fold_cv_seed)
fit_lasso_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = lasso_lambda),
  trControl = control
)

set.seed(k_fold_cv_seed)
fit_ridge_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = ridge_lambda),
  trControl = control
)


set.seed(k_fold_cv_seed)
fit_knn <- train(
  Survived ~ .,
  data = titanic_train,
  method = "knn",
  tuneGrid = expand.grid(k = knn_k),
  trControl = control,
  preProcess = c("center","scale")
)

set.seed(k_fold_cv_seed)
fit_logistic <- train(
  Survived ~ .,
  data = titanic_train,
  method = "glm",
  family = "binomial",
  trControl = control,
)

# ----- Transformation -----



set.seed(k_fold_cv_seed)
fit_logistic_with_log <- train(
  Survived ~ . + log(Fare + 1) + log(Parch + 1) + log(SibSp + 1),
  data = titanic_train,
  method = "glm",
  family = "binomial",
  trControl = control,
)

set.seed(k_fold_cv_seed)
fit_qda <- train(
  Survived ~ . + log(Parch + 1) + log(SibSp + 1) + log(Fare + 1),
  data = titanic_train,
  method = "qda",
  family = "binomial",
  trControl = control
)

set.seed(k_fold_cv_seed)
fit_lda <- train(
  Survived ~ .,
  data = titanic_train,
  method = "lda",
  trControl = control
)

set.seed(k_fold_cv_seed)
fit_nb <- train(
  Survived ~ .,
  data = titanic_train,
  method = "naive_bayes",
  trControl = control,
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(usekernel=TRUE, laplace=0, adjust=c(TRUE))
)

acc_cv10 <- c(
  fit_lasso_interaction$results$Accuracy,
  fit_ridge_interaction$results$Accuracy,
  fit_knn$results$Accuracy,
  fit_logistic$results$Accuracy,
  fit_logistic_with_log$results$Accuracy,
  fit_qda$results$Accuracy,
  fit_lda$results$Accuracy,
  fit_nb$results$Accuracy
)

acc_cv10

# ----- Now with boot -----

control <- trainControl(method = "boot")

boot_seed <- 8007

set.seed(boot_seed)
fit_lasso_interaction <- train(
  Survived ~ .,
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = lasso_lambda),
  trControl = control
)

set.seed(boot_seed)
fit_ridge_interaction <- train(
  Survived ~ .,
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = ridge_lambda),
  trControl = control
)


set.seed(boot_seed)
fit_knn <- train(
  Survived ~ .,
  data = titanic_train,
  method = "knn",
  tuneGrid = expand.grid(k = knn_k),
  trControl = control,
  preProcess = c("center","scale")
)

set.seed(boot_seed)
fit_logistic <- train(
  Survived ~ .,
  data = titanic_train,
  method = "glm",
  family = "binomial",
  trControl = control,
)


set.seed(boot_seed)
fit_logistic_with_log <- train(
  Survived ~ . + log(Fare + 1) + log(Parch + 1) + log(SibSp + 1),
  data = titanic_train,
  method = "glm",
  family = "binomial",
  trControl = control,
)

set.seed(boot_seed)
# Had to remove "CabinDeck" due to collinearity issue
fit_qda <- train(
  Survived ~ . + log(Fare + 1) + log(Parch + 1) + log(SibSp + 1),
  data = titanic_train,
  method = "qda",
  trControl = control
)

set.seed(boot_seed)
fit_lda <- train(
  Survived ~ .,
  data = titanic_train,
  method = "lda",
  trControl = control
)

set.seed(boot_seed)
fit_nb <- train(
  Survived ~ .,
  data = titanic_train,
  method = "naive_bayes",
  trControl = control,
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(usekernel=TRUE, laplace=0, adjust=c(TRUE))
)

acc_boot <- c(
  fit_lasso_interaction$results$Accuracy,
  fit_ridge_interaction$results$Accuracy,
  fit_knn$results$Accuracy,
  fit_logistic$results$Accuracy,
  fit_logistic_with_log$results$Accuracy,
  fit_qda$results$Accuracy,
  fit_lda$results$Accuracy,
  fit_nb$results$Accuracy
)


# ----- The predictions -----

hold_out_predictions <- data.frame(
  y = titanic_test$Survived,
  yhat_lasso_interaction = predict(fit_lasso_interaction, type = "raw", newdata = titanic_test),
  yhat_ridge_interaction = predict(fit_ridge_interaction, type = "raw", newdata = titanic_test),
  yhat_knn = predict(fit_knn, type = "raw", newdata = titanic_test),
  yhat_logistic = predict(fit_logistic, type = "raw", newdata = titanic_test),
  yhat_logistic_with_log = predict(fit_logistic_with_log, type = "raw", newdata = titanic_test),
  yhat_qda = predict(fit_qda, type = "raw", newdata = titanic_test),
  yhat_lda = predict(fit_lda, type = "raw", newdata = titanic_test),
  yhat_nb = predict(fit_nb, type = "raw", newdata = titanic_test)
)

library(yardstick)
acc_test <- c(
  accuracy(hold_out_predictions,y,yhat_lasso_interaction)$.estimate,
  accuracy(hold_out_predictions,y,yhat_ridge_interaction)$.estimate,
  accuracy(hold_out_predictions,y,yhat_knn)$.estimate,
  accuracy(hold_out_predictions,y,yhat_logistic)$.estimate,
  accuracy(hold_out_predictions,y,yhat_logistic_with_log)$.estimate,
  accuracy(hold_out_predictions,y,yhat_qda)$.estimate,
  accuracy(hold_out_predictions,y,yhat_lda)$.estimate,
  accuracy(hold_out_predictions,y,yhat_nb)$.estimate
)

acc_test

results_df <- data.frame(
  name = c("Lasso_Interaction","Ridge_Interaction","kNN","Logistic","Logistic w/ Log","QDA","LDA","NB"),
  acc_cv10 = acc_cv10,
  acc_boot = acc_boot,
  acc_test = acc_test
)

print(results_df)


# ----- Finalization -----

# Models we wanna keep, Lasso, Ridge, Knn, Logistic, LDA

# Add engineering stuff

titanic_train$fare_binned <- "Low" # default to low

titanic_train$fare_binned [titanic_train$Fare > 10] <- "Medium"  # in cases where fare is higher than 10, set to medium.  This won’t affect the cases where fare is < 10.

titanic_train$fare_binned [titanic_train$Fare > 50] <- "High"  # in cases where fare is higher than 50, set to high.  This won’t affect the cases where fare is < 50.

titanic_train$fare_binned [titanic_train$Fare > 100] <- "Highest"  # in cases where fare is higher than 100, set to highest.  This won’t affect the cases where fare is < 100.

titanic_train$fare_binned <- as.factor(titanic_train$fare_binned)  # turn into factor

#Predictor for Age
titanic_train$age_binned <- "Child" # default to low

titanic_train$age_binned [titanic_train$Age > 12] <- "Adult"

titanic_train$age_binned [titanic_train$Age > 54] <- "Senior"

titanic_train$age_binned <- as.factor(titanic_train$age_binned)  # turn into factor**

# ----- test -----

titanic_test$fare_binned <- "Low" # default to low

titanic_test$fare_binned [titanic_test$Fare > 10] <- "Medium"  # in cases where fare is higher than 10, set to medium.  This won’t affect the cases where fare is < 10.

titanic_test$fare_binned [titanic_test$Fare > 50] <- "High"  # in cases where fare is higher than 50, set to high.  This won’t affect the cases where fare is < 50.

titanic_test$fare_binned [titanic_test$Fare > 100] <- "Highest"  # in cases where fare is higher than 100, set to highest.  This won’t affect the cases where fare is < 100.

titanic_test$fare_binned <- as.factor(titanic_test$fare_binned)  # turn into factor

#Predictor for Age
titanic_test$age_binned <- "Child" # default to low

titanic_test$age_binned [titanic_test$Age > 12] <- "Adult"

titanic_test$age_binned [titanic_test$Age > 54] <- "Senior"

titanic_test$age_binned <- as.factor(titanic_test$age_binned)  # turn into factor**


# 10 fold ver.

set.seed(k_fold_cv_seed)
fit_lasso_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = lasso_lambda),
  trControl = control
)

set.seed(k_fold_cv_seed)
fit_ridge_interaction <- train(
  Survived ~ (.) * (.),
  data = titanic_train,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = ridge_lambda),
  trControl = control
)

set.seed(k_fold_cv_seed)
fit_knn <- train(
  Survived ~ .,
  data = titanic_train,
  method = "knn",
  tuneGrid = expand.grid(k = knn_k),
  trControl = control,
  preProcess = c("center","scale")
)

set.seed(k_fold_cv_seed)
fit_logistic <- train(
  Survived ~ .,
  data = titanic_train,
  method = "glm",
  family = "binomial",
  trControl = control,
)


set.seed(k_fold_cv_seed)
fit_lda <- train(
  Survived ~ .,
  data = titanic_train,
  method = "lda",
  trControl = control
)

new_acc <- c(
    fit_lasso_interaction$results$Accuracy,
    fit_ridge_interaction$results$Accuracy,
    fit_knn$results$Accuracy,
    fit_logistic$results$Accuracy,
    fit_lda$results$Accuracy
  )

new_acc 
  
  
new_hold_out_predictions <- data.frame(
  y = titanic_test$Survived,
  yhat_lasso_interaction = predict(fit_lasso_interaction, type = "raw", newdata = titanic_test),
  yhat_ridge_interaction = predict(fit_ridge_interaction, type = "raw", newdata = titanic_test),
  yhat_knn = predict(fit_knn, type = "raw", newdata = titanic_test),
  yhat_logistic = predict(fit_logistic, type = "raw", newdata = titanic_test),
  yhat_lda = predict(fit_lda, type = "raw", newdata = titanic_test)
  )


new_acc_test <- c(
  accuracy(hold_out_predictions,y,yhat_lasso_interaction)$.estimate,
  accuracy(hold_out_predictions,y,yhat_ridge_interaction)$.estimate,
  accuracy(hold_out_predictions,y,yhat_knn)$.estimate,
  accuracy(hold_out_predictions,y,yhat_logistic)$.estimate,
  accuracy(hold_out_predictions,y,yhat_lda)$.estimate
)


new_results_df <- data.frame(
  name = c("Lasso_Interaction","Ridge_Interaction","kNN","Logistic","LDA"),
  new_acc = new_acc,
  new_acc_test = new_acc_test
)

print(new_results_df)

# ----- Final fitting -----


titanic_train_kaggle$fare_binned <- "Low" # default to low

titanic_train_kaggle$fare_binned [titanic_train_kaggle$Fare > 10] <- "Medium"  # in cases where fare is higher than 10, set to medium.  This won’t affect the cases where fare is < 10.

titanic_train_kaggle$fare_binned [titanic_train_kaggle$Fare > 50] <- "High"  # in cases where fare is higher than 50, set to high.  This won’t affect the cases where fare is < 50.

titanic_train_kaggle$fare_binned [titanic_train_kaggle$Fare > 100] <- "Highest"  # in cases where fare is higher than 100, set to highest.  This won’t affect the cases where fare is < 100.

titanic_train_kaggle$fare_binned <- as.factor(titanic_train_kaggle$fare_binned)  # turn into factor

#Predictor for Age
titanic_train_kaggle$age_binned <- "Child" # default to low

titanic_train_kaggle$age_binned [titanic_train_kaggle$Age > 12] <- "Adult"

titanic_train_kaggle$age_binned [titanic_train_kaggle$Age > 54] <- "Senior"

titanic_train_kaggle$age_binned <- as.factor(titanic_train_kaggle$age_binned)  # turn into factor**

# ----- test -----

titanic_test_kaggle$fare_binned <- "Low" # default to low

titanic_test_kaggle$fare_binned [titanic_test_kaggle$Fare > 10] <- "Medium"  # in cases where fare is higher than 10, set to medium.  This won’t affect the cases where fare is < 10.

titanic_test_kaggle$fare_binned [titanic_test_kaggle$Fare > 50] <- "High"  # in cases where fare is higher than 50, set to high.  This won’t affect the cases where fare is < 50.

titanic_test_kaggle$fare_binned [titanic_test_kaggle$Fare > 100] <- "Highest"  # in cases where fare is higher than 100, set to highest.  This won’t affect the cases where fare is < 100.

titanic_test_kaggle$fare_binned <- as.factor(titanic_test_kaggle$fare_binned)  # turn into factor

#Predictor for Age
titanic_test_kaggle$age_binned <- "Child" # default to low

titanic_test_kaggle$age_binned [titanic_test_kaggle$Age > 12] <- "Adult"

titanic_test_kaggle$age_binned [titanic_test_kaggle$Age > 54] <- "Senior"

titanic_test_kaggle$age_binned <- as.factor(titanic_test_kaggle$age_binned)  # turn into factor**

titanic_train_kaggle$Survived <- as.factor(titanic_train_kaggle$Survived)



final_control <- trainControl(method = "cv", number = 10)

fit_final <- train(
  Survived ~ .,
  data = titanic_train_kaggle,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = ridge_lambda),
  trControl = final_control,
  preProcess = c("center", "scale")
)

predictions <- predict(fit_final, type = "raw", newdata = titanic_test_kaggle)

# Step 2: Create a new data frame for these predictions
predictions_df <- data.frame(PassengerId = titanic_test_kaggle$PassengerId, Survived = predictions)

# Step 3: Ensure correct submission format
# First, create a copy of the submission file to avoid altering the original
titanic_submission_filled <- titanic_submission_kaggle

# Then, use merge to align and update the Transported values based on PassengerId
# This step assumes titanic_submission_kaggle already has a Transported column that we want to update
titanic_submission_filled <- merge(titanic_submission_filled[, "PassengerId", drop = FALSE], predictions_df, by = "PassengerId")

# Ensure that it looks right
View(titanic_submission_filled)

# Export titanic_submission_corrected to a CSV file
write.csv(titanic_submission_filled, "kaggle_submission.csv", row.names = FALSE)






















