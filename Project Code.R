
 # Project 

 # Model Building Process/Evaluation

 df <- read.csv("C:/Users/jakeb/OneDrive/Documents/R Coding/Project Data.csv", 
                header = TRUE)

 colnames(df) <- c('Row', 'Country', 'Y', 'X1', 'X2','X3','X4','X5', 'X6')

 summary(df)
 
 # ignore project
 
 project <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, df)
 
 summary(project) 

 anova(project) 
 
 X1 = df$X1
 X2 = df$X2
 X3 = df$X3
 X4 = df$X4
 X5 = df$X5
 X6 = df$X6
 Y = df$Y
 df$Y <- Y + 0.00001
 
 summary(Y)
 summary(X1)
 summary(X2)
 summary(X3)
 summary(X4)
 summary(X5)
 summary(X6)
 
 X4p = (X4^0.3)
 
 fit <- lm(Y ~ X1 + X2 + X3 + X4p + X5 + X6, df) # use this
 
 summary(fit)
 anova(fit)
 
 
 # Outliers
 
 boxplot(Y, xlab = "Y", horizontal = TRUE, main = "Outliers in Y")
 
 boxplot(X1, xlab = "X1", horizontal = TRUE, main = "Outliers in X1") 
 
 boxplot(X2, xlab = "X2", horizontal = TRUE, main = "Outliers in X2")
 
 boxplot(X3, xlab = "X3", horizontal = TRUE, main = "Outliers in X3") 
 
 boxplot(X4, xlab = "X4", horizontal = TRUE, main = "Outliers in X4") 
 
 boxplot(X4p, xlab = "X4", horizontal = TRUE, main = "Outliers in X4p") 
 
 boxplot(X5, xlab = "X5", horizontal = TRUE, main = "Outliers in X5") 
 
 boxplot(X6, xlab = "X6", horizontal = TRUE, main = "Outliers in X6")
 
 
 # plots
 
 # 1
 
 plot(X1, Y, main = "Plot of X1 and Y")
 
 # 2
 
 plot(X2, Y, main = "Plot of X2 and Y")
 # 3
 
 plot(X3, Y, main = "Plot of X3 and Y")
 # 4
 
 plot(X4, Y, main = "Plot of X4 and Y")
 
 plot(X4p, Y, main = "Plot of X4p and Y")
 # 5
 
 plot(X5, Y, main = "Plot of X5 and Y")
 
  # 6
 
 plot(X6, Y, main = "Plot of X6 and Y")
 
 # scatter plot matrix
 plot(df, pch=20)
 
 
 # residual plots
 resid_df <- resid(project)
 
 plot(X1, resid_df, main = "Plot of X1 against residuals")
 plot(X2, resid_df, main = "Plot of X2 against residuals")
 plot(X3, resid_df, main = "Plot of X3 against residuals")
 plot(X4, resid_df, main = "Plot of X4 against residuals")
 plot(X4p, resid_df, main = "Plot of X4p against residuals")
 plot(X5, resid_df, main = "Plot of X5 against residuals")
 plot(X6, resid_df, main = "Plot of X6 against residuals")
 
 
 # correlation matrix
 
 df_fix <- data.frame(X1, X2, X3, X4p, X5, X6, Y)
 
 # Correlation Matrix
 names(df_fix) <- c('X1', 'X2', 'X3', 'X4p','X5','X6', 'Y') 
 with(df_fix, cor(data.frame('X1' = X1, 'X2' = X2,
                                 'X3' = X3, 'X4p' = X4p, 'X5' = X5, 'X6'= X6,
                                 'Y' = Y))) 
 
 # Scatter plot Matrix
 with(df_fix, pairs(data.frame('X1' = X1, 'X2' = X2,
                               'X3' = X3, 'X4p' = X4p, 'X5' = X5, 'X6'= X6,
                               'Y' = Y)))
 
 # Normal dist test
 
 shapiro.test(residuals(fit))
 
 # reject H_0 (Error may NOT follow normal distribution)
 
 # Constant Error variance test 
 
 qqnorm(residuals(fit))
 qqline(residuals(fit))
 
 
 library(lmtest)
 bptest(fit)
 
 # reject H_0 (Error variances may NOT be constant)
 
 # Randomness Test
 
 library(car)
 
 durbinWatsonTest(fit)
 
 # reject H_0 (Data may not random)
 
 
 # BoxCox Transformation
 
 boxCox(fit)
 
 boxcox.sse <- function(lambda, model)
 {
   X1 <- model.frame(model)$X1
   X2 <- model.frame(model)$X2
   X3 <- model.frame(model)$X3
   X4 <- model.frame(model)$X4
   X5 <- model.frame(model)$X5
   X6 <- model.frame(model)$X6
   Y <- model.frame(model)$Y
   K2 <- prod(Y)^(1/length(Y))
   K1 <- 1 / (lambda * K2^(lambda - 1))
   ifelse(lambda != 0,
          assign("W", K1 * (Y^lambda - 1)),
          assign("W", K2 * log10(Y)))
   
   return(deviance(lm(W ~ X1 + X2 + X3 + X4 + X5 + X6)))
 }
 
 lambda <- seq(-2, 2, by = 0.3)
 SSE = sapply(lambda, boxcox.sse, lm(Y ~ X1 + X2 + X3 + X4p + X5 + X6, df))
 plot(lambda, SSE, type = "l", xlab = "expression(lambda)")
 abline(v = 1.5, lty = 3)
 
 cbind('lambda' = lambda, 'SSE' = SSE)
 

 # lambda is 1.7 or lambda is 2
 
 # transformed model
 
 Y_prime = Y^2

 altfit <- lm(Y_prime ~ X1 + X2 + X3 + X4p + X5 + X6, df) 
 
 # adjusted Y vs X
 
 plot(X1, Y_prime, main = 'Y prime vs. X1')
 plot(X2, Y_prime, main = 'Y prime vs. X1')
 plot(X3, Y_prime, main = 'Y prime vs. X1')
 plot(X4p, Y_prime, main = 'Y prime vs. X4')
 plot(X5, Y_prime, main = 'Y prime vs. X1')
 plot(X6, Y_prime, main = 'Y prime vs. X1')
 
 # adjusted resid(Y) vs X
 plot(X1, resid_df, xlab = 'X1', ylab = 'Residuals') 
 plot(X2, resid_df, xlab = 'X2', ylab = 'Residuals') 
 plot(X3, resid_df, xlab = 'X3', ylab = 'Residuals') 
 plot(X4p, resid_df, xlab = 'X4', ylab = 'Residuals') 
 plot(X5, resid_df, xlab = 'X5', ylab = 'Residuals') 
 plot(X6, resid_df, xlab = 'X6', ylab = 'Residuals') 
 
 
 qqnorm(residuals(altfit))
 qqline(residuals(altfit))
 
 
 shapiro.test(residuals(altfit))
 bptest(altfit)
 durbinWatsonTest(altfit)
 
 model_full <- lm(Y_prime ~ X1 + X2 + X3 + X4p + X5 + X6, df)
 
 #fit reduced model
 model_reduced <- lm(Y_prime ~ X1 + X2 + X3 + X5 + X6, df)
 
 #perform ANOVA to test for differences in models
 anova(model_reduced, model_full)
 
 # "Best" Subsets Regression
 
 library(leaps)

 BestSubReg <- function(model,...)
 {
   subsets <- regsubsets(formula(model), model.frame(model),...)
   subsets <- with(summary(subsets),
                   cbind(p = which, rss, rsq, adjr2, cp, bic))
   return(subsets)
 }
 
 altfit
 round(BestSubReg(altfit, nbest = 3), 4)
 
 # Forward Step Regression
 
 altfit # Full model
 step(altfit, direction = "forward", k = 2) # forward stepwise
 
 step(altfit, direction = "backward", k = 2) # backward stepwise
 
 # model with 5 parameters 
 
 newfit <- lm(Y_prime ~ X1 + X2 + X3 + X4p + X6, df)

 summary(newfit) 
  
 
 # deleted residuals

 newfit
 summary(newfit)
 dfE = length(Y) - 5 # df for Error = n - p
 MSE = deviance(newfit)/dfE # deviance() function is the value of SSE
 cbind('X1' = X1,'X2' = X2,'X3' = X3, 'X4p' = X4p, 'X6' = X6, 'Y' = Y,
       'Yhat' = round(fitted(newfit),3), # fitted value
       'e' = round(resid(newfit),3), # residual
       'e*'= round(resid(newfit)/sqrt(MSE),3), # semistudentized residual
       'h' = round(hatvalues(newfit),3), # main diagonals of hat matrix H
       's' = round(MSE*(1-hatvalues(newfit)),3), # main diagonals of variance-covariance of residual
       'r' = round(resid(newfit)/sqrt(MSE*(1-hatvalues(newfit))),3), # studentized residual
       't' = round(rstudent(newfit),3) # studentized deleted residual
 )
 
 options(max.print=1000000)
 
 e = round(resid(newfit),3)
 
 estar = round(resid(newfit)/sqrt(MSE),3)
 
 estarab = abs(estar)
 
 S2 = round(MSE*(1-hatvalues(newfit)),3)
 
 S = sqrt(S2)
 
 twoSe = 2*S
 
 h = round(hatvalues(newfit),3)
 
 ordinaryresidual_ei <- which(estarab >= twoSe); ordinaryresidual_ei
 
 n = 195
 
 p = 6
 
 th <- round((2*p)/n, 3)
 
 
 lev <- which(h > th); lev
 
 
 # outliers for e: all of them *not usefull
 
 # outliers for e*: 16, 194, 195 * also not useful
 
 # outliers for ri: 16, 190, 194, 195
 
 # outliers for ti: 16, 190, 194, 195
 
 # high leverage points: 16, 43, 192, 193, 194, 195
 
 # we want to look at 16, 190, 194, 195
 
 # influential errors
 
 cbind('X1' = X1,'X2' = X2,'X3' = X3, 'X4p' = X4p, 'X6' = X6, 'Y' = Y,
       'Yhat' = round(fitted(newfit),3), # fitted value
       'e' = round(resid(newfit),3), # residual
       'h' = round(hatvalues(newfit),3), # main diagonals of hat matrix H
       't' = round(rstudent(newfit),3), # studentized deleted residual
       'DFFITS' = round(dffits(newfit),3),
       'D' = round(cooks.distance(newfit), 3),
       'DFBeta0' = round(dfbetas(newfit)[,1],3),
       'DFBeta1' = round(dfbetas(newfit)[,2],3),
       'DFBeta2' = round(dfbetas(newfit)[,3],3),
       'DFBeta3' = round(dfbetas(newfit)[,4],3),
       'DFBeta4' = round(dfbetas(newfit)[,5],3),
       'DFBeta5' = round(dfbetas(newfit)[,6],3)
 )
 
 DFFITS = round(dffits(newfit),3)
 
 infit <- which(abs(DFFITS) > 0.351); infit
 
 D = round(cooks.distance(newfit), 3)
 
 infit2 <- which(D > 0.895); infit2 
 
 DFBeta0 = round(dfbetas(newfit)[,1],3)
 DFBeta1 = round(dfbetas(newfit)[,2],3)
 DFBeta2 = round(dfbetas(newfit)[,3],3)
 DFBeta3 = round(dfbetas(newfit)[,4],3)
 DFBeta4 = round(dfbetas(newfit)[,5],3)
 DFBeta5 = round(dfbetas(newfit)[,6],3)
 
 
 incof0 <- which(abs(DFBeta0) > 0.143); incof0
 incof1 <- which(abs(DFBeta1) > 0.143); incof1
 incof2 <- which(abs(DFBeta2) > 0.143); incof2
 incof3 <- which(abs(DFBeta3) > 0.143); incof3
 incof4 <- which(abs(DFBeta4) > 0.143); incof4
 incof5 <- which(abs(DFBeta5) > 0.143); incof5
 
 # 16, 194, 195 seem to be influential on all aspects
 
 # 43, 190, 192, 193 might be influential
 
 # influence on inferences
 
 summary(newfit)
 
 df1 <- df[-c(16, 43, 190, 192, 193, 194, 195),]
 
 X1a = df1$X1
 X2a = df1$X2
 X3a = df1$X3
 X4a = df1$X4
 X6a = df1$X6
 Ya = df1$Y
 df1$Y <- Ya + 0.00001
 X4pa = (X4a^0.3)
 
 Y_primea = Ya^2
 
 newfit1 <- lm(Y_primea ~ X1a + X2a + X3a + X4pa + X6a, df1)
 
 summary(newfit1)
 
 # Multicollinearity
 
 df_fix1 <- data.frame(X1a, X2a, X3a, X4pa, X6a, Ya)
 names(df_fix1) <- c('X1a', 'X2a', 'X3a', 'X4pa','X6a', 'Ya') 
 with(df_fix1, cor(data.frame('X1a' = X1a, 'X2a' = X2a,
                             'X3a' = X3a, 'X4pa' = X4pa, 'X6a'= X6a,
                             'Ya' = Ya))) 
 checkfit <- lm(scale(Y_primea) ~ scale(X1a) + scale(X2a) + 
                  scale(X3a) + scale(X4pa) + scale(X6a), df1)
 
 cbind(
   'Beta*' = round(as.vector(coef(checkfit)[-1]), 4),
   'VIF' = round(vif(lm(Y_primea ~ X1a + X2a + X3a + X4pa + X6a, df1)),2)
 )
 
 
 midfit <- lm(Y_primea ~ X1a + X2a + X3a + X4pa, df1)
 
 midfit1 <- lm(scale(Y_primea) ~ scale(X1a) + scale(X2a) + scale(X3a) + scale(X4pa), df1)
 
 cbind(
   'Beta*' = round(as.vector(coef(midfit1)[-1]), 4),
   'VIF' = round(vif(lm(Y_primea ~ X1a + X2a + X3a + X4pa, df1)),2)
 )
 
 
 Finalfit <- lm(Y_primea ~ X1a + X2a + X3a + X4pa, df1)
 
 posfin <- lm(Ya ~ X1a + X2a + X3a + X4pa, df1)
 
 summary(Finalfit)

 
 
 
 
 
 
 
 
 
 
 
 
 