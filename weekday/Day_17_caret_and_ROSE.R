library(readxl)
library(caret)
library(dplyr)
library(ROSE)
df_test <- read_xlsx(path = 'df_test.xlsx') #I do not host this file
summary(df)

df_test[,14] <- NULL 
df_test$Factor_5 <- log(df_test$Factor_5 + 1)
df_test <- ROSE(status~., data=df_test, seed=42)$data
df_test$status <- factor(df_test$status)

division <- createDataPartition(df_test$status, p = 0.9, list = F)
df_train1 <- df_test[division,]
df_test1 <- df_test[-division,]

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = T,
  summaryFunction = twoClassSummary)

fit_gbm <- train(status ~ .,
                 data = df_train1,
                 metric = 'AUC',
                 method = 'gbm',
                 trControl = ctrl)
fit_rf <- train(status ~ .,
                data = df_train1,
                metric = 'ROC',
                method = 'rf',
                trControl = ctrl)
fit_svm <- train(status ~ .,
                 data = df_train1,
                 metric = 'ROC',
                 method = 'svmRadial',
                 trControl = ctrl)

resam <- resamples(list(GBM = fit_gbm,
                        RandomForest = fit_rf,
                        SVM = fit_svm))
summary(resam)
dotplot(resam, metric = "ROC")
x <- predict(fit_gbm, df_test1[,-13], type = 'prob')
confusionMatrix(x, df_test1$status)
roc.curve(df_test1$status, x)


normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

levels(df_test1$status) <- c("0", "1")
y_test_raw <- as.numeric(levels(df_test1$status))[df_test1$status]
normalizedGini(y_test_raw, x$X1)
