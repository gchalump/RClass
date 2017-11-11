#install.packages("missForest")
#install.packages("mice")
#install.packages("VIM")
#install.packages("caret")

library(missForest)
library(mice)
library(VIM)
library(caret)
library(ggplot2)

data("iris")

original <- subset(iris, select = -c(Species))

missing_percent <- c(0.02,0.05,0.1,0.15,0.2,0.25)
result_rmse <- data.frame(percent = double(),pmm = double(), mean = double(),sample = double())
result_knn <- data.frame(percent = double(),original = double(),pmm = double(), mean = double(),sample = double())

for(i in 1:length(missing_percent)){
  
    result_rmse[i,]$percent <- missing_percent[i]
    result_knn[i,]$percent <- missing_percent[i]
  
    df <- prodNA(original, noNA = missing_percent[i])
    summary(df)
    mice_plot <- aggr(df, col=c('green','red'),
                      numbers=TRUE, sortVars=TRUE,
                      labels=names(df), cex.axis=.7,
                      gap=3, ylab=c("Missing data",missing_percent[i]))
    
    imputed_pmm <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)
    imputed_mean <- mice(df, m=5, maxit = 50, method = 'mean', seed = 500)
    imputed_sample <- mice(df, m=5, maxit = 50, method = 'sample', seed = 500)
    
    imputed_pmm <- complete(imputed_pmm)
    imputed_mean <- complete(imputed_mean)
    imputed_sample <- complete(imputed_sample)
    
    result_rmse[i,]$pmm <- nrmse(original,df,imputed_pmm)
    result_rmse[i,]$mean <- nrmse(original,df,imputed_mean)
    result_rmse[i,]$sample <- nrmse(original,df,imputed_sample)
    
    
    imputed_pmm$Species <- iris$Species
    imputed_mean$Species <- iris$Species
    imputed_sample$Species <- iris$Species
    
    set.seed(555)
    ctrl <- trainControl(method="repeatedcv",repeats = 3)
    knn_original <- train(Species ~ ., data = iris, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
    
    set.seed(555)
    ctrl <- trainControl(method="repeatedcv",repeats = 3)
    knn_pmm <- train(Species ~ ., data = imputed_pmm, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
    
    set.seed(555)
    ctrl <- trainControl(method="repeatedcv",repeats = 3)
    knn_mean <- train(Species ~ ., data = imputed_mean, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
    
    set.seed(555)
    ctrl <- trainControl(method="repeatedcv",repeats = 3)
    knn_sample <- train(Species ~ ., data = imputed_sample, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
    
    
    
    result_knn[i,]$original <- knn_original$results$Accuracy[knn_original$results$k == knn_original$bestTune[1,1]]
    result_knn[i,]$pmm <- knn_pmm$results$Accuracy[knn_pmm$results$k == knn_pmm$bestTune[1,1]]
    result_knn[i,]$mean <- knn_mean$results$Accuracy[knn_mean$results$k == knn_mean$bestTune[1,1]]
    result_knn[i,]$sample <- knn_sample$results$Accuracy[knn_sample$results$k == knn_sample$bestTune[1,1]]
}

ggplot(result_rmse, aes(percent)) +  
  geom_line(aes(y = pmm, colour = "pmm"))+ 
  geom_line(aes(y = mean, colour = "mean"))+ 
  geom_line(aes(y = sample, colour = "sample"))+ 
  labs(title = "RMSE from random imputation methods",y = "RMSE", x = "Percentage of Missing", colour = "Methods")

ggplot(result_knn, aes(percent)) + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = pmm, colour = "pmm"))+ 
  geom_line(aes(y = mean, colour = "mean"))+ 
  geom_line(aes(y = sample, colour = "sample"))+ 
  labs(title = "KNN acuuracy from random imputation methods",y = "Accuracy", x = "Percentage of Missing", colour = "Methods")
