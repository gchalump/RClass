#install.packages("caret") 
#install.packages("e1071") 

library(caret)
library(e1071)
library(plyr)
library(xgboost)
library(Metrics)

# Create custom summary function in proper format for caret
custom_summary = function(data, lev = NULL, model = NULL){
  out = rmsle(data[, "obs"], data[, "pred"])
  names(out) = c("rmsle")
  out
}

# Create control object
control = trainControl(method = "cv",  # Cross validation
                       number = 4,     # 4 folds
                       summaryFunction = custom_summary     
)

# Create grid of tuning parameter
grid = expand.grid(nrounds=c(5, 10, 20),    # Test 3 values for boosting rounds
                   max_depth= c(4 , 6 , 10),# Test 2 values for tree depth
                   eta=c(0.2,0.1, 0.05, 0.025), # Test 3 values for learning rate
                   gamma= c(0.1),           # minimum loss reduction
                   colsample_bytree = c(1), # subsample ratio of columns when constructing each tree
                   min_child_weight = c(1), # minimum sum of instance weight
                   subsample = c(.5,.8, 1))    # subsample ratio of the training instance

set.seed(77)

xgb_tree_model =  train(SalePrice~.,      # Predict SalePrice using all features
                        data=MyData,
                        method="xgbTree",
                        trControl=control, 
                        tuneGrid=grid, 
                        metric="rmsle",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric
xgb_tree_model$results
plot(xgb_tree_model)
xgb_tree_model$bestTune

varImp(xgb_tree_model)
#######################################################################################
