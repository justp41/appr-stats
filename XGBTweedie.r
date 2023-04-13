library(xgboost)
library(caret)
library(dplyr)
library(gbm)
library(statmod)
library(TDboost)
setwd("D:/Actuariat/6e session/stats/tp/rapport 2")
### Importer les données et préparer train/eval
load("imp_data.RData")
set.seed(536778899, sample.kind = "Rejection")
ind.entr <- sample(1:nrow(completedata), 0.85*nrow(completedata), FALSE)
train <- completedata[ind.entr,]
test <- completedata[-ind.entr,]

# Create a new variable in the train and test data.frames, representing ClaimAmount/ExposTotal
train$ClaimAmountPerExpos <- train$ClaimAmount / train$ExposTotal
test$ClaimAmountPerExpos <- test$ClaimAmount / test$ExposTotal

library(xgboost)
library(caret)

# One-hot encode categorical variables and create a matrix for input features
train_matrix <- model.matrix(ClaimAmount ~ Gender + DrivAge + VehYear + Area + State + VehManuf + SumInsAvg, data=train)
test_matrix <- model.matrix(ClaimAmount ~ Gender + DrivAge + VehYear + Area + State + VehManuf + SumInsAvg, data=test)

# Create a vector for the response variable
train_labels <- train$ClaimAmount
test_labels <- test$ClaimAmount

# Create an offset term for the train and test datasets
train_offset <- train$ExposTotal
test_offset <- test$ExposTotal

# Create DMatrix objects for train and test datasets with the offset term
train_dmatrix <- xgb.DMatrix(data = train_matrix, label = train_labels, base_margin = log(train_offset))
test_dmatrix <- xgb.DMatrix(data = test_matrix, label = test_labels, base_margin = log(test_offset))

watchlist <- list(train = train_dmatrix, test = test_dmatrix)

# Set the parameters for the XGBoost model
params <- list(objective = "reg:tweedie",
               tweedie_variance_power = 1.5, # Tweedie power parameter (alpha)
               eta = 0.01, # Learning rate
               max_depth = 2, # Maximum depth of the trees
               subsample = 0.5, # Subsampling fraction
               eval_metric = "rmse")

# Train the XGBoost model using the Tweedie distribution and monitor the test dataset
xgb_tweedie <- xgb.train(params = params,
                         data = train_dmatrix,
                         nrounds = 3000,
                         watchlist = watchlist,
                         early_stopping_rounds = 50,
                         verbose = 1)

prev <- predict(xgb_tweedie,newdata = test_dmatrix)

rmse <- sqrt(mean((test$ClaimAmount-prev)^2))
rmse


# Set the parameters for the XGBoost model
params <- list(objective = "reg:tweedie",
               tweedie_variance_power = 1.5, # Tweedie power parameter (alpha)
               eta = 0.01, # Learning rate
               max_depth = 4, # Maximum depth of the trees
               subsample = 0.75, # Subsampling fraction
               eval_metric = "rmse")

# Train the XGBoost model using the Tweedie distribution and monitor the test dataset
xgb_tweedie2 <- xgb.train(params = params,
                         data = train_dmatrix,
                         nrounds = 3000,
                         watchlist = watchlist,
                         early_stopping_rounds = 50,
                         verbose = 1)

prev <- predict(xgb_tweedie2,newdata = test_dmatrix)

 1 - sum((test$ClaimAmount - prev)^2)/sum((test$ClaimAmount - mean(test$ClaimAmount))^2)

xgb.save(xgb_tweedie2, "xgb_tweedie.model")
