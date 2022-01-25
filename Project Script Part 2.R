# Install necessary packages
install.packages("tidyverse")
install.packages("here")
install.packages("randomForest")
install.packages("caret")
install.packages("mice")
install.packages("xgboost")
install.packages("keras")
install.packages("corrplot")
install.packages("e1071")

# Two packages used in original analysis but not required to run this code
#install.packages("tfruns")
#install.packages("tfestimators")

##############################

# Load libraries
library("tidyverse")
library("here")
library("randomForest")
library("caret")
library("mice")
library("xgboost")
library("keras")
library("corrplot")
library("e1071")

# Two libraries used in original analysis but not required to run this code
#library("tfruns")
#library("tfestimators")

##############################

# Load data set. Use tryCatch function in case file is not found
# Import with UTF-8 encoding to accommodate Portuguese characters, identifying 
# NA strings
tryCatch(airbnb_df <- read.csv(here("cleaned_listings2.csv"),
                               encoding="UTF-8", 
                               na.strings=c("","NA")),
         warning = function(c) {
             print("File not found")
             stop("Program stopped")
         }
)
# Also load original dataset in order to add two columns
cols_needed <- rep("NULL", 74)
cols_needed[c(1,23,53)] <- "numeric"
tryCatch(complete_df <- read.csv(here("listings.csv"),
                                 encoding="UTF-8", 
                                 colClasses = cols_needed),
         warning = function(c) {
             print("File not found")
             stop("Program stopped")
         }
)
# Merge dataframes by id
airbnb_df <- left_join(airbnb_df, complete_df, by="id")
# delete unneeded dataframe
remove(complete_df)

##############################

# Set random seed value
set.seed(0)
# Drop id column
sapply(airbnb_df, function(x) length(unique(x)) == length(x))
airbnb_df <- airbnb_df %>% select(-id)
# Re-categorise columns as per data cleaning step
airbnb_df$host_response_time <- ordered(factor(airbnb_df$host_response_time, levels=c("within an hour", "within a few hours", "within a day", "a few days or more")))
airbnb_df$bathrooms_text <- factor(airbnb_df$bathrooms_text)

# Create copy dataframe with NA rows dropped for quick modelling
airbnb_df_copy <- na.omit(airbnb_df)
# 80/20 training-test split 
trainingRows <- createDataPartition(airbnb_df_copy$review_scores_rating, 
                                   p=0.8, list=F)
trainingSubset<- airbnb_df_copy[trainingRows, ]
testSubset <- airbnb_df_copy[-trainingRows, ]
# Drop duplicated information
colnames(trainingSubset)
# Use Random Forest for quick modelling
rf <- randomForest(trainingSubset[, c(1:18, 26:44) ], 
                   trainingSubset$review_scores_rating, 
                   xtest=testSubset[, c(1:18, 26:44) ],
                   ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf$mse))))
print(paste("R-squared:", mean(rf$rsq)))

# Try price predicting model instead.
# Create new training/test partition
trainingRows <- createDataPartition(airbnb_df_copy$Price_BRL, 
                                   p=0.8, list=F)
trainingSubset<- airbnb_df_copy[trainingRows, ]
testSubset <- airbnb_df_copy[-trainingRows, ]
rf <- randomForest(trainingSubset[, c(1:14, 16:44) ], 
                   trainingSubset$Price_BRL, 
                   xtest=testSubset[, c(1:14, 16:44) ],
                   ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf$mse))))
print(paste("R-squared:", mean(rf$rsq)))

##############################

# Try multiple linear regression model.
# Reuse dataset without NA values
# Create minimum model with 0 features and maximum model with all features.
min_model <- lm(airbnb_df_copy$Price_BRL ~ 1, data=airbnb_df_copy)
max_model <- formula(lm(airbnb_df_copy$Price_BRL~.,airbnb_df_copy))

# Step forward through models

# WARNING - takes approx 3 minutes to complete next line of code

best_model <- step(
    min_model,
    direction="forward",
    scope = max_model, trace = 0
)
summary(best_model)
# Check plots for linear regression assumptions
plot(best_model)

###############################

# Return to random forest model
# Check for and drop rows with have smaller number of missing values
sapply(airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- airbnb_df[!is.na(airbnb_df$host_is_superhost),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- smaller_airbnb_df[!is.na(smaller_airbnb_df$beds),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- smaller_airbnb_df[!is.na(smaller_airbnb_df$bathrooms_text),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- smaller_airbnb_df[!is.na(smaller_airbnb_df$review_scores_accuracy),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- smaller_airbnb_df[!is.na(smaller_airbnb_df$review_scores_location),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))
smaller_airbnb_df <- smaller_airbnb_df[!is.na(smaller_airbnb_df$review_scores_value),]
sapply(smaller_airbnb_df, function(x) sum(is.na(x)))

# Check importance of columns
rf <- randomForest(trainingSubset[, c(1:14, 16:44) ], 
                   trainingSubset$Price_BRL, 
                   xtest=testSubset[, c(1:14, 16:44) ],
                   ntree=100, importance=TRUE)
varImpPlot(rf, sort=TRUE, n.var=min(43, nrow(rf$importance)))

# Drop four remaining columns with missing values
smallest_df <- smaller_airbnb_df %>% select(-c(bedrooms, host_acceptance_rate,
                                    host_response_rate, host_response_time))
smaller_trainingRows <- createDataPartition(smallest_df$Price_BRL, 
                                   p=0.8, list=F)
smaller_trainingSubset<- smallest_df[smaller_trainingRows, ]
smaller_testSubset <- smallest_df[-smaller_trainingRows, ]
colnames(smallest_df)
smallest_rf <- randomForest(smaller_trainingSubset[, c(1:10, 12:40) ], 
                   smaller_trainingSubset$Price_BRL, 
                   xtest=smaller_testSubset[, c(1:10, 12:40) ],
                   ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf$mse))))
print(paste("R-squared:", mean(smallest_rf$rsq)))


# Try imputing missing values.
# First, encode host_response_time using ordinal encoding
smaller_airbnb_df$host_response_time <- as.numeric(smaller_airbnb_df$host_response_time)
# Split into training and testing sets before imputation
trainingRows <- createDataPartition(smaller_airbnb_df$Price_BRL, 
                                           p=0.8, list=F)
trainingSubset<- smaller_airbnb_df[trainingRows, ]
testSubset <- smaller_airbnb_df[-trainingRows, ]
# Drop na rows from testSubset
testSubset <- na.omit(testSubset)
# Impute on trainingSubset

# WARNING - takes approx 2 minutes to complete next line of code

tempData <- mice(trainingSubset,m=3,maxit=50,meth='pmm',seed=500, printFlag = FALSE)
# Plot the imputed data to check distribution
densityplot(tempData)
# Compare three different imputations

# WARNING - takes approx 2 minutes to produce all three RF models

completedData1 <- complete(tempData, 1)
rf1 <- randomForest(completedData1[, c(1:14, 16:44) ], 
                    completedData1$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf1$mse))))
print(paste("R-squared:", mean(rf1$rsq)))

completedData2 <- complete(tempData, 2)
rf2 <- randomForest(completedData2[, c(1:14, 16:44) ], 
                    completedData2$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf2$mse))))
print(paste("R-squared:", mean(rf2$rsq)))

completedData3 <- complete(tempData, 3)
rf3 <- randomForest(completedData3[, c(1:14, 16:44) ], 
                    completedData3$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf3$mse))))
print(paste("R-squared:", mean(rf3$rsq)))

# Instead, drop rows with missing values in host_response_time, impute remainder
smaller_airbnb_df2 <- airbnb_df[!is.na(airbnb_df$host_response_time),]
trainingRows <- createDataPartition(smaller_airbnb_df2 $Price_BRL, 
                                   p=0.8, list=F)
trainingSubset<- smaller_airbnb_df2 [trainingRows, ]
testSubset <- smaller_airbnb_df2 [-trainingRows, ]
testSubset <- na.omit(testSubset)

# WARNING - takes approx 2 minutes to complete next line of code

tempData <- mice(trainingSubset,m=3,maxit=50,meth='pmm',seed=500, printFlag = FALSE)

# WARNING - takes approx 2 minutes to produce all three RF models

completedData1 <- complete(tempData, 1)
rf1 <- randomForest(completedData1[, c(1:14, 16:44) ], 
                    completedData1$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf1$mse))))
print(paste("R-squared:", mean(rf1$rsq)))

completedData2 <- complete(tempData, 2)
rf2 <- randomForest(completedData2[, c(1:14, 16:44) ], 
                    completedData2$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf2$mse))))
print(paste("R-squared:", mean(rf2$rsq)))

completedData3 <- complete(tempData, 3)
rf3 <- randomForest(completedData3[, c(1:14, 16:44) ], 
                    completedData3$Price_BRL, 
                    xtest=testSubset[, c(1:14, 16:44) ],
                    ntree=100)
print(paste("Root-mean-square-error:", sqrt(mean(rf3$mse))))
print(paste("R-squared:", mean(rf3$rsq)))

###############################

# Try SVR.
# Requires numerical, scaled data. Use ordinal encoding.
encoded_df <- airbnb_df_copy
encoded_df$host_response_time <- as.numeric(encoded_df$host_response_time)
encoded_df$neighbourhood_cleansed <- as.numeric(as.factor(encoded_df$neighbourhood_cleansed))
encoded_df$room_type <- as.numeric(as.factor(encoded_df$room_type))
# Instead of simply converting bathrooms_text, add a column for shared or 
# not, then convert bathrooms column to numeric
encoded_df$shared_bathroom = 0
shared <- grepl("shared", encoded_df$bathrooms_text)
encoded_df$shared_bathroom[shared] = 1
encoded_df$bathrooms_text <- sub("baths", "", encoded_df$bathrooms_text)
encoded_df$bathrooms_text <- sub("bath", "", encoded_df$bathrooms_text)
encoded_df$bathrooms_text <- sub("shared", "", encoded_df$bathrooms_text)
encoded_df$bathrooms_text <- as.numeric(encoded_df$bathrooms_text)
# Remove price to avoid scaling
scaled_df <- encoded_df[, c(1:14, 16:45)]
y <- encoded_df[,15]
# USe caret's preprocess to scale between 0 and 1 (range)
scaler <- preProcess(scaled_df, method = 'range')
scaled_df <- predict(scaler, scaled_df)
# Add unscaled price column back into dataframe
scaled_df$Price_BRL <- y

# Check for high correlation
corrplot(cor(scaled_df), method = "circle")
# Issue with None column
sum(scaled_df$None)
# Only contains zeros, so this can be dropped
scaled_df = scaled_df %>% select(-None)
# Drop highly correlated columns
scaled_df = scaled_df %>% select(-host_response_time)
scaled_df = scaled_df[,-c(25:39)]
scaled_df = scaled_df %>% select(-c(beds, shared_bathroom))
scaled_df <- scaled_df[,-c(17:22)]

# Create training and test sets
trainingRows <- createDataPartition(scaled_df$Price_BRL, 
                                   p=0.8, list=F)
trainingSubset<- scaled_df[trainingRows, ]
testSubset <- scaled_df[-trainingRows, ]

# Will try Radial Basis and Polynomial SVR models. Using train function from 
# Caret library
svr_poly_model <- svm(Price_BRL ~ ., data=trainingSubset, kernel="polynomial")
# Calculate RMSE
sqrt(mean(svr_poly_model$residuals^2))
# Calculate R-squared
target_mean <- mean(trainingSubset[,20])
tss <-  sum((trainingSubset[,20] - target_mean)^2 )
rss <-  sum(svr_poly_model$residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))

svr_rbf_model <- svm(Price_BRL ~ ., data=trainingSubset, kernel="radial")
sqrt(mean(svr_rbf_model$residuals^2))
target_mean <- mean(trainingSubset[,20])
tss <-  sum((trainingSubset[,20] - target_mean)^2 )
rss <-  sum(svr_rbf_model$residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))

###############################

# Try XGBoost

cvCtrl <- trainControl(method="cv", number=2)
xgb_model <- caret::train(trainingSubset[,-20], trainingSubset[,20],
                   method='xgbTree',
                   trControl = cvCtrl,
                   tunelength = 1,
)
xgb_model$results[which(rownames(xgb_model$results)==rownames(xgb_model$bestTune)), ]

# Performed grid search to find best hyperparameters. Took over three hours to 
# complete, therefore commented out here

# Use five fold cross validation
#cvCtrl <- trainControl(method="cv",
#                       number = 5)

# Define learning rates to search
#range <- log(0.5) - log(0.0001)
#step <- range/4
#lr_params <- c(0.0001, exp(log(0.0001)+step), exp(log(0.0001)+(2*step)),
#               exp(log(0.0001)+(3*step)), 0.5)

# Define grid to search
#xgbGrid <- expand.grid(nrounds=c(100,200,500),
#                       max_depth=c(5,10,20,25),
#                       eta = lr_params,
#                       subsample = c(0,0.25,0.5,0.75,1),
#                       gamma=0,
#                       colsample_bytree=c(0,0.25,0.5,0.75,1),
#                       min_child_weight=1)

# Build models
#xgb_model <- caret::train(trainingSubset[,-20], trainingSubset[,20],
#                   method='xgbTree',
#                   trControl = cvCtrl,
#                   tuneGrid = xgbGrid,
#)

# Display results for best model
#xgb_model$results[which(rownames(xgb_model$results)==rownames(xgb_model$bestTune)), ]

# Rebuild model using best parameters found by grid search
# Convert data to DMatrix format for XGBoost
dtrain <- xgb.DMatrix(as.matrix(trainingSubset[,-20]), 
                      label = trainingSubset$Price_BRL)
dtrain_X <- xgb.DMatrix(as.matrix(trainingSubset[,-20]))

tuned_xgb_model <- xgb.train(params = list(eta = 0.0594, max_depth = 5, gamma = 0,
                                       colsample_bytree = 0.75, subsample = 0.75),
                             data = dtrain,  nrounds=500)

predicted <- predict(tuned_xgb_model, dtrain_X)
residuals <- trainingSubset[,20] - predicted
RMSE <- sqrt(mean(residuals^2))
print(paste("Root-mean-squared-error:", RMSE))
target_mean <- mean(trainingSubset[,20])
tss <-  sum((trainingSubset[,20] - target_mean)^2 )
rss <-  sum(residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))

###############################

# Try neural network

# Convert data to array for Keras
train_keras <- array_reshape(as.matrix(trainingSubset[, -20]), dim(trainingSubset[,-20]))
test_keras <- array_reshape(as.matrix(testSubset[, -20]), dim(testSubset[,-20]))
train_labels <-array_reshape(as.matrix(trainingSubset[,20]), c(length(trainingSubset[,20]),1))
test_labels <-array_reshape(as.matrix(testSubset[,20]), c(length(testSubset[,20]),1))

model <- keras_model_sequential() %>% 
    layer_dense(units=128, kernel_regularizer = regularizer_l1(0.005), 
                activation='relu', input_shape=ncol(train_keras)) %>%
    layer_dense(units=256, kernel_regularizer = regularizer_l1(0.005), 
                activation="relu") %>% 
    layer_dense(units=256, kernel_regularizer = regularizer_l1(0.005), 
                activation = "relu") %>% 
    layer_dense(units=512, kernel_regularizer = regularizer_l1(0.005), 
                activation = "relu") %>% 
    layer_dense(units=1, activation="linear")

model %>% compile(
    loss = "mse",
    optimizer =  optimizer_adam(),
    metrics = list("mean_squared_error")
)

train_accuracy <- model %>% 
    fit(x=train_keras, y=train_labels,
        epochs = 100,validation_split=0.2,
        verbose = 1)

# Performed grid search through hyperparameters using Keras's training_runs
# method. Took over 16 hours to perform search, therefore code is commented out

# Save processed dataset to CSV
# write.csv(scaled_df, here("scaled_data.csv"), row.names = FALSE)

# Define hyperparameters to try
#grid_search <- list(
#        layers = c(3,4,5),
#        units = c(128,256,512), 
#        learning_rate = c(0.0001, 0.0008408964, 0.0070710678, 0.0594603558, 0.5),
#        dropout = c(0,0.25,0.5),
#        weight_decay = c(0.1,0.01,0.001)
#)

# Call tuning_runs, using the accompanying 'keras-tuning.R' script as source
#runs <- tuning_run("keras_tuning.R", flags=grid_search, confirm = FALSE)

# Define the best run by lowest MSE
#best_run <- ls_runs(
#    runs_dir = "runs",
#    order = metric_val_mean_squared_error,
#    decreasing = FALSE
#)

# Display MSE
#best_run[1,]$metric_val_mean_squared_error

# Display best run details including hyperparameters
#view_run(best_run$run_dir[1])

# Rebuild best model using hyperparameters found in grid search

tuned_model <- keras_model_sequential() %>% 
    layer_dense(units=512, kernel_regularizer = regularizer_l2(0.01), 
                activation='relu', input_shape=ncol(train_keras)) %>%
    layer_dropout(0.25) %>%
    layer_dense(units=256, kernel_regularizer = regularizer_l2(0.01), 
                activation="relu") %>% 
    layer_dropout(0.25) %>%
    layer_dense(units=256, kernel_regularizer = regularizer_l2(0.01), 
                activation = "relu") %>% 
    layer_dropout(0.25) %>%
    layer_dense(units=512, kernel_regularizer = regularizer_l2(0.01), 
                activation = "relu") %>% 
    layer_dropout(0.25) %>%
    layer_dense(units=512, kernel_regularizer = regularizer_l2(0.01), 
                activation = "relu") %>% 
    layer_dropout(0.25) %>%
    layer_dense(units=1, activation="linear")

tuned_model %>% compile(
    loss = "mse",
    optimizer =  optimizer_adam(lr=0.0001),
    metrics = list("mean_squared_error")
)

train_accuracy <- tuned_model %>% 
    fit(x=train_keras, y=train_labels,
        epochs = 40,validation_split=0.2,
        verbose = 1)

###############################

# See how SVR performs on test set

predicted <- predict(svr_rbf_model, testSubset[-20])
residuals <- testSubset[,20] - predicted
RMSE <- sqrt(mean(residuals^2))
print(paste("Root-mean-squared-error:", RMSE))
target_mean <- mean(testSubset[,20])
tss <-  sum((testSubset[,20] - target_mean)^2 )
rss <-  sum(residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))

# See how XGBoost performs on test set

dtest <- xgb.DMatrix(as.matrix(testSubset[,-20]), 
                      label = testSubset$Price_BRL)
dtest_X <- xgb.DMatrix(as.matrix(testSubset[,-20]))
predicted <- predict(tuned_xgb_model, dtest_X)
residuals <- testSubset[,20] - predicted
RMSE <- sqrt(mean(residuals^2))
print(paste("Root-mean-squared-error:", RMSE))
target_mean <- mean(testSubset[,20])
tss <-  sum((testSubset[,20] - target_mean)^2 )
rss <-  sum(residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))

xgb_results <- as.data.frame(cbind(predicted, testSubset[,20]))
colnames(xgb_results) <- c("Predicted", "Observed")

ggplot(xgb_results,aes(Predicted, Observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
    xlab("Predecited Price ") + ylab("Observed Price") + 
    theme(plot.title = element_text(size=16,hjust = 0.5),
          axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
          axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

# See how NN model performs on test set

# Line to load model if gird search used
#nn_model <- load_model_tf(paste0(best_run_mse$run_dir[1], "/airbnb"))


tuned_model %>% evaluate(test_keras, test_labels)
predicted <- predict(tuned_model, test_keras)
residuals <- test_labels - predicted
RMSE <- sqrt(mean(residuals^2))
print(paste("Root-mean-squared-error:", RMSE))
target_mean <- mean(test_labels)
tss <-  sum((test_labels - target_mean)^2 )
rss <-  sum(residuals^2)
rsq  <-  1 - (rss/tss)
print(paste("R-squared:", rsq))