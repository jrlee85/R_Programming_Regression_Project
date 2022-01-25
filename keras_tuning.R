# Load data
nn_scaled_df <- read.csv(here("scaled_data.csv"), encoding="UTF-8")

# Create training and test sets
trainingRows <- createDataPartition(nn_scaled_df$Price_BRL, 
                                   p=0.8, list=F)
trainingSubset<- scaled_df[trainingRows, ]
testSubset <- scaled_df[-trainingRows, ]

# Convert to arrays
train_keras <- array_reshape(as.matrix(trainingSubset[, -20]), dim(trainingSubset[,-20]))
test_keras <- array_reshape(as.matrix(testSubset[, -20]), dim(testSubset[,-20]))
train_labels <-array_reshape(as.matrix(trainingSubset[,20]), c(length(trainingSubset[,20]),1))
test_labels <-array_reshape(as.matrix(testSubset[,20]), c(length(testSubset[,20]),1))

# Define number of features for input shape
n_features <- ncol(train_keras)

# Hyperparameter flags ---------------------------------------------------

# Set flags for hyperparameters to be searched (with default values)
FLAGS <- flags(
    flag_integer("layers", 2),
    flag_integer("units", 16),
    flag_numeric("learning_rate", 0.01),
    flag_numeric("dropout", 0.5),
    flag_numeric("weight_decay", 0.01)
)

# Define Model --------------------------------------------------------------

# Create a model with a single hidden input layer

network <- keras_model_sequential() %>%
    layer_dense(units = FLAGS$units, activation = "relu", input_shape = n_features,
                kernel_regularizer = regularizer_l2(l = FLAGS$weight_decay)) %>%
    layer_dropout(rate = FLAGS$dropout)

# Add layers depending on 'layer' hyperparameter
if (FLAGS$layers > 1) {
    for (i in seq_len(FLAGS$layers - 1)) {
        network %>% 
            layer_dense(units = FLAGS$units, activation = "relu",
                        kernel_regularizer = regularizer_l2(l = FLAGS$weight_decay)) %>%
            layer_dropout(rate = FLAGS$dropout)
    }
}

# Add final output layer
network %>% layer_dense(units = 1, activation = "linear")


network %>% compile(
    optimizer = optimizer_adam(FLAGS$learning_rate),
    loss = "mse",
    metrics = c("mean_squared_error")
)

history <- network %>% 
    fit(
        train_keras,
        train_labels, 
        epochs = 100,
        validation_split = 0.2,
        verbose = FALSE,
        callbacks = list(
            callback_reduce_lr_on_plateau(patience = 5),
            callback_early_stopping(
                    monitor = "val_mean_squared_error", patience = 10)
        )
    )

#Save copy of each model in order to load later for evaluation
save_model_tf(network, "airbnb")
