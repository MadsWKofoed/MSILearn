# # R/training_functions.R



# # Combine all loaded data files into one matrix
# # must be a list of matrices
# # Therefore need to load the wanted datafiles into one list
# combined_matrix <- do.call(rbind, processed_msi_list1)
# dim(combined_matrix)
# # Transpose after combining
# # combined_matrix <- t(combined_matrix)
# # dim(combined_matrix)
# length(unique(rownames(combined_matrix)))
# nrow(combined_matrix)


# labels <- as.factor(combined_matrix[, "runNames"])


# mz_columns <- grep("^mz_", colnames(combined_matrix))
# feature_matrix <- as.matrix(combined_matrix[, mz_columns])


# set.seed(123)

# train_proportion <- 0.8

# unique_pixels <- 1:nrow(feature_matrix)  # every row = a pixel
# shuffled_pixels <- sample(unique_pixels)

# num_train <- ceiling(length(shuffled_pixels) * train_proportion)
# train_indices <- shuffled_pixels[1:num_train]
# test_indices  <- shuffled_pixels[(num_train+1):length(shuffled_pixels)]

# train_data <- feature_matrix[train_indices, ]
# train_target <- labels[train_indices]
# test_data  <- feature_matrix[test_indices, ]
# test_target <- labels[test_indices]

# class_frequencies <- table(train_target)
# class_weights <- 1 / class_frequencies
# class_weights <- class_weights / sum(class_weights)
# observation_weights <- class_weights[train_target]



# set.seed(1234)

# # Calculate class weights
# class_frequencies <- table(train_target)
# class_weights <- 1 / class_frequencies
# class_weights <- class_weights / sum(class_weights)
# observation_weights <- class_weights[train_target]


# # Have the option for the user to define parameters used for training
# # Number of Cross-validation folds (number = 10)
# # mtry (number of variables randomly sampled as candidates at each split) = 31
# # splitrule = "gini"
# # min.node.size = 10
# # num.trees = 500

# ctrl <- trainControl(
#   method = "cv", 
#   number = 10,            
#   classProbs = TRUE,
#   summaryFunction = multiClassSummary
# )

# # Train model with ranger
# rf_fit_cv10 <- train(
#   x = train_data,
#   y = train_target,
#   method = "ranger",
#   trControl = ctrl,
#   tuneGrid = expand.grid(
#     mtry = 31,              
#     splitrule = "gini",
#     min.node.size = 10
#   ),
#   num.trees = 500,
#   weights = observation_weights   
# )
