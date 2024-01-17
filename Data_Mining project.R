mydata<-read.csv("D:/diabetes_prediction_dataset.csv")
options(max.print = 10000000) 
mydata

view(mydata)
head(mydata)
tail(mydata)
str(mydata)
summary(mydata)

missing_values <- colSums(is.na(mydata))
print(missing_values) 



data_norm <- setdiff(names(mydata), c("gender", "smoking_history"))
mydata[data_norm] <- scale(mydata[data_norm])


head(data_norm)
tail(data_norm)
data_norm
mydata[data_norm]





predictor_cols <- names(mydata[data_norm])[-ncol(mydata[data_norm])]
predictor_cols
target_col <- names(mydata[data_norm])[ncol(mydata[data_norm])]
target_col 

set.seed(123)
train_indices <- sample(1:nrow(mydata[data_norm]), round(0.7 * nrow(mydata[data_norm])))
train_data <- mydata[data_norm][train_indices, predictor_cols]
train_labels <- mydata[data_norm][train_indices, target_col]
test_data <- mydata[data_norm][-train_indices, predictor_cols]
test_labels <- mydata[data_norm][-train_indices, target_col]



knn_with_distance_measure <- function(train_data, test_data, train_labels, k,
                                      distance_measure) {
  predicted_labels <- knn(train = train_data, test = test_data, cl = train_labels, k = k, prob =
                            TRUE, use.all = TRUE)
  return(predicted_labels)
}



k_values <- c(3, 5, 7)
accuracies <- vector()
for (k in k_values) {
  
  euclidean_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "euclidean")
  
  manhattan_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "manhattan")
  
  maximum_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                   "maximum")
  
  accuracy_euclidean <- sum(euclidean_predictions == test_labels) / length(test_labels)
  accuracy_manhattan <- sum(manhattan_predictions == test_labels) / length(test_labels)
  accuracy_maximum <- sum(maximum_predictions == test_labels) / length(test_labels)
  
  accuracies <- c(accuracies, accuracy_euclidean, accuracy_manhattan, accuracy_maximum)
  
  cat("Accuracy for k =", k, "\n")
  cat("Euclidean Distance:", accuracy_euclidean, "\n")
  cat("Manhattan Distance:", accuracy_manhattan, "\n")
  cat("Maximum Distance:", accuracy_maximum, "\n")
  cat("\n")
}


accuracy_df <- data.frame(Distance = rep(c("Euclidean", "Manhattan", "Maximum"),
                                         length(k_values)),
                          K = rep(k_values, each = 3),
                          Accuracy = accuracies)
accuracy_df




