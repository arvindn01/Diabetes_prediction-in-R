library(mlbench)
library(caTools)
all_test_accuracies_knn <- matrix(nrow=100,ncol=9)
for (split_number in c(1:100)){
  train_ind <- sample.split(Diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  
  neighbors <- c(2:10)
  accuracies <- matrix(nrow=1, ncol=9)
  
  for (n_neighbors in neighbors){
    knn_fit <- knn(Diabetes[train_ind,],Diabetes[test_ind,],diabetes$Outcome[train_ind],k=n_neighbors)
    cm <- table(Actual = Diabetes$Outcome[test_ind],Predicted = knn_fit)
    accuracy <- sum(diag(cm))/sum(test_ind)
    accuracies[n_neighbors-1] <- accuracy
  }
  all_test_accuracies_knn[split_number,] <- accuracies
}