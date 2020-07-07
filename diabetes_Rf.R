library(mlbench)
library(caTools)
data(PimaIndiansDiabetes)
PimaIndiansDiabetes
for (split_number in c(1:100)){
  train_ind <- sample.split(PimaIndiansDiabetes$Pregnancies,SplitRatio = 0:8)
  test_ind <- !train_ind
  rf <- randomForest(as.factor(Outcome) ~ ., data = PimaIndiansDiabetes[train_ind,],ntree=100)
  train_accuracy <- sum(diag(rf$confusion))/sum(train_ind)
  cm <- table(predict(rf,PimaIndiansDiabetes[test_ind,]),PimaIndiansDiabetes$Outcome[test_ind])
  test_accuracy <- sum(diag(cm))/sum(test_ind)
  
  all_train_accuracies_rf[split_number] <- train_accuracy
  all_test_accuracies_rf[split_number] <- test_accuracy
  
  importance <- rf$importance/sum(rf$importance)
  all_importances_rf[split_number,1] <- importance["Glucose",]
  all_importances_rf[split_number,2] <- importance["BMI",]
  all_importances_rf[split_number,3] <- importance["Age",]
  all_importances_rf[split_number,4] <- importance["Insulin",]
  all_importances_rf[split_number,5] <- importance["DiabetesPedigreeFunction",]
  all_importances_rf[split_number,6] <- importance["Pregnancies",]
  all_importances_rf[split_number,7] <- importance["BloodPressure",]
  all_importances_rf[split_number,8] <- importance["SkinThickness",]
}