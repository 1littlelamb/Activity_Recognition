# Feature extraction and model making - Felix Slothower
# setwd('C:/Users/felix/Documents/UCI Bullshit Forms/CLASSES/MAE 195 (Machine Learning)/Actitivty_Recognition/code/Activity_Recognition/')

windowsFonts(`LM Roman 10` = windowsFont('LM Roman 10'))

pacman::p_load(rio, dplyr, tidyr, caTools, caret, e1071, MASS, zoo, class, randomForest, utils,
               diptest, knnGarden)
source('functions/lighten.R',)
source('functions/differences.R')
source('functions/user_split.R')
source('functions/most.R')
source('functions/magnitudes.R')
source('functions/max_fft.R')
source('functions/energy.R')
source('functions/axis_var.R')
source('functions/extrema.R')
source('functions/dip_test.R')
source('functions/energy.R')
source('functions/column_correlation.R')
source('functions/position.R')
source('functions/roll_apply.R')

# Import the data
data <- import("rds_datasets/wisdm_dataset_df.rds")

# Creating a lightweight sample of the data
lwt <- lighten(data, 'all', samples = 1:3200) %>% dplyr::select(-Time)

# Creating the Mean feature
#difference <- t(apply(as.matrix(means), 1, differences)) %>% as.data.frame()
win <- 200
by <- 100

usr   <- lwt$User     %>% rollApply(win, most, by) %>% as.data.frame() %>% 'names<-'('User') 
actvt <- lwt$Activity %>% rollApply(win,  max, by) %>% as.data.frame() %>% 'names<-'('Activity')

means  <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win, colMeans, by)
corr   <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,  colCorr, by)
ffts   <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,  max.fft, by)
vars   <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win, axis.var, by)
extrm  <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,  extrema, by)
dip    <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,     dips, by)
energies <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win, energy, by)
positions <- lwt %>% dplyr::select(-c("User", "Activity")) %>% 
             dplyr::select(starts_with(c("PA", "WA"))) %>% rollApply(win, colPosition, by)

dip <- sapply(dip, factor)

names(dip)   <- paste0(c('P','W'), '.DIP')
names(means) <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.E')
names(ffts)  <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.mf')
names(vars)  <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.var')
names(extrm) <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.ext')
names(energies) <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.energy')
names(positions) <- paste0(names(positions), '.pos')
names(corr)  <- paste0(c(rep("PA",3), rep("PG",3), rep("WA",3), rep("WG",3)),
                       c("xy", "xz", "yz"),
                       ".cor")

features <- cbind(means, ffts, vars, extrm, energies, positions, corr, dip, actvt, usr)
actvt_and_usr_cols <- c(dim(features)[2]-1,dim(features)[2])
features[,-actvt_and_usr_cols] <- scale(features[,-actvt_and_usr_cols])

saveRDS(features, 'rds_datasets/features.rds')

###################################################################################################


#Setting the k-folds to be used for cross validation
k.fold = 5
folds <- user.split.kcv(features$User, kFold = k.fold)

### QDA ####
qda_acc <- vector()
for (i in 1:k.fold) {
  set.seed(102)
  
  train.data.qda <- features[folds[[i]],]
  test.data.qda <- features[!folds[[i]],]
  
  model_qda <- qda(Activity ~ ., train.data.qda)
  predictions_qda <- predict(model_qda, test.data.qda)
  
  (qda_acc[i] <- mean(predictions_qda$class == test.data.qda$Activity))
}
(qda_acc_avg <- mean(qda_acc))

### KNN ####
knn_acc <- vector()
for (i in 1:k.fold) {
  set.seed(101)
  sample.knn <- user.split(features$User)
  
  train.data.knn <- features[folds[[i]],] %>% dplyr::select(-Activity, -User)
  test.data.knn <- features[!folds[[i]],] %>% dplyr::select(-Activity, -User)
  train.response.knn <- features[folds[[i]],] %>% pull(Activity)
  test.response.knn <- features[!folds[[i]],] %>% pull(Activity)
  
  model_knn <- knn(train.data.knn, test.data.knn, train.response.knn, k=138)
  
  (knn_acc[i] <- mean(model_knn == test.response.knn))
}
(knn_acc_avg <- mean(knn_acc))

### SVM ####
svm_acc_linear <- vector()
svm_acc_poly <- vector()
svm_acc_radial <- vector()
for (i in 1:k.fold) {
  set.seed(102)
  
  train.data.svm <- features[folds[[i]],] %>% dplyr::select(-User)
  test.data.svm <- features[!folds[[i]],] %>% dplyr::select(-User)
  train.response.svm <- features[folds[[i]],] %>% pull(Activity)
  test.response.svm <- features[!folds[[i]],] %>% pull(Activity)

  # Linear
  model_svm_linear <- svm(Activity ~ ., train.data.svm, kernel = 'linear')
  predictions_svm_linear <- predict(model_svm_linear, test.data.svm)
  
  (svm_acc_linear[i] <- mean(predictions_svm_linear == test.response.svm))
  
  # Polynomial
  model_svm_poly <- svm(Activity ~ ., train.data.svm, kernel = 'polynomial')
  predictions_svm_poly <- predict(model_svm_poly, test.data.svm)
  
  (svm_acc_poly[i] <- mean(predictions_svm_poly == test.response.svm))
  
  # Tuning of the Polynomial Kernel
  #poly.tune <- tune(svm, train.data.svm, train.response.svm, kernel = 'polynomial',
  #                     ranges = list(cost = 10^(-1:2), degree = c(1:4)))

  # Radial
  model_svm_rad <- svm(Activity ~ ., train.data.svm, kernel = 'radial', gamma = 0.01, cost = 5)
  predictions_svm_rad <- predict(model_svm_rad, test.data.svm)
  
  # Tuning of the Radial Kernel
  #train.data.svm.tune <- features[folds[[i]],] %>% dplyr::select(-User, -Activity)
  #radial.tune <- tune(svm, train.data.svm.tune, train.response.svm, kernel = 'radial',
  #                    ranges = list(cost = 10^(-2:4), gamma = 10^(-5:4)))
  
  (svm_acc_rad[i] <- mean(predictions_svm_rad == test.response.svm))

}
(svm_acc_linear_avg <- mean(svm_acc_linear))
(svm_acc_poly_avg <- mean(svm_acc_poly))
(svm_acc_radial_avg <- mean(svm_acc_rad))
 
### RF ####
rf_acc <- vector()
for (i in 1:k.fold) {
  set.seed(108)
  
  train.data.rf <- features[folds[[i]],] %>% dplyr::select(-User)
  test.data.rf <- features[!folds[[i]],] %>% dplyr::select(-User)
  train.response.rf <- features[folds[[i]],] %>% pull(Activity)
  test.response.rf <- features[!folds[[i]],] %>% pull(Activity)
  
  model_rf <- randomForest(Activity ~ ., train.data.rf, mtry = 9)
  predictions_rf <- predict(model_rf, test.data.rf)
  
  (rf_acc[i] <- mean(predictions_rf == test.response.rf))
}
(rf_acc_avg <- mean(rf_acc))

#tuneRF(train.data.rf, train.response.rf, mtryStart = 9, stepFactor = 1.25, improve = 0.02,
#       ntreeTry = 250, trace = TRUE)

# Creating a pretty plot of the importance of the top 20 most important predictors
predictorImportance <- importance(model_rf, scale = FALSE)
dotchart(sort(predictorImportance[1:30,]), family = "LM Roman 10", xlab = "Mean Decrease in Gini Coef.",
         main = "Predictor Importance", pch = 16, color = "#a63a61")


# Assembling accuracies into a table to present in the report
# This same code chunk is used for all accuracy tables and the saved .rds file is given new names
# for each different table.
accuracies <- signif(c(qda_acc_avg, knn_acc_avg,
                       svm_acc_linear_avg, svm_acc_poly_avg,
                       svm_acc_radial_avg, rf_acc_avg)*100, digits = 4)
accuracies <- paste0(accuracies, '%')
names(accuracies) <- c("QDA", "KNN", "SVM-Linear", "SVM-Polynomial", "SVM-Radial", "RF")
table.acc <- data.frame(Accuracy = accuracies)
saveRDS(table.acc, "tables/all_acc.rds")
