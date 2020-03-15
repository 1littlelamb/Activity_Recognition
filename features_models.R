# Feature extraction and model making - Felix Slothower

#TODO: 
#   - Include the diptest function
#   - Test the effect of removing the gyroscopic data
#   - Fix the magnitudes function to allow for the above

setwd('C:/Users/felix/Documents/UCI Bullshit Forms/CLASSES/MAE 195 (Machine Learning)/Actitivty_Recognition/code/Activity_Recognition/')

pacman::p_load(rio, dplyr, tidyr, caTools, caret, e1071, MASS, zoo, class, randomForest, progress,
               diptest)
source('functions/lighten.R')
source('functions/differences.R')
source('functions/user_split.R')
source('functions/most.R')
source('functions/magnitudes.R')
source('functions/max_fft.R')
source('functions/axis_var.R')
source('functions/extrema.R')
source('functions/roll_apply.R')

# Import the data
data <- import("./../../rds_data/wisdm_dataset_df.rds")

# Creating a lightweight sample of the data
lwt <- lighten(data, c("G"), samples = 2000) %>% dplyr::select(-starts_with(c('PG','WG')))

# Creating the Mean feature
#difference <- t(apply(as.matrix(means), 1, differences)) %>% as.data.frame()
win <- 200
by <- 100

pb.mag      <- progress_bar$new(total = ceiling((dim(lwt)[1] - win + 1)/by))
pb.extrm    <- progress_bar$new(total = ceiling((dim(lwt)[1] - win + 1)/by))
pb.max.fft  <- progress_bar$new(total = ceiling((dim(lwt)[1] - win + 1)/by))
pb.axis.var <- progress_bar$new(total = ceiling((dim(lwt)[1] - win + 1)/by))

usr   <- lwt$User     %>% rollApply(win, most, by) %>% as.data.frame() %>% 'names<-'('User') 
actvt <- lwt$Activity %>% rollApply(win,  max, by) %>% as.data.frame() %>% 'names<-'('Activity')

means <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win, colMeans, by)
ffts  <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,  max.fft, by) ; pb.max.fft$terminate()
vars  <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win, axis.var, by) ; pb.axis.var$terminate()
extrm <- lwt %>% dplyr::select(-c("User", "Activity")) %>% rollApply(win,  extrema, by) ; pb.extrm$terminate()
#TODO include diptest feature

E.mag <- means %>% magnitudes() ; pb.mag$terminate()

names(E.mag) <- paste0(c("PA", "WA"), '.Emag')
names(ffts)  <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.mf')
names(vars)  <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.var')
names(extrm) <- paste0(names(lwt %>% dplyr::select(-c("User", "Activity"))), '.ext')

features <- cbind(E.mag, ffts, vars, extrm, actvt, usr)
actvt_and_usr_cols <- c(dim(features)[2]-1,dim(features)[2])
features[,-actvt_and_usr_cols] <- scale(features[,-actvt_and_usr_cols])

#saveRDS(features, 'features.rds')

### QDA2 ####
set.seed(102)
sample.qda <- user.split(features$User)

train.data.qda <- features[sample.qda,]
test.data.qda <- features[!sample.qda,]

model_qda <- qda(Activity ~ ., train.data.qda)
predictions_qda <- predict(model_qda, test.data.qda)

(qda_acc2 <- mean(predictions_qda$class == test.data.qda$Activity))

### KNN2 ####
set.seed(101)
sample.knn <- user.split(features$User)

train.data.knn <- features[sample.knn,] %>% dplyr::select(-Activity, -User)
test.data.knn <- features[!sample.knn,] %>% dplyr::select(-Activity, -User)
train.response.knn <- features[sample.knn,] %>% pull(Activity)
test.response.knn <- features[!sample.knn,] %>% pull(Activity)

model_knn <- knn(train.data.knn, test.data.knn, train.response.knn, k=20)

(knn_acc2 <- mean(model_knn == test.response.knn))

### SVM2 ####
set.seed(102)
sample.svm <- user.split(features$User)

train.data.svm <- features[sample.svm,] %>% dplyr::select(-User)
test.data.svm <- features[!sample.svm,] %>% dplyr::select(-User)
train.response.svm <- features[sample.svm,] %>% pull(Activity)
test.response.svm <- features[!sample.svm,] %>% pull(Activity)

# Linear
model_svm_linear <- svm(Activity ~ ., train.data.svm, kernel = 'linear')
predictions_svm_linear <- predict(model_svm_linear, test.data.svm)

(svm_acc_linear2 <- mean(predictions_svm_linear == test.response.knn))

# Polynomial
model_svm_poly <- svm(Activity ~ ., train.data.svm, kernel = 'polynomial')
predictions_svm_poly <- predict(model_svm_poly, test.data.svm)

(svm_acc_poly2 <- mean(predictions_svm_poly == test.response.svm))

#tune.results <- tune(svm, train.data.svm[,-49], train.response.svm, kernel = 'polynomial',
#                     ranges = list(cost = 10^(-1:2), degree = c(1:4)))
#
#pl1 <- plot(tune.results, main = "Performance of SVM with Polynomial Kernel", 
#            xlab = 'Cost', ylab = 'Degree')
#
#tune.results.z1 <- tune(svm, train.data.svm[,-49], train.response.svm, kernel = 'polynomial',
#                        ranges = list(cost = seq(10,25,5), degree = c(2:4)))
#
#pl2 <- plot(tune.results.z1, main = "Performance of SVM with Polynomial Kernel", 
#            xlab = 'Cost', ylab = 'Degree')
#
#tune.results.z2 <- tune(svm, train.data.svm[,-49], train.response.svm, kernel = 'polynomial',
#                        ranges = list(cost = seq(1,10,2), degree = c(2:4)))
#
#pl3 <- plot(tune.results.z2, main = "Performance of SVM with Polynomial Kernel", 
#            xlab = 'Cost', ylab = 'Degree')

# These ended up being the best and were also the default values. Welp, worth a shot.
model_svm_poly_tuned <- svm(Activity ~ ., train.data.svm, kernel = 'polynomial', degree = 3, cost = 1)
predictions_svm_poly_tuned <- predict(model_svm_poly_tuned, test.data.svm)

(svm_acc_poly_tuned2 <- mean(predictions_svm_poly_tuned == test.response.svm))

# Radial
model_svm_rad <- svm(Activity ~ ., train.data.svm, kernel = 'radial')
predictions_svm_rad <- predict(model_svm_rad, test.data.svm)

(svm_acc_rad2 <- mean(predictions_svm_rad == test.response.knn))

### RF2 ####
set.seed(101)
sample.rf <- user.split(features$User)

train.data.rf <- features[sample.rf,] %>% dplyr::select(-User)
test.data.rf <- features[!sample.rf,] %>% dplyr::select(-User)
test.response.rf <- features[!sample.rf,] %>% pull(Activity)

model_rf <- randomForest(Activity ~ ., train.data.rf, mtry = 7)
predictions_rf <- predict(model_rf, test.data.rf)

(rf_acc <- mean(predictions_rf == test.response.rf))

