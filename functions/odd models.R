features <- readRDS('features.rds')

### QDA ####
set.seed(102)
sample.qda <- sample.split(features$Activity, SplitRatio = 0.70)

train.data.qda <- features[sample.qda,] %>% dplyr::select(-User)
test.data.qda <- features[!sample.qda,] %>% dplyr::select(-User)

model_qda <- qda(Activity ~ ., train.data.qda)
predictions_qda <- predict(model_qda, test.data.qda)

(qda_acc1 <- mean(predictions_qda$class == test.data.qda$Activity))

### KNN ####
set.seed(102)
sample.knn <- sample.split(features$Activity, SplitRatio = 0.70)

train.data.knn <- features[sample.knn,] %>% dplyr::select(-Activity, -User)
test.data.knn <- features[!sample.knn,] %>% dplyr::select(-Activity, -User)
train.response.knn <- features[sample.knn,] %>% pull(Activity)
test.response.knn <- features[!sample.knn,] %>% pull(Activity)

model_knn <- knn(train.data.knn, test.data.knn, train.response.knn, k=6)

(knn_acc1 <- mean(model_knn == test.response.knn))
