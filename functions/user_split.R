# Splits the dataframe by user for train and test

user.split.kcv <- function(vect, splitRatio = 70, kFold, is.set.seed = TRUE, seed = 101) {
  
  if (is.set.seed == TRUE) set.seed(seed)
  
  indices <- sample(0:50)
  cuts <- cut(indices, breaks = kFold, labels = FALSE)
  folds <- list()
  
  for (i in 1:kFold) {
    test_users <- which(cuts == i)
    folds[[i]] <- !(vect %in% (test_users+1600))
  }
  return(folds)
}