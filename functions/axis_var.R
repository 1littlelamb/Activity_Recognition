# The covariance between axes of the different sensors

axis.var <- function(data_) {
  return(sapply(data_, var))
}