# The covariance between axes of the different sensors

axis.var <- function(data_) {
  pb.axis.var$tick()
  return(sapply(data_, var))
}