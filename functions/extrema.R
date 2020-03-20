# Finding the extrema relative to the mean

extrema <- function(data_) {

  E.col <- colMeans(data_)
  max.col <- sapply(data_, max) %>% as.data.frame()
  min.col <- sapply(data_, min) %>% as.data.frame()
  
  return(apply(rbind(t(max.col-E.col),t(E.col-min.col)), 2, max))
  
}