# The correlation between each axis of any given sensor

colCorr <- function(data_) {
  
  temp <- cor(data_, method = 'kendall')
  xy.yz.vect.dirty <- diag(temp[-ncol(temp),-1])
  xz.vect.dirty <- diag(temp[-c(ncol(temp),ncol(temp)-1),-c(1,2)])
  
  xz.vect.clean <- vector()
  for (i in 1:length(xz.vect.dirty)) {
    if (i%%3 == 1) {xz.vect.clean[ceiling(i/3)] <- xz.vect.dirty[i]}
  }
  
  xy.yz.vect.clean <- vector()
  for (i in 1:length(xy.yz.vect.dirty)) {
    if (i%%3 %in% c(1,2)) {xy.yz.vect.clean[i] <- xy.yz.vect.dirty[i]}
  }
  xy.yz.vect.clean <- na.omit(xy.yz.vect.clean)
  
  xy.yz.counter <- 1
  xz.counter <- 1
  
  correlations <- vector()
  for (i in 1:(length(xy.yz.vect.clean) + length(xz.vect.clean))) {
    if (i%%3 %in% c(0,1)) {
      correlations[i] <- xy.yz.vect.clean[xy.yz.counter]
      xy.yz.counter <- xy.yz.counter + 1
    }
    if (i%%3 == 2) {
      correlations[i] <- xz.vect.clean[xz.counter]
      xz.counter <- xz.counter + 1
    }
  }
  return(correlations)
}
  