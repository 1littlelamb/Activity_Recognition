# Computing the magnitude of the component accelerations.

require(dplyr)

magnitudes <- function(data_) {
  
  mag <- data.frame(seq(0,200))
  remaining <- vector()
  
  sensors <- c("PA", "PG", "WA", "WG")
  for (i in 1:4) {
    temp_mag <- data_ %>% dplyr::select(starts_with(sensors[i])) %>% 
      apply(1, function(x) sqrt((x[1]^2)+(x[2]^2)+(x[3]^2)))
    if (!any(is.na(temp_mag))) {mag <- cbind(mag, as.data.frame(temp_mag))}
    else {remaining[i] <- sensors[i]}
  }
  
  return(mag[-1] %>% 'names<-'(sensors))
}