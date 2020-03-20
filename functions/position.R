# Calculate the change in position of the sensor using integration

colDisplacement <- function(data_) {
  
  position <- function(acc.vect) {
    vel.vect <- vector()
    pos.vect <- vector()
    for (i in 1:(length(acc.vect)-1)) {
      vel.vect[i] <- (acc.vect[i] + (acc.vect[i+1]-acc.vect[i])/2)*(0.05)
    }
    for (i in 1:(length(vel.vect)-1)) {
      pos.vect[i] <- (vel.vect[i] + (vel.vect[i+1]-vel.vect[i])/2)*(0.05)
    }
    return(pos.vect[length(pos.vect)])
  }
  
  phone.acc <- data_ %>% dplyr::select(starts_with("PA")) %>% sapply(function(x) x*x) %>% 
               rowSums() %>% sqrt()
  
  watch.acc <- data_ %>% dplyr::select(starts_with("WA")) %>% sapply(function(x) x*x) %>% 
               rowSums() %>% sqrt()
  
  temp <- list(phone.acc, watch.acc)
  
  final_positions <- vector()
  for (i in 1:length(temp)[2]) {
    final_positions[i] <- sapply(temp[[i]], position)
  }
  
  final_positions <- final_positions %>% as.data.frame() %>% t()
  return(final_positions)
}