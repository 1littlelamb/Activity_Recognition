# My max fft function for finding the most prominent frequency from the given window

source('functions/complex_magnitude.R')

max.fft <- function(data_) {
  
  pb.max.fft$tick()
  
  temp <- as.data.frame(data_) %>% 
    #dplyr::select(-Activity,-User) %>% 
    as.matrix() %>% 
    fft() %>% 
    complex_magnitude() %>% 
    as.data.frame %>% 
    slice(2:(ceiling(win/2)))
  
  freq <- apply(temp, 2, which.max) %>% as.data.frame()
  mag <- apply(temp, 2, max) %>% as.data.frame()
  
  return(cbind(freq, mag))
  
}
