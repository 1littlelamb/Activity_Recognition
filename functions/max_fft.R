# My max fft function for finding the most prominent frequency from the given window

max.fft <- function(data_) {
  
  pb.max.fft$tick()
  
  temp <- as.data.frame(data_) %>% 
    #dplyr::select(-Activity,-User) %>% 
    as.matrix() %>% 
    fft() %>% 
    magnitude() %>% 
    as.data.frame %>% 
    slice(2:(ceiling(win/2)))
  
  return(apply(temp, 2, which.max))
  
}
