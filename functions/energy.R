# Finding the total energy of the windowed signal

source('functions/complex_magnitude.R')

energy <- function(data_) {
  
  temp <- as.data.frame(data_) %>% 
    #dplyr::select(-Activity,-User) %>% 
    as.matrix() %>% 
    fft() %>% 
    complex_magnitude() %>% 
    as.data.frame %>% 
    slice(2:(ceiling(win/2)))
  
  energy_ <- colSums(temp)/(win/2) %>% as.data.frame() %>% t()
  
  return(energy_)
}
