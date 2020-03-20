# This will create a curated lightweight version of the main dataframe

pacman::p_load(dplyr, tidyr)

lighten <- function(data, actvt, samples) {
  
  # Error Handling
  if (samples[length(samples)] > 3567) {
    warning('Number of sample must be below 3567')
    break()
  }
  
  if (actvt == 'all') actvt <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "O", "P", "Q", "R", "S")
  
  # The main loop
  lwt <- data.frame()
  for (i in 1:51) {
    user_actvts <- data.frame()
    for (j in 1:length(actvt)) {
      user_actvt <- data %>% dplyr::filter(User == (i-1) + 1600) %>% dplyr::filter(Activity == actvt[j])
      if (length(user_actvt$User) == 0) next
      user_actvt <- user_actvt %>% slice(samples)
      
      user_actvts <- rbind(user_actvts, user_actvt)
    }
    lwt <- rbind(lwt, user_actvts)
  }
  return(lwt)
}
