# Version 2 of Loading Data

pacman::p_load(rio, dplyr, tidyr)

directories <- paste0("./wisdm-dataset/raw/", list.dirs(path = "./wisdm-dataset/raw/", full.names = FALSE),"/")[c(3,4,6,7)]

df <- list()

for (i in c(1:4)) {
  filenames <- directories[i] %>% list.files(pattern = "*.txt",) %>% paste0(directories[i], .)
  temp <- lapply(filenames, function(i){
      user <- import(i, col.names = c("user","activity","sys_time","x_axis","y_axis","z_axis"))
      user$z_axis <- as.double(sub(";$","",user$z_axis))
      user <- arrange(user, sys_time)
      user$sys_time <- (user$sys_time/1e9)# - user$sys_time[1]/1e9
      return(user)
    })
  names(temp) <- paste0('p', 1:51)
  df[[directories[i]]] <- temp
}

names(df) <- c('phone_accel','phone_gyro','watch_accel','watch_gyro')

# TODO: restructure. groupby %>% forcelength %>% pivot_wider

