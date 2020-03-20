# Function for creating a common activity length for each activity and user

truncate_to_min_activity <- function(data){
  
  missing_activities <- data.frame()
  record_min <- 1e6
  activity_list = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "O", "P", "Q", "R", "S") # Can't use Letters b/c no 'N'
  
  # Finding min and missing activities
  for (i in 1:length(data)) {
    for (j in 1:51) {
      temp_count <- table(data[[i]][[j]][,2])
      if (length(names(temp_count)) < 18) {
        missing_activities <- rbind(missing_activities, data.frame(p = j, letter = paste(setdiff(activity_list, names(temp_count)))))
      }
      if (min(temp_count) < record_min) {
        record_min <- min(temp_count)
        record_i <- i
        record_j <- j
        record_loc <- which.min(temp_count)
      }
    }
  }
  # Removing incomplete/missing acitivities
  for (i in 1:dim(missing_activities)[1]) {
    for (j in 1:length(data)) {
      data[[j]][[missing_activities$p[i]]] <- data[[j]][[missing_activities$p[i]]] %>%  filter(!(V2 %in% missing_activities$letter[i]))
    }
  }
  
  # Truncating all activities
  for (i in 1:length(data)) {
    for (j in 1:51) {
      temp_df <- data.frame()
      for (k in LETTERS[1:18]) {
        temp_activity <- data[[i]][[j]] %>% filter(V2 == k)
        if (dim(temp_activity)[1] > 0) {
          temp_activity <- temp_activity[1:record_min,]
        }
        temp_df <- rbind(temp_df, temp_activity)
      }
      data[[i]][[j]] <- temp_df
    }
  }
  return(data)
}
