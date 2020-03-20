# Writing my own roll apply function

rollApply <- function(data_, window, FUN, by = 1, fullStep = FALSE){
    
    if (fullStep == TRUE) by = window
    
    outbound <- data.frame()
    
    for (i in 1:ceiling((dim(data_)[1] - window + 1)/by)){
        temp <- data_ %>% slice((i*by):((i*by) + window))
        temp <- FUN(temp)
        outbound <- rbind(outbound, temp)
    }
    
    
    
}