# Writing my own roll apply function

rollApply <- function(data_, win, FUN, by = 1, fullStep = FALSE){
    
    if (fullStep == TRUE) by = win
    
    pb <- txtProgressBar(min = 0,
                         max = ceiling((dim(lwt)[1] - win + 1)/by),
                         char = '>')
    
    if (is.vector(data_)) {
        outbound <- vector()
        for (i in 1:ceiling((length(data_) - win + 1)/by)){
            setTxtProgressBar(pb, i)
            outbound[i] <- FUN(data_[((i*by)-by+1):(((i*by)-by+1) + win - 1)])
        }
        close(pb)
        return(outbound)
    }
    
    if (is.data.frame(data_)) {
        outbound <- data.frame()
        for (i in 1:ceiling((dim(data_)[1] - win + 1)/by)){
            setTxtProgressBar(pb, i)
            temp <- data_ %>% slice(((i*by)-by+1):(((i*by)-by+1) + win - 1)) %>% FUN
            outbound <- rbind(outbound, temp)
        }
        close(pb)
        return(outbound)
    }
}
