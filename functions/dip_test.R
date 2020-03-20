# Classifying whether a signals distribution is multimodal

dips <- function(data_, colStats) {
  
  dip_classifier <- vector()
  
  watchCols <- grepl('W', names(data_))
  phoneCols <- grepl('P', names(data_))
  nWatch <- sum(watchCols)
  nPhone <- sum(phoneCols)
  
  if (nPhone > 0) {
    dip.phone <- "Unimodal"
    for (i in 1:nPhone) {
      if (diptest::dip(data_[,phoneCols][[i]]) > 0.04) dip.phone <- "Multimodal" ; break
    }
    dip_classifier[2] <- dip.phone
  }
  
  if (nWatch > 0) {
    dip.watch <- "Unimodal"
    for (i in 1:nWatch) {
      if (diptest::dip(data_[,watchCols][[i]]) > 0.04) dip.watch <- "Multimodal" ; break
    }
    dip_classifier[1] <- dip.watch
  } 
  
  return(as.data.frame(t(dip_classifier)))
  
}