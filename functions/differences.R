# Claculates the differences between mean of x, y, and z

differences <- function(x) {
  
  differencePA <- as.numeric(x['PAX'] - x['PAY'] - x['PAZ'])
  differencePG <- as.numeric(x['PGX'] - x['PGY'] - x['PGZ'])
  differenceWA <- as.numeric(x['WAX'] - x['WAY'] - x['WAZ'])
  differenceWG <- as.numeric(x['WGX'] - x['WGY'] - x['WGZ'])
  
  a <- c(diffPA = differencePA, diffPG = differencePG, 
         diffWA = differenceWA, diffWG = differenceWG)
 
  return(a)
}