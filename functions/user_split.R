# Splits the dataframe by user for train and test

user.split <- function(v, cutoff = 38) return(v <= (1600 + (cutoff-1)))
