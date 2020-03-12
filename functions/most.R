# Find the mode

most <- function(v) {
  if (!is.numeric(v)) warning('Must be type numeric. If char use max()')
  
  m <- tabulate(v + ifelse(min(v) <= 0, abs(min(v))+1, 0)) %>% which.max()
  return(m)
}
