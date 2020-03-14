# Computing the magnitudes of the 4 accelerations.

magnitudes <- function(data_) {
  
  pa_mag <- data_ %>% subset(select = 1:3) %>% apply(1, function(x) sqrt((x[1]^2)+(x[2]^2)+(x[3]^2)))
  pg_mag <- data_ %>% subset(select = 4:6) %>% apply(1, function(x) sqrt((x[1]^2)+(x[2]^2)+(x[3]^2)))
  wa_mag <- data_ %>% subset(select = 7:9) %>% apply(1, function(x) sqrt((x[1]^2)+(x[2]^2)+(x[3]^2)))
  wg_mag <- data_ %>% subset(select = 10:12) %>% apply(1, function(x) sqrt((x[1]^2)+(x[2]^2)+(x[3]^2)))
  
  pa_mag <- pa_mag %>% as.data.frame() %>% 'names<-'('PA')
  pg_mag <- pg_mag %>% as.data.frame() %>% 'names<-'('PG')
  wa_mag <- wa_mag %>% as.data.frame() %>% 'names<-'('WA')
  wg_mag <- wg_mag %>% as.data.frame() %>% 'names<-'('WG')
  
  return(cbind(PA = pa_mag, PG = pg_mag, WA = wa_mag, WG = wg_mag))
}