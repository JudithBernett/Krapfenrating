transpose_background <- function(d){
  
  rater_names <- d$Rater
  d[, Rater := NULL]
  dt <- transpose(d)
  setnames(dt, rater_names)
  
  dt
}