# private auxiliary functions
# input:trials and prob
# output: return the corresponding value from the computed summary measure
aux_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(trials*prob)
}

aux_variance <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(trials*prob*(1-prob))
}

aux_mode <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(as.integer(trials*prob+prob))
}

aux_skewness <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return((1-2*prob)/(sqrt(trials*prob*(1-prob))))
}

aux_kurtosis <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}
