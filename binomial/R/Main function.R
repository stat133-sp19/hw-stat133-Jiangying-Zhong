#' @title: function bin_choose()
#' @description: calculates the number of combinations in which k successes can occur in n trials
#' @param: success,trials
#' @return: the number of combinations in which k successes can occur in n trials
#' @export: 
#' @examples: 
#' bin_choose(n=5,k=2)
#' bin_choose(5,0)
#' bin_choose(5,1:3)
bin_choose <- function(trials,success){
  if(success > trials){
    stop("k cannot be greater than n.\n")
  }
  result <- factorial(trials)/(factorial(success)*factorial(trials-success))
  return (result)
}


#' @title: function bin_probability()
#' @description: evaluate the probability of getting success in trials
#' @param: success,trials,prob
#' @return: the probability of getting success in trials
#' @export:
#' @examples:
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  result <- bin_choose(trials,success)*(prob^success)*((1-prob)^(trials-success))
  return(result)
}


#' @title: function bin_distribution()
#' @description: create binomial probability distribution
#' @param: trials and prob
#' @return: an object of class \code{"bindis"}
#' @export:
#' @examples:
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  succcess <- 0:trials
  probability <- bin_probability(success = success,trials = trials,prob = prob)
  result <- list(success=c(0:trials),probability=probability)
  class(result) <- "bindis"
  return(result)
}

#' @export: a dataframe contained binomial probability distribution
print.bindis <- function(x){
  cat("\n\n")
  result <- data.frame(success = x$success,probability = x$probability)
  print(result)
  invisible(x)
}

#' @export: graphs a barplot to display the probability histgram of a binomial distribution object
plot_bindis <- function(x){
  barplot(x$probability,main = "binomial probability distribution",xlab = "successes",ylab = "probability",names.arg = c(0:5))
}


#' @title: function bin_cumulative()
#' @description: create binomial cumulative distribution
#' @param: trials and prob
#' @return: an object of class \code{"bindis"}
#' @export:
#' @examples:
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  probability <- bin_probability(success = success,trials = trials,prob = prob)
  result <- list(success = success,probability = probability,cumulative = cumsum(cumulative))
  class(result) <- "bincum"
  return(result)
}

#' @export: a dataframe contained cumulative distribution
print.bincum <- function(x){
  cat("\n\n")
  result <- data.frame(success = x$success,probability = x$probability,cumulative = x$cumulative)
  print(result)
  invisible(x)
}

#' @export: graphs the cumulative distribution in ab object
plot_bincum <- function(x){
  plot(x$cumulative,type = "o",main = "binomial cumulative distribution",xlab = "successes",ylab = "probability")
}

#' @title: function bin_variable()
#' @description: return a binomial random variable object
#' @param: trials and prob
#' @return: an object of class \code{"binvar}
#' @export:
#' @examples:
#' bin_variable(trials = 10, p = 0.3) 
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  obj <- list(trials = trials,prob = prob)
  class(obj) <- "binvar"
  return(obj)
}

#' @export: print the content of an object "binvar"
print.binvar <- function(x){
  cat("\"Binomial variable\"\n\n")
  cat("Parameters\n")
  cat("- number of trials:",x$trials)
  cat("\n")
  cat("- prob of success :",x$prob)
  invisible(x)
}

#'@export: a list of class "summary.binvar" 
summary.binvar <- function(x){
  mean <- aux_mean(x$trials,x$prob)
  variance <- aux_variance(x$trials,x$prob)
  mode <- aux_mode(x$trials,x$prob)
  skewness <- aux_skewness(x$trials,x$prob)
  kurtosis <- aux_kurtosis(x$trials,x$prob)
  result <- list(trials = x$trials,prob = x$prob,mean = mean, variance = variance,mode = mode,skewness = skewness,kurtosis = kurtosis)
  class(result) <- "summary.binvar"
  return(result)
}

#' @export: print the content of an object "summary.binvar"
print.summary.binvar <- function(x){
  print.binvar(x)
  cat("\n")
  cat("Measures\n")
  cat("- mean :",x$mean,"\n")
  cat("- variance:",x$variance,"\n")
  cat("- mode :",x$mode,"\n")
  cat("- skewness:",x$skewness,"\n")
  cat("- kurtosis:",x$kurtosis,"\n")
}


#' @title: functions of treasures
#' @description: calculate the attributions of a binominal distribution
#' @param: trials and prob
#' @return: a numeric object
#' @export:
#' @examples:
#' bin_mean(10, 0.3)
#' bin_variance(10, 0.3)
#' bin_mode(10, 0.3)
#' bin_skewness(10, 0.3)
#' bin_kurtosis(10, 0.3)
bin_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials,prob)
}

bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials,prob)
}

bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials,prob)
}

bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials,prob)
}

bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials,prob)
}


