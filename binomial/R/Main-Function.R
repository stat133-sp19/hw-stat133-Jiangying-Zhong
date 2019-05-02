#' @title function bin_choose()
#' @description Caculate the combinations "n choose k"
#' @param n the total number of trials
#' @param k the total number of successes
#' @return the combinations "n choose k"
#' @export
#' @examples
#' bin_choose(n=5,k=2)

bin_choose <- function(n,k){
  if(max(k) > n){
    stop("k cannot be greater than n.\n")
  }
  result <- factorial(n)/(factorial(k)*factorial(n-k))
  return (result)
}


#' @title function bin_probability()
#' @description evaluate the probability of getting success in trials
#' @param success the number of successes
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return the probability of getting success in trials
#' @export
#' @examples
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


#' @title function bin_distribution()
#' @description create binomial probability distribution
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"bindis"}
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  probability <- bin_probability(success = success, trials = trials, prob = prob)
  result <- list(success = success, probability = probability)
  class(result) <- "bindis"
  return(result)
}


# a dataframe contained binomial probability distribution
#' @export
print.bindis <- function(x){
  cat("\n\n")
  result <- data.frame(success = x$success,probability = x$probability)
  print(result)
  invisible(x)
}

#' @export
plot.bindis <- function(x){
  barplot(x$probability,main = "binomial probability distribution",xlab = "successes",ylab = "probability",names.arg = c(0:5))
}



#' @title function bin_cumulative()
#' @description create binomial cumulative distribution
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"bindis"}
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  probability <- bin_probability(success = success, trials = trials, prob = prob)
  result <- list(success = success, probability = probability, cumulative = cumsum(probability))
  class(result) <- "bincum"
  return(result)
}


#' @export
print.bincum <- function(x){
  cat("\n\n")
  result <- data.frame(success = x$success,probability = x$probability,cumulative = x$cumulative)
  print(result)
  invisible(x)
}



#' @export
plot.bincum <- function(x){
  plot(x$cumulative,type = "o",main = "binomial cumulative distribution",xlab = "successes",ylab = "probability")
}

#' @title function bin_variable()
#' @description return a binomial random variable object
#' @param trials the total number of the trials
#' @param prob the probability of success every time
#' @return an object of class \code{"binvar"}
#' @export
#' @examples
#' bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  obj <- list(trials = trials,prob = prob)
  class(obj) <- "binvar"
  return(obj)
}

#' @export
print.binvar <- function(x){
  cat("\"Binomial variable\"\n\n")
  cat("Parameters\n")
  cat("- number of trials:",x$trials)
  cat("\n")
  cat("- prob of success :",x$prob)
  invisible(x)
}

#' @export
summary.binvar <- function(x) {
  trials <- x$trials
  prob <- x$prob
  obj <- list(trials = trials,
              prob = prob,
              mean = aux_mean(trials = trials, prob = prob),
              variance = aux_variance(trials = trials,prob = prob),
              mode = aux_mode(trials = trials,prob = prob),
              skewness = aux_skewness(trials = trials,prob = prob),
              kurtosis = aux_kurtosis(trials = trials,prob = prob))
  class(obj) <- "summary.binvar"
  return(obj)
}

#' @export
print.summary.binvar <- function(x) {
  trials <- x$trials
  prob <- x$prob
  cat("\nSummary Binomial\n\n")
  cat("Paramaters")
  cat("\n- number of trials:", trials)
  cat("\n- prob of success:", prob)
  cat("\n\nMeasures")
  cat("\n- mean:",aux_mean(trials = trials,prob = prob))
  cat("\n- variance:",aux_variance(trials = trials,prob = prob))
  cat("\n- mode:",aux_mode(trials = trials,prob = prob))
  cat("\n- skewness:",aux_skewness(trials = trials,prob = prob))
  cat("\n- kurtosis:",aux_kurtosis(trials = trials,prob = prob))
}



#' @title functions of mean treasure
#' @description calculate the mean of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials,prob)
}

#' @title functions of variance treasure
#' @description calculate the variance of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials,prob)
}

#' @title functions of mode treasure
#' @description calculate the mode of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials,prob)
}

#' @title functions of skewness treasure
#' @description calculate the skewness of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials,prob)
}

#' @title functions of kurtosis treasure
#' @description calculate the kurtosis of a binominal distribution
#' @param trials the total number of trials
#' @param prob the probability of success every time
#' @return a numeric object
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials,prob)
}


