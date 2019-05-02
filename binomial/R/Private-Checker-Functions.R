# private function to check probility
# input:prob
# output:test if prob is a valid probability value
check_prob <- function(prob){
  if((prob < 0)|(prob > 1)){
    stop("Invalid prob value!\n")
  }
  TRUE
}

# private function to check trials
# input:trials
# output:test if an input trials is a valid value for number of trials
check_trials <- function(trials){
  if(trials < 0){
    stop("Invalid trials value!\n")
  }
  TRUE
}

# private function to check success
# input:success and trials
# output:test if an input success is a valid value for number of successes
check_success <- function(success,trials){
  if((min(success) < 0)|(max(success) > trials)){
    stop("Invalid success value!\n")
  }
  TRUE
}
