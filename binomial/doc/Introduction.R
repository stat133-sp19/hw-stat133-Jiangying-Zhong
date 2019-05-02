## ------------------------------------------------------------------------
#knitr::opts_chunk$set(collapse = T, comment = "#>")
library(binomial)

## ------------------------------------------------------------------------
bin_choose(n=5,k=2)
bin_probability(success=2,trials=5,prob=0.5)
bin_distribution(trials = 5, prob = 0.5)
plot(bin_distribution(trials = 5, prob = 0.5))
bin_cumulative(trials = 5, prob = 0.5)
plot(bin_cumulative(trials = 5, prob = 0.5))
bin_variable(trials = 10, prob = 0.3)
summary(bin_variable(trials = 10, prob = 0.3))

