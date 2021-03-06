## Overview

"binomial" is a minimal [R](http://www.r-project.org/) package that provides functions to calculate probabilities of a Binomial random variable.
There are three types of functions in this package:

1.private: these are auxiliary functions not intended to be called by the user.

  * `check_prob()`  test if an input prob is a valid probability value 
  * `check_trials()` test if an input trials is a valid value for number of trials
  * `check_success()` test if an input success is a valid value for number of successes 
  * `aux_mean()` returns the mean of the expression
  * `aux_variable()` returns the variance of the expression
  * `aux_mode()` returns the mode of the expression
  * `aux_skewness()` returns the skewness of the expression
  * `aux_kurtosis()` returns the kurtosis of the expression
  
2.main: these are the main functions that the user is expected to invoke.
  * `bin_choose()` calculates the number of combinations in which k successes can occur in n trials
  * `bin_probability()` calculates the probability of combinations in which k successes can occur in n trials
  * `bin_distribution()` creates a binomial object (of class `"bindis"`))
  * `bin_cumulative()` creates a binomial object (of class `"bincum"`))
  * `bin_variable()` creates a binomial object (of class `"binvar"`))
  * `bin_mean()` returns a summary measure of "mean" attribute
  * `bin_variance()` returns a summary measure of "variance" attribute
  * `bin_mode()` returns a summary measure of "mode" attribute
  * `bin_skewness()` returns a summary measure of "skewness" attribute
  * `bin_kurtosis()` returns a summary measure of "kurtosis" attribute
  
3.methods: these are also public functions that support classes of objects generated by some of the main functions.

  * `print.binvar()` print the content of an object "binvar"
  * `summary.binvar()` print a summary description of an object "binvar"
  * `print.summary.binvar()` a print method for a `"binvar"` object.
  * `plot.bindis()` graphs a barplot to display the probability histogram of a binomial distribution object "bindis"
  * `plot.bincum()` graphs the cumulative distribution in ab object "bincum"

## Motivation

This package has been developed to illustrate some of the concepts behind the creation of an R package.

## Installation

Install the development version from GitHub via the package `"devtools"`:
```{r}
# development version from GitHub:
#install.packages("devtools") 
# install "binomial" (without vignettes)
devtools::install_github("Jiangying-Zhong/binomial")
# install "binomial" (with vignettes)
devtools::install_github("Jiangying-Zhong/binomial", build_vignettes = TRUE)
```

## Usage

```{r}
library(binomial)
# invoke bin_choose function
bin_choose(n = 5, k = 2)
bin_choose(5, 0)
bin_choose(5, 1:3)

# probability of getting 2 successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 2, trials = 5, prob = 0.5)
# probabilities of getting 2 or less successes in 5 trials
# (assuming prob of success = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)
# 55 heads in 100 tosses of a loaded coin with 45% chance of heads
bin_probability(success = 55, trials = 100, prob = 0.45)

# binomial probability distribution
bin_distribution(trials = 5, prob = 0.5)
# plotting binomial probability distribution
plot(bin_distribution(trials = 5, prob = 0.5))

# binomial cumulative distribution
bin_cumulative(trials = 5, prob = 0.5)
# plotting binomial cumulative distribution
plot(bin_cumulative(trials = 5, prob = 0.5))

# invoke bin_variable function
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1
# invoke summary.binvar function
summary(bin_variable(trials = 10, p = 0.3))

```

