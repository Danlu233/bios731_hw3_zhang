# load libraries
library(tidyverse)

# function to simulate data
# n: sample size
# beta_treat: true beta of treatment
# error_distribution: either Normal or Gamma
# output includes x and y

get_simdata = function(n, beta_treat, error_distribution){
  beta0 = 1
  x = rbinom(n, 1, prob = 0.5)

  if(error_distribution == "Normal"){
    epsilon = rnorm(n, 0, sd = sqrt(2))
  } else if (error_distribution == "Gamma") {
    epsilon = rgamma(n, shape = 1, rate = 2)
  } else {
    errorCondition("Wrong distribution!")
  }


  y = beta0 + beta_treat * x + epsilon

  tibble(
    x = x,
    y = y
  )

}



