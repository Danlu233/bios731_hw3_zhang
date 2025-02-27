library(broom)
library(tidyverse)

## fit linear regression model, given a certain dataset
fit_lm = function(simulated_data){
  lm(y ~ x, data = simulated_data)
}



## apply nonparametric bootstrap. Does one bootstrap iteration

# bootstrap_iter: bootstrap iteration number
do_bootstrap = function(bootstrap_iter, simulated_data,){

  rows = sample(1:nrow(simulated_data), size = nrow(simulated_data), replace = TRUE)
  bootstrap_data = simulated_data[rows,]
  fit = lm(y ~ x, data = bootstrap_data)

  tidy(fit) %>%
    filter(term == "x") %>%
    rename(beta_hat_boot = estimate) %>%
    mutate(boot_id = bootstrap_iter,
           boot_t_se = boot_t_se) %>%
    select(boot_id, beta_hat_boot, boot_t_se)
}

