library(broom)
library(tidyverse)

get_estimates = function(model_fit, true_beta){
  
  tidy(model_fit, conf.int = TRUE) %>%
    filter(term == "x") %>%
    mutate(coverage_lower = ifelse(true_beta >= conf.low, 1, 0),
           coverage_upper = ifelse(true_beta <= conf.high, 1, 0),
           method = "lm") %>%
    rename(beta_hat = estimate) %>%
    dplyr::select(beta_hat, std.error, coverage_lower, coverage_upper, method)
}


# beta_hat is the beta estimate from the original sample
# true_beta is the true parameter value
get_bootstrap_coverage = function(bootstrap_df, beta_hat, true_beta, alpha = 0.05){
  
  boot_se = sd(bootstrap_df$beta_hat_boot, na.rm = TRUE)

  boot_ci = quantile(bootstrap_df$beta_hat_boot, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
  
  tibble(beta_hat = beta_hat, method = "percentile", std.error = boot_se) %>%
    mutate(coverage_lower = ifelse(true_beta >= min(boot_ci), 1, 0),
           coverage_upper = ifelse(true_beta <= max(boot_ci), 1, 0)) %>%
    dplyr::select(beta_hat, std.error, coverage_lower, coverage_upper, method)
}