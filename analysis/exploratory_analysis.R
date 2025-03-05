library(tidyverse)
library(ggplot2)

# read in all simulation results

rda_files <- list.files(here::here("data"), pattern = "\\.RDA$", full.names = TRUE)

all_results = NA
for (i in 1:length(rda_files)) {
  load(rda_files[i])
  tmp = bind_rows(results)
  all_results = rbind(all_results, tmp)
}

all_results = na.omit(all_results)


# bias of beta hat
# Wald and bootstrap interval only impact CI, use one of them to calculate bias
dat_bias = all_results %>% filter(method == "lm") %>% group_by(true_beta, error_distribution) %>%
  summarise(beta_hat_avg = mean(beta_hat), se = sum(beta_hat - beta_hat_avg)^2, n = n()) %>%
  mutate(beta_bias = beta_hat_avg - true_beta, bias_se = sqrt(se/(n*(n-1))),
         lower = beta_bias - 1.96*bias_se, upper = beta_bias + 1.96*bias_se)

p = ggplot(dat_bias, aes(x = error_distribution, y = beta_bias)) + geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  geom_hline(aes(yintercept = 0), lty = "dashed") + 
  facet_grid(cols = vars(true_beta),
             labeller = labeller(true_beta = function(x) paste0("True beta = ", x))) + 
  labs(x = "Error distribution", y = "Bias of estimated beta") + 
  theme(text = element_text(size = 22))

png(here::here("result","figure_for_bias.png"), width = 720, height = 720)
p
dev.off()


# coverage of beta hat

dat_coverage = all_results %>% mutate(coverage = (lower <= true_beta & upper >= true_beta)) %>% 
  group_by(true_beta, error_distribution, method) %>%
  summarise(coverage = mean(coverage), n = n()) %>% 
  mutate(coverage_se = sqrt(coverage * (1 - coverage) / n), 
         coverage_lower = coverage - 1.96*coverage_se, coverage_upper = coverage + 1.96*coverage_se)

p = ggplot(dat_coverage, aes(x = error_distribution, y = coverage, color = method)) + 
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = coverage_lower, ymax = coverage_upper), width = 0.2, position = position_dodge(width = 0.4)) +
  geom_hline(aes(yintercept = 0.95), lty = "dashed") +
  facet_grid(cols = vars(true_beta),
             labeller = labeller(true_beta = function(x) paste0("True beta = ", x))) + 
  labs(x = "Error distribution", y = "Coverage") + 
  theme(text = element_text(size = 22), legend.position = "bottom")

png(here::here("result","figure_for_coverage.png"), width = 720, height = 540)
p
dev.off()

# Power for true beta != 0

dat_power = all_results %>% 
  group_by(true_beta, error_distribution, method) %>%
  summarise(value = mean((lower > 0) | (upper < 0)), n = n()) %>%
  mutate(value_se = sqrt(value * (1 - value) / n), 
         value_lower = value - 1.96*value_se, value_upper = value + 1.96*value_se)

dat_power$true_beta = factor(dat_power$true_beta, 
                             levels = c(0,0.5),
                             labels = c("Type I error", "Power"))


p = ggplot(dat_power, aes(x = error_distribution, y = value, color = method)) + 
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = value_lower, ymax = value_upper), width = 0.2, position = position_dodge(width = 0.4)) +
  geom_hline(aes(yintercept = 0.8), lty = "dashed") +
  geom_hline(aes(yintercept = 0.05), lty = "dashed") +
  facet_grid(cols = vars(true_beta)) + 
  labs(x = "Error distribution", y = "Values") + 
  theme(text = element_text(size = 22), legend.position = "bottom")

png(here::here("result","figure_for_power.png"), width = 720, height = 540)
p
dev.off()

# computation time
dat_time = all_results %>% group_by(method) %>%
  summarise(time = mean(time)) 

write.csv(dat_time, here::here("result","time.csv"), row.names = F)

