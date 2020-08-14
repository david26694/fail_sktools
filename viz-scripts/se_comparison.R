library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)

theme_set(theme_minimal())

cv_results <- read.csv('results_regression/cv_results_se.csv')

cv_long <- cv_results %>% 
  pivot_longer(
    cols = split0_test_score:split11_test_score,
    names_to = c("split"),
    names_pattern = "split(.*)_test_score",
    values_to = "test_score"
  ) %>% 
  select(learner:test_score) %>% 
  mutate(
    test_score = -test_score,
    data = stringr::str_remove_all(data, 'data/'),
    data = stringr::str_remove_all(data, '.csv')
  )

cv_long %>% count(encoder)

cv_long <- cv_long %>% 
  filter(data != 'house_kaggle') %>% 
  mutate(
    encoder = case_when(
      encoder == 'me' ~ 'M-estimate',
      encoder == 'pe' ~ 'Quantile',
      encoder == 'se' ~ 'Summary'
    )
  )

levels <- c('Summary', 'Quantile', 'M-estimate')

cv_long$encoder <- factor(cv_long$encoder, levels = levels)

cv_long %>% 
  group_by(data, encoder) %>% 
  summarise(
    mean_score = mean(test_score),
    sd_score = sd(test_score)
  ) %>% 
  ggplot(aes(x = encoder, y = mean_score)) +
  geom_point() + 
  # geom_bar(stat = "identity", color = "black",
  #          position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_score - sd_score, ymax = mean_score + sd_score), 
                width = .2,
                position = position_dodge(.9)) +
  facet_wrap(~data, scales = 'free_y') +
  ylab('Cross validation MAE') + 
  ggsave('results_regression/summary_compare.png', width = 12, height = 9)
