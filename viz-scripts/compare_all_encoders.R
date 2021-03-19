library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)

theme_set(theme_minimal())

cv_results <- read.csv('results_regression/cv_results_21.csv')

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
cv_long %>% count(data)

cv_long <- cv_long %>% 
  filter(data != 'house_kaggle') %>% 
  mutate(
    encoder = case_when(
      encoder == 'cat_e' ~ 'Catboost',
      encoder == 'me' ~ 'M-estimate',
      encoder == 'oe' ~ 'Ordinal',
      encoder == 'pe' ~ 'Quantile',
      encoder == 'te' ~ 'Target',
      encoder == 'js' ~ 'James-Stein',
      encoder == 'stats_e' ~ 'Mixed models',
    ),
    data = case_when(
      data == 'cauchy' ~ 'Cauchy',
      data == 'ks' ~ 'Kickstarter Projects',
      data == 'so2019' ~ 'StackOverflow 2019',
      data == 'stackoverflow' ~ 'StackOverflow 2018',
      data == 'medical_payments_sample' ~ 'Medical Payments'
    )
  ) %>% 
  filter(!is.na(encoder))

levels <- c('Quantile', 'Catboost', 'M-estimate', 'James-Stein', 'Mixed models', 'Target', 'Ordinal')

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
  theme(
    text = element_text(size = 15),
    axis.text.x = element_text(size = 10)
    ) + 
  ylab('') + 
  xlab('') +
  ggsave('results_regression/lm_categorical_compare.png', width = 20, height = 9)
