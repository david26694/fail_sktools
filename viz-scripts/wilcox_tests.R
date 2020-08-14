library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)

theme_set(theme_minimal())

cv_results <- read.csv('results_regression/cv_results_v0.csv')

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

cv_wide <- cv_long %>% 
  pivot_wider(
    names_from = encoder,
    values_from = test_score
  )



p_values <- cv_wide %>% 
  filter(learner != 'lg', data != 'house_kaggle') %>% 
  group_by(data, learner) %>% 
  summarise(
    p_value = wilcox.test(
      te, 
      pe, 
      alternative = 'greater', 
      paired = T, 
      exact = T)$p.value
  ) %>% 
  select(
    Dataset = data,
    `p-value` = p_value
  )

print(xtable(
  p_values, 
  caption = "p-values of Wilcoxon tests in several datasets",
  digits = 4),
  include.rownames = F
  )
