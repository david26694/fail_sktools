library(dplyr)
library(ggplot2)
library(tidyr)

theme_set(theme_minimal())

cv_results <- read.csv('results_regression/results_MAEvsMASE.csv')

relative_errors <- cv_results %>% 
  select(
    te_mse = enet_te_test_mse, 
    te_mae = enet_te_test_mae, 
    pe_mse = enet_pe_test_mse, 
    pe_mae = enet_pe_test_mae, 
    data = NameDataset
    ) %>% 
  mutate(
    data = stringr::str_remove_all(data, 'data/'),
    data = stringr::str_remove_all(data, '.csv')
  ) %>% 
  mutate(
    relative_error_mae = 100 * (te_mae - pe_mae) / te_mae,
    relative_error_mse = 100 * (te_mse - pe_mse) / te_mse
  ) %>% 
  pivot_longer(
    c(relative_error_mae, relative_error_mse),
    names_to = c("metric"),
    names_pattern = "relative_error_(.*)",
    values_to = "relative_error"
  ) %>% 
  mutate(
    improvement_qe = if_else(relative_error > 0, 'QE beats TE', 'TE beats QE'),
    metric = stringr::str_to_upper(metric)
    ) %>% 
  filter(data != 'house_kaggle') %>% 
  mutate(
    data = case_when(
      data == 'cauchy' ~ 'Cauchy',
      data == 'ks' ~ 'Kickstarter Projects',
      data == 'so2019' ~ 'StackOverflow 2019',
      data == 'stackoverflow' ~ 'StackOverflow 2018',
      data == 'medical_payments_sample' ~ 'Medical Payments'
    )
  )

  
ordered_data <- relative_errors %>% 
  filter(metric == 'MAE') %>% 
  arrange(desc(relative_error)) %>% 
  pull(data)


relative_errors$data <- factor(relative_errors$data, levels = ordered_data)

relative_errors %>% 
  ggplot(aes(x = data, y = relative_error, fill = improvement_qe)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~metric, scales = 'free_y', ncol = 1) +
  xlab('Dataset') + 
  ylab('Relative error difference (%)') +
  labs(fill = '') + 
  xlab("") + 
  ylab("") + 
  theme(
    text = element_text(size = 15),
    axis.text.x = element_text(size = 10)
  ) + 
  # ggtitle('Relative error difference between QE and TE',
  #         'Difference for several datasets in test MAE and MSE') +
  ggsave("results_regression/mae_mse_differences.png", width = 10, height = 8)


