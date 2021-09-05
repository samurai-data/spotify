## Import libraries
library(scorecard)
library(ggplot2)
library(ggplotify)
library(plotly)
library(dplyr)

df <- read.csv('credit_risk_dataset.csv', sep = ';', header = TRUE)
attach(df)



## Calculate bin breaks for numeric variables with respect to their relationships with the outcome variable Churn
bins = woebin(df[, c('person_age', 'person_income', 'person_emp_length', 'loan_amnt', 'loan_percent_income','person_home_ownership', 'loan_intent','loan_int_rate', 'loan_percent_income', 'person_prev_default', 'loan_status')], y = 'loan_status', positive = '1')

## Visualize bins
woebin_plot(bins$person_age)$person_age
woebin_plot(bins$loan_percent_income)$loan_percent_income


# Creates a data frame of binned variables for Logistic Regression: 
df_woe <- woebin_ply(df, bins)

# Logistic Regression:
my_logistic <- glm(loan_status ~ ., family = binomial, data = df_woe)
summary(my_logistic)



# Calculate scorecard scores for variables based on the results from woebin and glm: 
my_card <- scorecard(bins, my_logistic, points0 = 600, odds0 = 1/19, pdo = 50)

# Show Results: 

library(stringr)
do.call("bind_rows", my_card) %>% 
  slice(-1) %>% 
  select(-breaks, -is_special_values,-neg, -pos, -posprob, -count, -count_distr) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  mutate(bin = bin %>% 
           str_replace_all("\\[", "From ") %>% 
           str_replace_all("\\,", " to ") %>% 
           str_replace_all("\\)", "")) -> iv_for_predictors_point

iv_for_predictors_point %>% 
  knitr::kable(col.names = c("Predictor", "Group", "WOE", "Scorecard", "Bin IV", "Total IV"))
