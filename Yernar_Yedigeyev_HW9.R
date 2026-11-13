library(ggplot2)
library(tidyverse)
library(haven)

setwd("C:/Users/Yerr1/Documents/ecob2000_lecture1")

load("ACS2021_couples.RData")

trad_data <- acs2021_couples %>%
  filter(SEX == "Female", h_sex == 1) %>%
  mutate(
    age_diff = AGE - h_age,
    he_more_than_7yrs_than_her = as.numeric(age_diff < -7),
    educ_hs = as.numeric(EDUC == "Grade 12"),
    educ_somecoll = as.numeric(
      EDUC %in% c("1 year of college", "2 years of college")
    ),
    educ_college = as.numeric(EDUC == "4 years of college"),
    educ_advdeg = as.numeric(EDUC == "5+ years of college")
  )

dim(trad_data)

table(trad_data$he_more_than_7yrs_than_her)

ols_out1 <- lm(
  he_more_than_7yrs_than_her ~
    educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE,
  data = trad_data
)

summary(ols_out1)


model_logit1 <- glm(
  he_more_than_7yrs_than_her ~
    educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE,
  data = trad_data,
  family = binomial(link = "logit")
)

summary(model_logit1)


model_probit1 <- glm(
  he_more_than_7yrs_than_her ~
    educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE,
  data = trad_data,
  family = binomial(link = "probit")
)

summary(model_probit1)


actual <- trad_data$he_more_than_7yrs_than_her

cutoff <- mean(actual)

pred_ols <- predict(ols_out1, newdata = trad_data)
pred_logit <- predict(model_logit1, newdata = trad_data, type = "response")
pred_probit <- predict(model_probit1, newdata = trad_data, type = "response")

evaluate_model <- function(predictions, actual, cutoff) {
  predicted <- as.integer(predictions > cutoff)
  
  TP <- sum(predicted == 1 & actual == 1)
  TN <- sum(predicted == 0 & actual == 0)
  FP <- sum(predicted == 1 & actual == 0)
  FN <- sum(predicted == 0 & actual == 1)
  
  tibble(
    Accuracy = (TP + TN) / length(actual),
    Precision = ifelse(TP + FP > 0, TP / (TP + FP), NA_real_),
    Recall = ifelse(TP + FN > 0, TP / (TP + FN), NA_real_),
    False_Positive = FP,
    False_Negative = FN
  )
}

comparison <- bind_rows(
  OLS = evaluate_model(pred_ols, actual, cutoff),
  Logit = evaluate_model(pred_logit, actual, cutoff),
  Probit = evaluate_model(pred_probit, actual, cutoff),
  .id = "Model"
)

comparison %>%
  mutate(across(c(Accuracy, Precision, Recall), ~ round(.x * 100, 2)))


cutoffs <- c(0.10, 0.20, 0.50)

cutoff_comparison <- bind_rows(
  lapply(cutoffs, function(c) {
    evaluate_model(pred_logit, actual, c) %>%
      mutate(Cutoff = c, Model = "Logit")
  })
)

cutoff_comparison %>%
  select(Model, Cutoff, Accuracy, Precision, Recall,
         False_Positive, False_Negative) %>%
  mutate(across(c(Accuracy, Precision, Recall),
                ~ round(.x * 100, 2)))


cutoff_graph <- map_dfr(seq(0.05, 0.30, by = 0.01), function(c) {
  evaluate_model(pred_logit, actual, c) %>%
    mutate(Cutoff = c)
})

cutoff_graph_long <- cutoff_graph %>%
  select(Cutoff, False_Positive, False_Negative) %>%
  pivot_longer(
    cols = c(False_Positive, False_Negative),
    names_to = "Error_Type",
    values_to = "Count"
  )

ggplot(cutoff_graph_long,
       aes(x = Cutoff, y = Count, color = Error_Type)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Logit Model: False Positives vs. False Negatives",
    subtitle = "Husband more than 7 years older",
    x = "Prediction Cutoff",
    y = "Number of Incorrect Predictions",
    color = "Error Type"
  ) +
  theme_minimal()

names(trad_data)

table(trad_data$REGION, useNA = "ifany")
table(trad_data$RACE, useNA = "ifany")

trad_data <- trad_data %>%
  mutate(
    REGION = as.factor(REGION),
    RACE = as.factor(RACE)
  )

model_logit2 <- glm(
  he_more_than_7yrs_than_her ~
    educ_hs + educ_somecoll + educ_college +
    educ_advdeg + AGE + REGION + RACE,
  data = trad_data,
  family = binomial(link = "logit")
)

summary(model_logit2)

AIC(model_logit1, model_logit2)

anova(model_logit1, model_logit2, test = "Chisq")


pred_logit2 <- predict(
  model_logit2,
  newdata = trad_data,
  type = "response"
)

expanded_comparison <- bind_rows(
  "Original Logit" = evaluate_model(pred_logit, actual, cutoff),
  "Expanded Logit" = evaluate_model(pred_logit2, actual, cutoff),
  .id = "Model"
)

expanded_comparison %>%
  mutate(
    across(c(Accuracy, Precision, Recall),
           ~ round(.x * 100, 2))
  )


final_comparison <- bind_rows(
  comparison,
  expanded_comparison %>%
    filter(Model == "Expanded Logit")
)

final_comparison_long <- final_comparison %>%
  select(Model, Accuracy, Precision, Recall) %>%
  pivot_longer(
    cols = c(Accuracy, Precision, Recall),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(final_comparison_long,
       aes(x = Model, y = Value * 100, fill = Model)) +
  geom_col(width = 0.65) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Comparison of OLS, Logit, and Probit Models",
    subtitle = "Prediction cutoff: 12.35%",
    x = "Model",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


#In this lab, I compared OLS, Logit, and Probit models to predict
# whether a husband is more than 7 years older than his wife,
# using 412,275 observations from the ACS 2021 couples dataset.
#
# All three models showed negative and statistically significant
# relationships between education, age, and the probability of
# having a husband more than 7 years older.
#
# Using a prediction cutoff of 12.35%, OLS achieved 55.1% accuracy,
# Logit achieved 58.6%, and Probit achieved 57.9%. OLS had the
# highest recall (59.1%), but also produced more false positives.
#
# I also tested different prediction cutoffs for the Logit model.
# Lower cutoffs increased recall but produced more false positives,
# while higher cutoffs reduced false positives but increased
# false negatives. At a cutoff of 0.50, the model achieved 87.6%
# accuracy but failed to identify any positive cases.
#
# Next, I expanded the Logit model by adding region and race.
# The AIC decreased from 303071.2 to 301264.4, and the likelihood-
# ratio test showed a statistically significant improvement
# (p-value < 0.001). Prediction accuracy increased from 58.6%
# to 59.6%.
#
# Overall, the expanded Logit model performed slightly better
# in terms of accuracy and precision, while OLS had the highest
# recall. However, all models had relatively low precision.
# These results show that statistical significance does not
# necessarily mean strong predictive performance.
#
# Finally, these relationships should not be interpreted as
# causal effects because other unobserved factors may influence
# marriage patterns. The predictions were also evaluated on
# the same data used to estimate the models, so performance
# on new observations may be different.


subgroup_results <- trad_data %>%
  mutate(
    predicted = as.integer(pred_logit2 > cutoff),
    education_group = case_when(
      educ_advdeg == 1 ~ "Advanced Degree",
      educ_college == 1 ~ "College",
      educ_somecoll == 1 ~ "Some College",
      educ_hs == 1 ~ "High School",
      TRUE ~ "Less than High School"
    )
  ) %>%
  group_by(education_group) %>%
  summarise(
    N = n(),
    Actual_Rate = mean(he_more_than_7yrs_than_her) * 100,
    Accuracy = mean(predicted == he_more_than_7yrs_than_her) * 100,
    False_Positive = sum(predicted == 1 & he_more_than_7yrs_than_her == 0),
    False_Negative = sum(predicted == 0 & he_more_than_7yrs_than_her == 1),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(Actual_Rate, Accuracy), ~ round(.x, 2))
  )

print(subgroup_results)



# I also examined the expanded Logit model's performance across
# different education groups.
#
# The model achieved the highest accuracy among women with
# advanced degrees (76.3%), followed by college graduates (68.5%).
# Accuracy was lower for women with high school education (54.8%)
# and some college education (54.5%).
#
# The lowest accuracy was among women with less than a high school
# education (30.1%). This group also had the highest actual rate
# of husbands being more than 7 years older (18.0%), compared
# with only 9.64% among women with advanced degrees.
#
# These findings suggest that the model's predictive performance
# varies across education groups. However, differences in group
# sizes and outcome frequencies should also be considered.

