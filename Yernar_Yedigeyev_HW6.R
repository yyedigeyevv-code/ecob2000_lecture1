#Homework 6 Lab5 write up
#Group: Yernar, Justin

library(ggplot2)
library(tidyverse)
library(haven)
library(viridis)

load("acs2021_recoded.RData")


dim(acs2021)
names(acs2021)
summary(acs2021)


acs_age <- acs2021 %>%
  filter(AGE >= 25, AGE <= 55, !is.na(INCWAGE))


model_age2 <- lm(INCWAGE ~ AGE + I(AGE^2), data = acs_age)

summary(model_age2)

b <- coef(model_age2)

peak_age <- -b["AGE"] / (2 * b["I(AGE^2)"])

peak_wage <- predict(
  model_age2,
  newdata = data.frame(AGE = peak_age)
)

peak_age
peak_wage

model_age3 <- lm(
  INCWAGE ~ AGE + I(AGE^2) + I(AGE^3),
  data = acs_age
)

summary(model_age3)

anova(model_age2, model_age3)

model_age4 <- lm(
  INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4),
  data = acs_age
)

summary(model_age4)

anova(model_age3, model_age4)

model_logage <- lm(
  INCWAGE ~ log(AGE),
  data = acs_age
)

summary(model_logage)

model_logage2 <- lm(
  INCWAGE ~ log(AGE) + I(log(AGE)^2),
  data = acs_age
)

summary(model_logage2)

anova(model_logage, model_logage2)

table(acs2021$educ_college, useNA = "ifany")
table(acs2021$educ_advdeg, useNA = "ifany")
table(acs2021$EDUC, useNA = "ifany")

acs_subgroup <- acs2021 %>%
  filter(
    AGE >= 25, AGE <= 55,
    LABFORCE == 2,
    WKSWORK2 > 4,
    UHRSWORK >= 35,
    Hispanic == 1,
    female == 1,
    !is.na(INCWAGE)
  ) %>%
  mutate(
    educ_hs = as.integer(EDUC == "Grade 12")
  )

table(acs_subgroup$educ_hs)
table(acs_subgroup$educ_college)
table(acs_subgroup$educ_advdeg)

model_educ <- lm(
  INCWAGE ~ educ_hs,
  data = acs_subgroup
)

summary(model_educ)

model_educ2 <- lm(
  INCWAGE ~ educ_hs + educ_college,
  data = acs_subgroup
)

summary(model_educ2)

model_educ3 <- lm(
  INCWAGE ~ educ_hs + educ_college + educ_advdeg,
  data = acs_subgroup
)

summary(model_educ3)

acs_subgroup_log <- acs_subgroup %>%
  filter(INCWAGE > 0)

model_logwage <- lm(
  log(INCWAGE) ~ educ_hs + educ_college + educ_advdeg,
  data = acs_subgroup_log
)

summary(model_logwage)


model_wage_positive <- lm(
  INCWAGE ~ educ_hs + educ_college + educ_advdeg,
  data = acs_subgroup_log
)

education_cases <- data.frame(
  educ_hs = c(0, 1, 0, 0),
  educ_college = c(0, 0, 1, 0),
  educ_advdeg = c(0, 0, 0, 1)
)

education_cases$predicted_wage <- predict(
  model_wage_positive,
  newdata = education_cases
)

education_cases$predicted_log_wage <- predict(
  model_logwage,
  newdata = education_cases
)

education_cases$predicted_wage_from_log <- exp(
  education_cases$predicted_log_wage
)

education_cases


acs_gender <- acs2021 %>%
  filter(
    AGE >= 25, AGE <= 55,
    LABFORCE == 2,
    WKSWORK2 > 4,
    UHRSWORK >= 35,
    Hispanic == 1,
    !is.na(INCWAGE)
  )


model_gender <- lm(
  INCWAGE ~ female * (AGE + I(AGE^2)),
  data = acs_gender
)

summary(model_gender)


b <- coef(model_gender)


peak_age_men <- -b["AGE"] / (2 * b["I(AGE^2)"])


age_coef_women <- b["AGE"] + b["female:AGE"]
age2_coef_women <- b["I(AGE^2)"] + b["female:I(AGE^2)"]

peak_age_women <- -age_coef_women / (2 * age2_coef_women)


peak_wage_men <- predict(
  model_gender,
  newdata = data.frame(
    female = 0,
    AGE = peak_age_men
  )
)

peak_wage_women <- predict(
  model_gender,
  newdata = data.frame(
    female = 1,
    AGE = peak_age_women
  )
)

data.frame(
  Gender = c("Men", "Women"),
  Peak_Age = c(peak_age_men, peak_age_women),
  Predicted_Annual_Wage = c(peak_wage_men, peak_wage_women)
)


model_gender_no_interactions <- lm(
  INCWAGE ~ female + AGE + I(AGE^2),
  data = acs_gender
)

anova(model_gender_no_interactions, model_gender)




# Joint significance test for gender interactions:
# I compared a model without gender interactions to a model
# including interactions between female and both AGE and AGE^2.
# The joint F-test produced F(2, 114643) = 54.091, with
# p < 0.001. Therefore, I reject the null hypothesis that both
# interaction coefficients are zero.
#
# The interaction terms jointly improve the model, providing
# evidence that the relationship between age and annual wages
# differs between men and women in this sample.
#
# The estimated peak predicted wage occurs at approximately
# age 50.6 for men and age 44.0 for women. The predicted annual
# wages at these respective peak ages are approximately $64,725
# for men and $55,060 for women.
#
# However, the model's R-squared is only 0.0185, meaning that
# the model explains about 1.85% of the variation in annual
# wages. These results represent associations, not necessarily
# causal effects.
