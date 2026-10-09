# Homework #5 — Lab 4
# Yernar Yedigeyev
# Study group: Yernar Yedigeyev, Piter Miedema

library(tidyverse)

load("d_HHP2020_24.Rdata")


# Select prime-age individuals who report a work status
prime_age_laborforce_data <- d_HHP2020_24 %>%
  filter(!is.na(work_kind),
         Age >= 25,
         Age <= 55)

# Inspect the sample and income variable
dim(prime_age_laborforce_data)

summary(prime_age_laborforce_data$income_midpoint)

table(prime_age_laborforce_data$work_kind, useNA = "always")


# First regression: income in levels
model_1 <- lm(
  income_midpoint ~ Age + Gender + Education + Race + Hispanic,
  data = prime_age_laborforce_data
)

summary(model_1)



# 95% confidence intervals
confint(model_1, level = 0.95)



# Install once if needed:
# install.packages("lmtest")
# install.packages("sandwich")

library(lmtest)
library(sandwich)

# Regression results with HC1 robust standard errors
coeftest(model_1, vcov. = vcovHC(model_1, type = "HC1"))



# Keep observations with positive, non-missing income
prime_age_positive_income <- prime_age_laborforce_data %>%
  filter(!is.na(income_midpoint), income_midpoint > 0)

# Second regression: log income
model_2 <- lm(
  log(income_midpoint) ~ Age + Gender + Education + Race + Hispanic,
  data = prime_age_positive_income
)

summary(model_2)



# Mean predicted income from the level model
mean(predict(model_1), na.rm = TRUE)

# Mean exponentiated predicted log income
mean(exp(predict(model_2)), na.rm = TRUE)



# Create profiles for ages 25 through 55
to_be_predicted1 <- data.frame(
  Age = 25:55,
  Gender = "female",
  Education = "adv degree",
  Race = "Black",
  Hispanic = "Hispanic"
)

# Predict income using the level model
to_be_predicted1$predicted_income_level <- predict(
  model_1,
  newdata = to_be_predicted1
)

# Predict log income and transform to dollars
to_be_predicted1$predicted_income_log <- exp(
  predict(model_2, newdata = to_be_predicted1)
)

# Display selected ages
to_be_predicted1[to_be_predicted1$Age %in% c(25, 30, 35, 40, 45, 50, 55), ]




# Restrict the sample to college graduates and people with advanced degrees
college_graduate_data <- prime_age_laborforce_data %>%
  filter(Education %in% c("college grad", "adv degree"))

# Regression for college graduates
model_3 <- lm(
  income_midpoint ~ Age + Gender + Education + Race + Hispanic,
  data = college_graduate_data
)

summary(model_3)



# Restrict the sample to females
female_data <- prime_age_laborforce_data %>%
  filter(Gender == "female")

# Regression for females only
model_4 <- lm(
  income_midpoint ~ Age + Education + Race + Hispanic,
  data = female_data
)

summary(model_4)




library(car)

# Joint test: all slope coefficients in Model 1 equal zero
linearHypothesis(
  model_1,
  c(
    "Age = 0",
    "Genderfemale = 0",
    "Gendertrans = 0",
    "Genderother = 0",
    "Educationsome hs = 0",
    "Educationhigh school = 0",
    "Educationsome college = 0",
    "Educationassoc deg = 0",
    "Educationcollege grad = 0",
    "Educationadv degree = 0",
    "RaceBlack = 0",
    "RaceAsian = 0",
    "Raceother = 0",
    "HispanicHispanic = 0"
  )
)


# Joint hypothesis test interpretation:
# The F-statistic is 6970.2, with 14 and 328,817 degrees of freedom.
# The p-value is below 2.2e-16, so I reject the null hypothesis
# that all 14 slope coefficients are jointly equal to zero.
# Therefore, the explanatory variables are jointly statistically
# significant in explaining income variation in this sample.
# This does not imply that every individual coefficient is significant.


# Compare age and education coefficients across models
coef(model_1)
coef(model_4)


# Comparison of Model 1 and Model 4:
# Model 1 uses the full sample, while Model 4 includes only females.
# The estimated age coefficient decreases from about $1,238 to $1,050,
# suggesting a smaller conditional association between age and income
# in the female-only sample.
# The estimated income differences associated with higher education
# are generally larger in Model 4. For example, the college-graduate
# coefficient increases from about $67,093 to $71,055.
# The R-squared decreases from 0.2289 to 0.2161.
# These differences may reflect changes in sample composition and
# relationships among the explanatory variables. They should be
# interpreted as conditional associations, not causal effects.


# Conceptual question: Predicting a subsequent grade from a previous grade
# A reasonable starting model is:
# Subsequent Grade = alpha + beta * Previous Grade + error.
# I would expect beta to be positive because students who earn higher
# grades previously will generally tend to earn higher grades later.
# A linear model is a useful first approximation, although the
# relationship could be nonlinear, particularly near the maximum
# possible grade. The intercept represents the predicted subsequent
# grade when the previous grade is zero, but this may not be meaningful
# if few students have grades near zero. The exact coefficient values
# depend on the data and cannot be determined without estimating the model.