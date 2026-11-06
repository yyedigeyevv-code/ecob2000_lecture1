
# Lab 7 - Yernar Yedigeyev

library(ggplot2)
library(tidyverse)
library(haven)

setwd("C:/Users/Yerr1/Documents/ecob2000_lecture1")

load("ACS2021_couples.RData")

dim(acs2021_couples)

acs2021_couples$RACE <- fct_recode(
  as.factor(haven::zap_labels(acs2021_couples$RACE)),
  "White" = "1",
  "Black" = "2",
  "American Indian or Alaska Native" = "3",
  "Chinese" = "4",
  "Japanese" = "5",
  "Other Asian or Pacific Islander" = "6",
  "Other race" = "7",
  "two races" = "8",
  "three races" = "9"
)

table(acs2021_couples$RACE)


acs2021_couples$h_race <- fct_recode(
  as.factor(haven::zap_labels(acs2021_couples$h_race)),
  "White" = "1",
  "Black" = "2",
  "American Indian or Alaska Native" = "3",
  "Chinese" = "4",
  "Japanese" = "5",
  "Other Asian or Pacific Islander" = "6",
  "Other race" = "7",
  "two races" = "8",
  "three races" = "9"
)

acs2021_couples$HISPAN <- fct_recode(
  as.factor(haven::zap_labels(acs2021_couples$HISPAN)),
  "Not Hispanic" = "0",
  "Mexican" = "1",
  "Puerto Rican" = "2",
  "Cuban" = "3",
  "Other" = "4"
)

acs2021_couples$h_hispan <- fct_recode(
  as.factor(haven::zap_labels(acs2021_couples$h_hispan)),
  "Not Hispanic" = "0",
  "Mexican" = "1",
  "Puerto Rican" = "2",
  "Cuban" = "3",
  "Other" = "4"
)

table(acs2021_couples$h_race)
table(acs2021_couples$HISPAN)
table(acs2021_couples$h_hispan)

trad_data <- acs2021_couples %>%
  filter(
    SEX == "Female",
    h_sex == 1
  ) %>%
  mutate(
    age_diff = AGE - h_age,
    he_more_than_7yrs_than_her = as.numeric(age_diff < -7)
  )

dim(trad_data)

table(trad_data$he_more_than_7yrs_than_her)

mean(trad_data$he_more_than_7yrs_than_her, na.rm = TRUE) * 100

table(
  trad_data$he_more_than_7yrs_than_her,
  cut(trad_data$age_diff, c(-100, -7, 0, 7, 100))
)

# The sample contains 412,275 couples.
# In 50,930 couples (12.35%), the husband is more than
# 7 years older than his wife.
# The dependent variable equals 1 for these couples
# and 0 otherwise.

grep("educ", names(trad_data),
     ignore.case = TRUE, value = TRUE)

class(trad_data$EDUC)

table(trad_data$EDUC)

trad_data <- trad_data %>%
  mutate(
    educ_hs = as.numeric(EDUC == "Grade 12"),
    educ_somecoll = as.numeric(
      EDUC %in% c("1 year of college", "2 years of college")
    ),
    educ_college = as.numeric(EDUC == "4 years of college"),
    educ_advdeg = as.numeric(EDUC == "5+ years of college")
  )

ols_out1 <- lm(
  he_more_than_7yrs_than_her ~
    educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE,
  data = trad_data
)

summary(ols_out1)

# Model 1: Linear Probability Model
# All education coefficients are negative and statistically
# significant at the 1% level.
# Compared with the omitted education group, women with
# high school education have a 4.10 percentage-point lower
# predicted probability of having a husband more than 7
# years older. The corresponding differences are 5.42
# percentage points for some college, 8.16 for college,
# and 8.94 for advanced degrees.
#
# The AGE coefficient is -0.001924, indicating that each
# additional year of current age is associated with a
# 0.1924 percentage-point lower predicted probability,
# holding education constant.
#
# R-squared = 0.01289, so the model explains approximately
# 1.29% of the variation in the dependent variable.
# The overall F-test is statistically significant
# (F = 1076, p < 0.001).
# These results show associations, not causal effects.

class(trad_data$h_educ)

table(trad_data$h_educ)

levels(trad_data$EDUC)

education_years <- c(
  "N/A or no schooling" = NA_real_,
  "Nursery school to grade 4" = 2,
  "Grade 5, 6, 7, or 8" = 7,
  "Grade 9" = 9,
  "Grade 10" = 10,
  "Grade 11" = 11,
  "Grade 12" = 12,
  "1 year of college" = 13,
  "2 years of college" = 14,
  "3 years of college" = 15,
  "4 years of college" = 16,
  "5+ years of college" = 17
)

trad_data <- trad_data %>%
  mutate(
    educ_numeric = unname(education_years[as.character(EDUC)]),
    h_educ_numeric = unname(education_years[as.character(h_educ)])
  )

ols_out2 <- lm(
  he_more_than_7yrs_than_her ~
    educ_numeric + h_educ_numeric + AGE,
  data = trad_data
)

summary(ols_out2)

# Model 2: Education of both partners
# The coefficient on the woman's education is -0.008198,
# indicating a 0.82 percentage-point lower predicted
# probability for each additional year of education,
# holding the other variables constant.
#
# The husband's education coefficient is -0.002097,
# corresponding to a 0.21 percentage-point decrease
# for each additional year of education.
#
# The AGE coefficient is -0.001809, indicating a
# 0.18 percentage-point decrease per additional year.
#
# All coefficients are statistically significant at 1%.
# R-squared = 0.01151, meaning that approximately
# 1.15% of the variation is explained by the model.
#
# 10,425 observations were excluded due to missing values.
# The education variables represent approximate years
# of schooling rather than exact completed years.

ols_out3 <- lm(
  he_more_than_7yrs_than_her ~ EDUC + h_educ + AGE,
  data = trad_data
)

summary(ols_out3)

# Model 3: Categorical education variables
# Both partners' education levels are included as factors.
# The reference category is "N/A or no schooling".
#
# Women with 4 years of college have a 7.77 percentage-point
# lower predicted probability of having a husband more than
# 7 years older, compared with the reference category.
#
# Women with 5+ years of college have an 8.89 percentage-point
# lower predicted probability.
#
# The husband's education coefficients also generally
# indicate negative associations with the dependent variable.
# However, some coefficients are not significant at 5%.
#
# The AGE coefficient is -0.001948, corresponding to a
# 0.195 percentage-point decrease per additional year.
#
# R-squared = 0.01420.
# The model explains approximately 1.42% of the variation.
# These results represent associations, not causal effects.

class(trad_data$REGION)

table(trad_data$REGION)

levels(as.factor(trad_data$REGION))

ols_out4 <- lm(
  he_more_than_7yrs_than_her ~
    educ_numeric + h_educ_numeric + AGE + factor(REGION),
  data = trad_data
)

summary(ols_out4)

# Model 4: Regional differences
# New England is the reference region.
# East North Central and West North Central have
# significantly lower predicted probabilities.
# South Atlantic and Pacific have significantly
# higher predicted probabilities.
#
# Other regional coefficients are not statistically
# significant at the 5% level.
#
# R-squared increased from 0.01151 to 0.01292
# after including regional indicators.
# The model still has relatively low explanatory power.

anova(ols_out2, ols_out4)

# Joint F-test for regional differences
# H0: All eight regional coefficients equal zero.
# H1: At least one regional coefficient is nonzero.
#
# F(8, 401838) = 72.045, p < 0.001.
# We reject the null hypothesis at the 5% level.
#
# Regional indicators jointly improve the model.
# This provides evidence that the probability of a husband
# being more than 7 years older differs across regions,
# after controlling for both partners' education and age.
#
# However, the increase in R-squared is relatively small.

region_results <- data.frame(
  Region = c(
    "Middle Atlantic",
    "East North Central",
    "West North Central",
    "South Atlantic",
    "East South Central",
    "West South Central",
    "Mountain",
    "Pacific"
  ),
  Coefficient = coef(ols_out4)[5:12],
  SE = summary(ols_out4)$coefficients[5:12, 2]
)

region_results <- region_results %>%
  mutate(
    Lower = (Coefficient - 1.96 * SE) * 100,
    Upper = (Coefficient + 1.96 * SE) * 100,
    Effect = Coefficient * 100
  )

ggplot(region_results, aes(x = reorder(Region, Effect), y = Effect)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Regional Differences in Husband-Wife Age Gaps",
    subtitle = "Reference region: New England",
    x = "Region",
    y = "Difference in predicted probability (percentage points)"
  ) +
  theme_minimal()

# The graph displays regional coefficients with 95% confidence
# intervals, using New England as the reference category.
# Pacific and South Atlantic have significantly higher
# predicted probabilities, while East North Central and
# West North Central have significantly lower probabilities.
# The remaining regions are not significantly different
# from New England at the 5% level.

age_predictions <- data.frame(
  AGE = 18:85,
  educ_numeric = 12,
  h_educ_numeric = 12
)

age_predictions$predicted_probability <- predict(
  ols_out2,
  newdata = age_predictions
)

ggplot(age_predictions,
       aes(x = AGE, y = predicted_probability * 100)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Predicted Probability of Husband Being Over 7 Years Older",
    subtitle = "Both partners have 12 years of education",
    x = "Woman's Current Age",
    y = "Predicted Probability (%)"
  ) +
  theme_minimal()

# The graph shows a negative relationship between the woman's
# current age and the predicted probability that her husband
# is more than 7 years older.
#
# Holding both partners' education at 12 years, the predicted
# probability decreases as the woman's age increases.
#
# Each additional year of age is associated with a 0.181
# percentage-point decrease in predicted probability.
# This relationship is an association, not a causal effect.

class(trad_data$STATEFIP)

length(unique(trad_data$STATEFIP))

table(trad_data$STATEFIP)

ols_out5 <- lm(
  he_more_than_7yrs_than_her ~
    educ_numeric + h_educ_numeric + AGE + factor(STATEFIP),
  data = trad_data
)

summary(ols_out5)

# Model 5: State-level differences
# Alabama is the reference state.
#
# Hawaii, Nevada, and Florida have significantly higher
# predicted probabilities than Alabama, controlling for
# both partners' education and the woman's current age.
#
# Utah, Iowa, and Minnesota have significantly lower
# predicted probabilities than Alabama.
#
# R-squared = 0.01427, compared with 0.01151 in Model 2
# and 0.01292 in Model 4.
#
# State indicators improve the model's explanatory power,
# although the overall R-squared remains relatively low.
# These associations should not be interpreted as causal.

anova(ols_out2, ols_out5)

# Joint F-test for state indicators
# H0: All 50 state coefficients are equal to zero.
# H1: At least one state coefficient is nonzero.
#
# F(50, 401796) = 22.468, p < 0.001.
# We reject the null hypothesis at the 5% significance level.
#
# State indicators are jointly statistically significant
# after controlling for education and age.
#
# R-squared increases from 0.01151 to 0.01427.
# Although the improvement is statistically significant,
# the model still has relatively low explanatory power.