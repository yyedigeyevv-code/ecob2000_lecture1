# Homework #3
# Yernar Yedigeyev
# Study group: Yernar Yedigeyev

library(tidyverse)

# Create partnered variable
d_HHP2020_24$partnered <- (d_HHP2020_24$Mar_Stat == "Married") | 
  (d_HHP2020_24$Mar_Stat == "widowed") | 
  (d_HHP2020_24$Mar_Stat == "divorced") |
  (d_HHP2020_24$Mar_Stat == "separated")

# Check partnered variable
xtabs(~ Mar_Stat + partnered, data = d_HHP2020_24)

# Create age groups for people under 45
HHP_under45 <- d_HHP2020_24 %>%
  filter(Age < 45)

HHP_under45$Age_groups <- cut(
  HHP_under45$Age,
  breaks = c(-Inf, 24.5, 29.5, 34.5, 39.5, Inf),
  labels = c("under 25", "25 to 29", "30 to 34", "35 to 39", "40 to 44")
)

# Calculate partnering rates by age group
partnered_by_age <- HHP_under45 %>%
  group_by(Age_groups) %>%
  summarize(
    n = sum(!is.na(partnered)),
    partnered = sum(partnered, na.rm = TRUE),
    partnered_rate = partnered / n * 100
  )

partnered_by_age


# Graph partnering rate by age group
ggplot(partnered_by_age, aes(x = Age_groups, y = partnered_rate)) +
  geom_col() +
  labs(
    title = "Partnering Rate by Age Group",
    x = "Age Group",
    y = "Partnered (%)"
  )

# Question 2: Lab 2 results

# Lab 2 showed a clear relationship between age and partnering status.
# Among respondents under 25, 13.6% were partnered. This increased to
# 36.5% for ages 25 to 29, 58.8% for ages 30 to 34, 73.4% for ages
# 35 to 39, and 80.1% for ages 40 to 44.
#
# These results suggest that partnering is much more common among older
# respondents in the under-45 sample. I learned that it is important to
# compare groups using both percentages and graphs, rather than looking
# only at the overall data. The results also show a strong association
# between age and partnering, but this does not mean that age by itself
# causes people to become partnered. Other factors, such as education,
# income, and other characteristics, could also be related to partnering.