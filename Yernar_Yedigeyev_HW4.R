# Homework #4
# Yernar Yedigeyev
# Study group: Yernar Yedigeyev

library(tidyverse)

load("d_HHP2020_24.Rdata")

# Create partnered variable
d_HHP2020_24$partnered <- (d_HHP2020_24$Mar_Stat == "Married") |
  (d_HHP2020_24$Mar_Stat == "widowed") |
  (d_HHP2020_24$Mar_Stat == "divorced") |
  (d_HHP2020_24$Mar_Stat == "separated")
ls()

# Select respondents ages 30 to 34
d_HHP_Age30_34 <- d_HHP2020_24 %>%
  filter((Age >= 30) & (Age < 35) & !is.na(partnered))

# Calculate partnering rates by state
frac_MS_byState <- d_HHP_Age30_34 %>%
  group_by(State, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

frac_MS_byState

names(d_HHP2020_24)

nrow(d_HHP_Age30_34)

# Calculate partnering rate for ages 30 to 34
partner_rate_30_34 <- d_HHP_Age30_34 %>%
  summarize(
    total = n(),
    partnered = sum(partnered),
    partnered_rate = partnered / total
  )

partner_rate_30_34

# Calculate partnering rates by age
partner_rate_sum <- d_HHP2020_24 %>%
  filter(Age < 88) %>%
  group_by(Age) %>%
  summarize(
    partnered_rate = sum(partnered, na.rm = TRUE) /
      sum(!is.na(partnered)),
    number_obs = n()
  )

partner_rate_sum

# Graph partnering rate by age
ggplot(partner_rate_sum, aes(x = Age, y = partnered_rate)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Partnering Rate by Age",
    x = "Age",
    y = "Partnered Rate"
  )

partner_rate_30_34_gender <- d_HHP_Age30_34 %>%
  group_by(Gender) %>%
  summarize(
    total = n(),
    partnered = sum(partnered),
    partnered_rate = partnered / total
  )

partner_rate_30_34_gender

partner_rate_30_34_education <- d_HHP_Age30_34 %>%
  group_by(Education) %>%
  summarize(
    total = n(),
    partnered = sum(partnered),
    partnered_rate = partnered / total
  )

partner_rate_30_34_education

partner_rate_30_34_race <- d_HHP_Age30_34 %>%
  group_by(Race) %>%
  summarize(
    total = n(),
    partnered = sum(partnered),
    partnered_rate = partnered / total
  )

partner_rate_30_34_race

partner_rate_30_34_hispanic <- d_HHP_Age30_34 %>%
  group_by(Hispanic) %>%
  summarize(
    total = n(),
    partnered = sum(partnered),
    partnered_rate = partnered / total
  )

partner_rate_30_34_hispanic

# Question 2: Lab 3 results

# Lab 3 examined how partnering varies with age and demographic characteristics.
# For respondents ages 30 to 34, 58.8% were partnered.

# The age analysis showed that the partnering rate generally increases with age.
# The increase is especially noticeable through the 20s and early 30s, while
# the rate becomes flatter around the mid-30s.

# Among respondents ages 30 to 34, the partnering rate was 56.3% for males
# and 60.8% for females. The rates for the trans and other categories were
# lower, although these groups had much smaller sample sizes.

# Education did not show a simple increasing relationship with partnering.
# The partnering rates ranged from 52.4% to 61.7% across education groups.

# Race showed larger differences. The partnering rate was 61.8% for White
# respondents, 37.2% for Black respondents, 57.2% for Asian respondents,
# and 53.2% for respondents in the other category.

# Hispanic status showed a relatively small difference: 59.0% for respondents
# who were not Hispanic and 56.9% for Hispanic respondents.

# These results describe associations in the data and do not establish
# that any of these characteristics cause people to become partnered.