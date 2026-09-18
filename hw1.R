# ECO B2000 - Homework 1
# Study Group: Yernar, Alex, Sarah


# Load Household Pulse Data
load("d_HHP2020_24.RData")


# Look at the first 10 observations and 6 variables
d_HHP2020_24[1:10, 1:6]


# Average age of females
mean(d_HHP2020_24$Age[d_HHP2020_24$Gender == "female"])


# Average age of males
mean(d_HHP2020_24$Age[d_HHP2020_24$Gender == "male"])


# Difference in average age
mean(d_HHP2020_24$Age[d_HHP2020_24$Gender == "male"]) -
  mean(d_HHP2020_24$Age[d_HHP2020_24$Gender == "female"])


# Average household size
mean(d_HHP2020_24$Number_people_HH)


# Median household size
median(d_HHP2020_24$Number_people_HH)


# Interesting observation:
# The average household has about 2.72 people,
# while the median household has 2 people.
# This suggests that some larger households increase the average.


# Interesting questions about the Household Pulse Data

# 1. Is there a statistically significant difference in average age
# between males and females?

# 2. Does household income differ depending on education level?

# 3. Does the number of children in a household vary with household income?

# 4. Is employment status related to education level?

# 5. Does the reported level of anxiety differ between people
# with different levels of education?


# Dice experiment
# 20 simulated dice rolls
dice_rolls <- c(3, 6, 2, 5, 1, 4, 6, 3, 2, 6,
                5, 1, 4, 2, 6, 3, 5, 4, 1, 2)


# Number of times a 6 was rolled
sum(dice_rolls == 6)


# Proportion of rolls that were 6
mean(dice_rolls == 6)


# 4 out of 20 rolls were 6, or 20%.