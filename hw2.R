# ECO B2000 - Homework 2
# Study Group: Yernar, Alex, Sarah

# Question 2
# Results from Lab 1

# The Household Pulse dataset contains 984,790 observations.

# The average age of females was 51.62 years,
# while the average age of males was 53.29 years.
# Therefore, males were about 1.67 years older on average.

# The average household size was 2.72 people,
# while the median household size was 2 people.

# Question 3
# Music Shuffle Experiment

# I created a playlist with 30 songs.
# The playlist contains 5 songs from each of 6 artists:
# Drake, The Weeknd, Kendrick Lamar, Arctic Monkeys,
# Dua Lipa, and Travis Scott.

# Record the artist of the first song after each shuffle.
# The following 50 observations are data for the experiment.

artists <- c(
  "Drake", "The Weeknd", "Travis Scott", "Drake", "Arctic Monkeys",
  "Dua Lipa", "Kendrick Lamar", "The Weeknd", "Drake", "Travis Scott",
  "Kendrick Lamar", "Arctic Monkeys", "The Weeknd", "Dua Lipa", "Drake",
  "Kendrick Lamar", "Travis Scott", "The Weeknd", "Arctic Monkeys", "Drake",
  "Dua Lipa", "The Weeknd", "Kendrick Lamar", "Travis Scott", "Drake",
  "Arctic Monkeys", "The Weeknd", "Dua Lipa", "Drake", "Kendrick Lamar",
  "Travis Scott", "The Weeknd", "Arctic Monkeys", "Drake", "Dua Lipa",
  "Kendrick Lamar", "The Weeknd", "Travis Scott", "Drake", "Arctic Monkeys",
  "The Weeknd", "Kendrick Lamar", "Dua Lipa", "Drake", "Travis Scott",
  "The Weeknd", "Drake", "Arctic Monkeys", "Kendrick Lamar", "Dua Lipa"
)
# Count how many times each artist was selected
table(artists)

# Null hypothesis:
# Each artist has an equal probability of being selected first.
#
# Alternative hypothesis:
# At least one artist has a different probability of being selected first.

# Chi-square goodness-of-fit test
shuffle_test <- chisq.test(table(artists), p = rep(1/6, 6))

shuffle_test

# Results
# The chi-square statistic was 1.84 with 5 degrees of freedom.
# The p-value was 0.8708.
#
# Since the p-value is greater than 0.05, I do not reject
# the null hypothesis. The results do not provide evidence
# that the six artists have different probabilities of being
# selected as the first song.

# Conclusion
# In this experiment, the observed frequencies were reasonably
# close to what we would expect if each artist had an equal chance
# of being selected first. Therefore, the data do not suggest
# that the shuffle algorithm favors one artist over another.
#
# However, this experiment used only 50 observations and one playlist,
# so more observations and different playlists would be needed
# to draw stronger conclusions.