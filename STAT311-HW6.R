# STAT311 - Homework 6
# Name: 

# # # # # # # DO NOT EDIT # # # # # # #
vehicleTheft=c(1031, 951, 931, 911, 917, 1004, 951)
gambler=c(15, 30, 36, 50, 57, 59, 55, 48, 36, 20, 17)
# # # # # # # DO NOT EDIT # # # # # # #

# Question 1
# We wish to test the hypothesis that the rate of vehicle thefts is constant 
#  throughout the week. This data can be found in the variable vehicleTheft.

# Question 1.a
# Calculate the expected counts for the number of theft of vehicle crimes for 
# each day of the week. Your answer should be in the form of a list of length 7.
# Save your answer in the variable q1.a
q1.a <- rep(mean(vehicleTheft), 7)
q1.a

# Question 1.b
# Calculate the chi2 test statistic for testing the hypothesis described. 
# Save your answer in the variable q1.b
q1.b <- sum((vehicleTheft - q1.a)^2 / q1.a)
q1.b

# Question 1.c
# Calculate the p-value for the hypothesis. 
# Save your answer in the variable q1.c
q1.c <- pchisq(q1.b, df=6, lower.tail=FALSE)
q1.c

# TRUE or FALSE, testing at a 5% level, we can reject the null 
# hypothesis and conclude that vehicle thefts are more 
# common on Monday and Saturday. 
# (Answer Hidden, consider carefully)
# Save your answer in the variable q1.d
reject_null <- q1.c < 0.05

# Second, are thefts actually more common on Monday and Saturday?
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
higher_days <- days[vehicleTheft > q1.a]
correct_days <- all(c("Monday", "Saturday") %in% higher_days) && length(higher_days) == 2

# For q1.d to be TRUE, we need to both reject the null AND conclude Monday/Saturday have more thefts
q1.d <- reject_null && correct_days
q1.d

# Question 2
# A nervous gambler finds that they keep losing money at the craps table, 
# and suspect the house of cheating. They record a large number of rolls 
# (over which they continue to lose money...) and find the following data.

# Note that in craps, each roll is a roll of two dice, summed together, taking 
# values from 2 to 12. This data can be found in the variable gambler.

# Question 2.a
# To find evidence that the dice are unfair, what will be the assumed 
# distribution of each possible value? Your answer should be a list of 
# length 11, with the first value representing P(X=2), the second P(X=3), etc. 
# Save your answer in the variable q2.a
q2.a <- c(1,2,3,4,5,6,5,4,3,2,1) / 36
q2.a
  
# Question 2.b
# Calculate the expected counts for the number of rolls of each die. 
# Your answer should be in the form of a list of length 11. 
# Save your answer in the variable q2.b
total_rolls <- sum(gambler)
q2.b <- q2.a * total_rolls

# Question 2.c
# Calculate the chi2 test statistic for testing the hypothesis that the dice are unfair. 
# Save your answer in the variable q2.c
q2.c <- sum((gambler - q2.b)^2 / q2.b)
q2.c

# Question 2.d
# Calculate the p-value for the hypothesis. 
# Save your answer in the variable q2.d
q2.d <- pchisq(q2.c, df=10, lower.tail=FALSE)
  
# Question 2.e
# TRUE or FALSE, testing at a 5% level, the gambler cannot reject 
# the hypothesis that the dice are fair. 
# (Answer Hidden, consider carefully)
# Save your answer in the variable q2.e
q2.e <- q2.d >= 0.05
q2.e
  
# Question 2.f
# TRUE or FALSE, testing at a 5% level, the gambler can reject 
# the alternative hypothesis that the dice are not fair. 
# (Answer Hidden, consider carefully)
# Save your answer in the variable q2.f
q2.f <- !q2.e 
q2.f


# Question 3
# A survey of voters in "Frontline" counties, counties that represent highly 
# contested regions for congressional races, contacted a total of 1000 voters. 
# Poll respondents were split in half, and each half was asked different questions. 
# 322 of 500 respondents agreed with "Making community college tuition-free for all". 
# 341 of 500 respondents agree with "Making community college tuition-free for
# families with incomes below $125,000 a year". 
# We wish to test the hypothesis that the percentage of voters in frontline 
# districts supporting free community college for lower income families is 
# higher than the percentage supporting free community college for all families. 
# Specifically, we test against the alternative p{lower income}-p{all}>0.
# 
# Note that these questions should be solved using a normal approximation, 
# not the exact or continuity corrected solutions provided by prop.test.

# Question 3.a
# If we don't assume the two proportions are the same, what is our best 
# estimate of the standard error of the difference between these two proportions? 
# Save your answer in the variable q3.a
p1 <- 322/500
p2 <- 341/500
q3.a <- sqrt(p1*(1-p1)/500 + p2*(1-p2)/500)
q3.a
  
# Question 3.b
# Using the estimate of the standard error found above, calculate a 95% 
# confidence interval for the difference between the percentage supporting 
# free community college for lower income families and the percentage supporting 
# free community college for all families. (Specifically calculate a CI 
# for p{lower income}-p{all}). 
# Save your answer in the variable q3.b
diff <- p2 - p1
q3.b <- c(diff - 1.96*q3.a, diff + 1.96*q3.a)
q3.b

# Question 3.c
# Under the assumption needed for our hypothesis test, we can calculate a 
# more accurate estimate of the standard error of the difference between 
# the two proportions; calculate that value. 
# Save your answer in the variable q3.c
p_pool <- (322 + 341) / 1000
q3.c <- sqrt(p_pool * (1 - p_pool) * (1/500 + 1/500))
q3.c

# Question 3.d
# Calculate the test statistic Z for this hypothesis test. 
# Save your answer in the variable q3.d
q3.d <- (p2 - p1) / q3.c
q3.d

# Question 3.e
# Calculate the p-value for this hypothesis test. 
# Save your answer in the variable q3.e
q3.e <- pnorm(q3.d, lower.tail=FALSE)
q3.e

# Question 3.f
# TRUE or FALSE, testing at a 5% level, we can reject the alternative that a 
# higher proportion of voters support free community college for lower income 
# families than support free community college for all families.
# (Answer Hidden, consider carefully) 
# Save your answer in the variable q3.f
q3.f <- TRUE
q3.f
  



