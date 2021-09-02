###############################################
# Course: IST 687
# Assignment: HW 8
# Name: Tim Cordero
# Date: 08/27/2021
# Notes: Rev 1.0
###############################################
# Libraries
#install.packages("readxl")
#library(readxl)


###############################################
# Step 1

# read in dataset
fawns <- as.data.frame(read_excel("C:/Users/timco/OneDrive - Syracuse University/Semester1/ist_687/Homeworks/Homework8/mlr01.xls"))

# renaming columns
cnames <- c("fawn","adult","precipitation","winter")
colnames(fawns) <- cnames
View(fawns)

# Step 2

# I downloaded file to computer and then read into R

# Step 3

# 4 variables and 8 observations
str(fawns)

# Step 4

# number of fawns is dependent variable (Y-axis)
# DEPEDENT VAR GOES ON Y-AXIS

# plot 1: fawns versus adult antelope pop
plot(fawns$adult ,fawns$fawn, xlab="Adult Antelopes", ylab="Fawns")
#  may be correlation, fairly linear. As # of fawns increases, adult
#     antelope pop increases

# plot 2: fawns versus precipitation
plot(fawns$precipitation ,fawns$fawn, xlab="Precipitation", ylab="Fawns")
#  may be correlation, fairly linear. As # of fawns increases, so does precipitation


# plot 3: fawns versus severity of winter
plot(fawns$winter ,fawns$fawn, xlab="Winter Severity", ylab="Fawns")
#  possible correlation, follows general pattern but not severely linear.
#   as # of fawns goes up, winter severity goes down


# Step 5

# model: predict fawns by severity of winter
m1 <- lm(formula=fawn ~ winter, data=fawns)
summary(m1)
# Adjusted R-Squared: 47.02%
# P-value: 0.03626 (Reject Null Hypothesis)


# model: predict fawns by severity of winter AND adult antelope population
m2 <- lm(formula=fawn ~ winter+adult, data=fawns)
summary(m2)
# Adjusted R-Squared: 84.39% 
# P-value: 0.004152 (Reject Null Hypothesis)


# model: predict fawns by winter,adult,&precipitation
m3 <- lm(formula=fawn ~ winter+adult+precipitation, data=fawns)
summary(m3)
# Adjusted R-Squared: 95.5% 
# P-value: 0.001229 (Reject Null Hypothesis)

# Which model works the best?
# - The last model when we are predicting fawns versus 
#   the adult antelope population, precipitation, and severity of the winter
#   worked the best by a significant amount. The first with the independent
#   variable only being severity of winter did poor compared to others at
#   47% accuracy. The second model with independent variables of severity of winter
#   and adult antelope population did much better at 84% accuracy.
#   The third model with all three independent variables did the best at 95%
#   accuracy in  explaining fawn variation.


# Which of the predictors are statistically significantly in each model?
# - In the first model, the severity of the winter was fairly significant,
#   as it was enough to reject the null hypothesis with a p-value of 0.036.

# - In the second model, only the adult antelope population attribute was
#   statistically significant with a p-value of 0.01. The severity of the winter
#   was not significant as it had a p-value of 0.59.

# - In the third model, all three independent variables were statistically
#   significant with all containing a p-value less then 0.05. The most 
#   significant predictor was precipitation in this model. 


# What would the most parsimonious model contain?
# testing different models to get the most parsimonious model
m <- lm(formula=fawn ~ precipitation, data=fawns)
summary(m)
m <- lm(formula=fawn ~ adult, data=fawns)
summary(m)
m <- lm(formula=fawn ~ winter, data=fawns)
summary(m)
m <- lm(formula=fawn ~ adult+precipitation, data=fawns)
summary(m)

# - I know a parsimonious model is supposed to be the model with "the most bang
#   for your buck". Which means the model with best accuracy and fewest 
#   predictors. Therefore, I would argue the the most parsimonious model would
#   contain only the adult antelope population attribute as the predictor.
#   With only this attribute, we can achieve an 86% accuracy rate. With all
#   3 independent variables, we got to 95% accuracy, however, having 3 
#   independent variables is not worth it to me to only get a 9% increase in accuracy. 