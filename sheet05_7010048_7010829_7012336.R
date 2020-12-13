### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, December 14. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.


## Please write below your (and your teammates') name and matriculation number. 
## Name: Divesh Kumar
## Matriculation number: 7010048

## Name: Aleena Siji
## Matriculation number: 7010829 

## Name: Deepa Rani Mahato
## Matriculation number: 7012336

## Change the name of the file by adding your matriculation numbers
## (sheet05_7010048_7010829_7012336.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)
library(ggplot2)
library(ggpubr)

#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.
data <- languageR::ratings


# b) Take a look at the data frame.
head(data)

# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).
ggplot(data, aes(x = Frequency, y = Length)) +
  geom_point()


# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?
# The variables word frequency and length are not highly correlated to each other as can be seen from scatterplot.

# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Frequency, data$Length, use = "pairwise.complete.obs") # cor = -0.4281

# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?
# Correlation coefficient is not large as per the context and suggests -ve correlation 
# (i.e.) Higher the word length lower the frequency.


# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Frequency, data$Length, method = "kendall") # cor = -0.31
# The result with Kendall's method is suggesting even lesser correlation than Pearson's 
# which is what we can see from the scatter plot suggesting lesser correlation.


# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?
cor.test(data$Frequency,data$Length, method = "kendall")
# We get tau = -0.31 which we got as correlation coefficient from cor() function as well,
# p-value = 8.907e-05 and z = -3.19. p-value is < 0.05 suggesting that Frequency and Length are
# not totally uncorrelated.

# i) Finally, also calculate Spearman's rank correlation for the same data.
cor.test(data$Frequency, data$Length, method = "spearman")
# Rank correlation suggests rho = -0.43 , S = 126747 and p-value = 5.851e-05 and p<0.05
# Suggesting that true rho value is not 0 (variables are not uncorrelated)

#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
regressionModel <- lm(data$Frequency ~ data$Length, data)
summary(regressionModel)


# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.
plot(data$Frequency,data$Length)
abline(regressionModel)


# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data, aes(x = Frequency, y = Length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = Word))



#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable. 
getwd()
setwd("/Users/diveshkumar/Desktop/Stats")
csvData <- read.csv(file="digsym_clean.csv")

# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.
cast <- cast(csvData, Subject + Age ~ . ,  mean, value = "correct_RT_2.5sd", na.RM = true)
colnames(cast) <- c("Subject", "Age", "RT_mean")
head(cast)


# c) Now fit the regression model.
# DIVESH TODO : csvRegress <- lm(correct_RT_2.5sd ~ Age, data = cast)

# d) Let's go over the output - what's in there?
# How do you interpret the output?
summary(csvRegress)

# e) Plot the data points and the regression line.



# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?



# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.


# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.


# i) Make a subset of "cast" by excluding the influential subject and name it cast2.

# j) Fit the model from c) again, using cast2, and take a good look at the output.

# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?


# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?


# m) Display the two plots side by side to better see what's going on.


# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.

# o) How do you interpret R Squared?

