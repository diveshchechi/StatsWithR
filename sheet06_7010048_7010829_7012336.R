### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 04. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name: Divesh Kumar
## Matriculation number: 7010048

## Name: Aleena Siji
## Matriculation number: 7010829 

## Name: Deepa Rani Mahato
## Matriculation number: 7012336

## Change the name of the file by adding your matriculation numbers
## (sheet06_7010048_7010829_7012336.R)

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 
?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.
data <- amis
head(data)

# c) All our columns have numeric type. Convert the categorial columns to factors.
str(data)
data$period = as.factor(data$period)
data$warning = as.factor(data$warning)
data$pair = as.factor(data$pair)
# As except speed all other variables are categorical.

# d) Plot boxplots for the distribution of `speed` for each of the `period` values 
# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)
dWarning1 = subset(data, warning == 1)
dWarning2 = subset(data, warning == 2)
# Box Plot for data with Warning 1
ggplot(dWarning1, aes(x = period,y = speed))+
  geom_boxplot() + ggtitle("Warning value 1")
# Box Plot for data with Warning 2
ggplot(dWarning2, aes(x = period,y = speed))+
  geom_boxplot()  + ggtitle("Warning value 2")


# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?
# Regarding readings with Warning 1 we can see that the mean speed dropped immediately
# after the sign was erected however in "after some time" situation mean speed increased
# indicating that people started ignoring the warning signs.


# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?
# to differentiate between the effect of having a sign versus random changes in speed


#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.
Warning1 = subset(data, warning == 1)
casted <-  cast(Warning1, period + pair ~ .,  mean, value = "speed", na.RM = TRUE)
colnames(casted) <- c("period", "pair", "avg_speed")
Warning1
casted

# b) Build boxplots of the average speed depending on "period".
ggplot(casted, aes(x = period,y = avg_speed))+
  geom_boxplot()  + ggtitle("Boxplot avg_speed vs pair")

# c) Looking at the boxplots, is there a difference between the periods?
# Yes, in the "immediately after installing sign period" the avg_speed dropped from
# the avg_speed when no warning sign was installed, however in "after some time" phase
# the avg_speed actually increased even higher than the pre warning sign phase.


# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)



# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.


# h) what were the degrees of freedom from the result in part g)

# i) Calcuate the effect size and interpret the results. 

# j) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().


# k) Report the pairwise p-values and interpret the result in detail.


# l) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change? 

# m) If the results change why do they? What does Bonferroni correction do?


# n) If the assumption of Normality does not hold, which test would you be using in this scenario.

#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.
casted_data2 <- cast(amis, period + pair + warning ~ .,  mean, value = "speed", na.RM = TRUE)
colnames(casted_data2) <- c("period", "pair", "warning", "avg_speed")
casted_data2 

# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.
casted_data3 <- cast(casted_data2, period + warning ~ .,  mean, value = "avg_speed", na.RM = TRUE)
colnames(casted_data3) <- c("period","warning","mean_speed")
casted_data3

# c) Do you think there is a significant difference between some of the groups?
# Yes, there seems to be significant difference betweeen some of the groups, for example, between 35.7 and 39.8 

# d) State the main difference between the applicabilty of 1-way and 2-way ANOVA.
# 1-way anova is used when we test for difference in means between different levels of one factor variable. 
# 2-way anova is used when we test for difference in means between different levels of more than one factor variables. 
# here, there are two variables - period and warning - that could have an effect on the data and could also be interacting with one another.

# e) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail. Properly formulate the findings!
summary(aov(mean_speed ~ period + warning, data=casted_data3))

# f) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
