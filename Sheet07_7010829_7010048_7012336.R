### Stats with R Exercise sheet 7

##########################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 11. Write the code below the questions. 
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
## (sheet07_7010048_7010829_7012336.R)

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########

library(ggplot2)
library(broom)

# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.
kidiq_df <- read.table("kidiq.txt",header=TRUE)
summary(kidiq_df)


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
ggplot(data=kidiq_df,aes(x=mom_iq,y=kid_score))+
  geom_point() +
  geom_smooth(method='lm',formula= y~x) +
  ggtitle("Plot of Kid's Score against Mom's IQ") +
  xlab("Mom's IQ") +
  ylab("Kid's Score")


# c) State the main difference between correlation and regression .Calculate a simple regression model 
#for kid_score with mom_hs as a predictor and interpret the results.

# Correlation can be understood as standardized covariance. 
# It will give information about the relationship between the predictor and the response, 
# i.e, how they change with respect to one another.
# Regression tries to model this relationship. 
# The regression coefficient or the slope will be proportional to the correlation. 
# However, the regression coefficient is not standardized (i.e, it is dependent on the units/scale of the variable) 
# and so will not be equal to the correlation coefficient.

lm1 <- lm(kid_score~mom_iq, data=kidiq_df)
summary(lm1)
# Results obtained are intercept value of 25.79, coefficient for mom_iq = 0.60997 
# WWe observe that for 1 pt. increase in Mom's IQ the Kid's score increases by 0.609
# Adjusted R squared is 0.1991. We will compare it with next question.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model 
#    Then compare this regression model to the previous model and state which has a better model fit.

lm2 <- lm(kid_score ~ mom_iq + mom_hs, data=kidiq_df)
summary(lm2)
# Results obtained are Intercept value of 25.73, coefficient for mom_iq = 0.56, coeff for mom_hs 5.95
# Adjusted R squared value is 0.21. This value is higher than the previous method,
# Thus we can say that this model with mom_iq and mom_hs is more suitable than just using mom_iq as input variable.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))

kidiq_df$mom_hs = as.factor(kidiq_df$mom_hs)

pred = data.frame(mom_iq=kidiq_df$mom_iq, mom_hs=kidiq_df$mom_hs, kid_score_pred1=fitted(lm1), kid_score_pred2=fitted(lm2))

ggplot(data=kidiq_df,aes(x=mom_iq,y=kid_score, color=mom_hs))+
  geom_point() +
  geom_smooth(method='lm',formula= y~x) 



# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.
lm3 <- lm(kid_score~mom_iq + mom_hs + mom_iq * mom_hs, data=kidiq_df)
summary(lm3)
# results are intercept -11.48, coeff of mom_iq = 0.9689, coeff mom_hs1 = 51.2 and interaction term is -0.48
# The coefficient of interaction term (-0.48) denotes Change in Mom_iq if Mom has hs degree Mom_hs.

# g) Next, let's plot the results of this model.
ggplot(augment(lm3), aes(x=mom_iq,y=kid_score, color=mom_hs))+
  geom_point() +
  geom_smooth(method='lm',formula= y~x)


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.
?predict.lm()
kidiq2 <- subset(kidiq_df, mom_hs == 1 & mom_iq >= 100)
predict(lm(kid_score~mom_iq, kidiq2, interval = "confidence", level = 0.95))


# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?
ggplot(data=kidiq_df,aes(x=mom_iq,y=kid_score))+
  geom_point() +
  geom_smooth(method='lm',formula= y~x) +
  ggtitle("Plot of Kid's Score against Mom's IQ") +
  xlab("Mom's IQ") +
  ylab("Kid's Score")
# Confidence interval denotes that with  a 95% confidence level,
# you can be 95% confident that the confidence interval contains the population mean 
# for the specified values of the variables in the model.


# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.
plot(lm3)
# Hit enter to see plots
# We see that Normal Q-Q plot is close to a straight line and also that cook's distance 
# is being affected due to some points that have higher leverage.
