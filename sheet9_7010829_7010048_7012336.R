##########################
#Week 11: Model Families and Logistic Regression
##########################
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students


## Please write below your (and your teammates) name, matriculation number. 
## Name: Divesh Kumar
## Matriculation number: 7010048

## Name: Aleena Siji
## Matriculation number: 7010829 

## Name: Deepa Rani Mahato
## Matriculation number: 7012336

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person iâ€™s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.
speeddating <- read.csv(file="Speed Dating Data.csv")
summary(speeddating)
head(speeddating)


#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.
mylogit <- glm(data = speeddating, dec ~ attr + sinc + intel + fun, family = "binomial")

ggplot(gather(subset(speeddating,select=-c(dec)), cols, value), aes(x = value)) + geom_histogram()+facet_wrap(~cols)

ggplot(gather(subset(speeddating,select=-c(dec)), cols, value), aes(x = value)) + geom_boxplot()+facet_wrap(~cols)
#predictors intel and sinc has more outliers compared to attr and fun
#this also verifies the positive skewness of the predictors intel and sinc

summary(mylogit)
#p value of intel is too high ie 0.22 which shows that it is insignificant
#p value is of attr and fun is less hence they are most significant.

plot(mylogit)


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model. 
randInt_iid <- glmer(dec ~ attr + sinc + intel + fun +(1 | iid), data = speeddating, family = binomial)
summary(randInt_iid)

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.Interpret the model outcome and explain what the varying Intercepts are.
randInt_pid <- glmer(dec ~ attr + sinc + intel + fun +(1 | pid), data = speeddating, family = binomial)
summary(randInt_pid)

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)
randInt <- glmer(dec ~ attr * sinc * intel * fun + (1 | iid) + (0 + attr + fun | iid), data = speeddating, family = binomial(logit))
summary(randInt)

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?
AIC(randInt_iid, randInt_pid, randInt)
## We observe that our model in (2) gives 6698 (3) gives AIC 8236 and in (4) gives 6675
## The smaller the AIC the better the fit is, therefore our model randInt_pid in (3) gives the beszt results.

####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
head(p)
p <- p %>% rename(id = ï..id)
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.
pairs(p)
# there exists some effect but not highly correlated or linear.

#(7) what model family is used and explain the reason for using it.
#Run a generalized linear model to test for significance of effects.
fitglm <- glm(num_awards ~ math + prog, data=p, family=poisson(link=log))
summary(fitglm)
# poisson model family is used as the response variable is a count variable. it would follow poisson distribution rather than normal distribution.

#(8) Do model comparisons to find out whether the predictors significantly improve model fit.
fitglm <- glm(num_awards ~ math , data=p, family=poisson(link=log))
summary(fitglm)
# Compared with model including math and prog the model only with math gives AIC of 384.08 
# whereas model with math and prog gives us 373.5. There fore predictors improve our model prediction
# as lower value of AIC is better.

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.
fitlm <- lm(num_awards ~ math + prog, data=p)
summary(fitlm)
# the slopes and the significance have changed

##Task 3

## Please explain within and between subject experimental design.
##How does the design affect the random effect structure during analysis.

# In within-subject experimental design, the same set of participants are subjected to different conditions of study.
# This is like the repeated measures test. 
# In between-subjects experimental design, one set of participants is subjected to one condition and another set of participants are subjected to the other condition. 
# The random noise will be less in within-subjects experimental design. 
