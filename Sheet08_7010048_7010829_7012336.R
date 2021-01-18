### Stats with R Exercise sheet 8

##########################
#Linear Mixed Effects Models
##########################

## Please fill in the exam survey posted on Teams.
##https://teams.microsoft.com/l/entity/81fef3a6-72aa-4648-a763-de824aeafb7d/_djb2_msteams_prefix_835713882?context=%7B%22subEntityId%22%3Anull%2C%22channelId%22%3A%2219%3Af7da5fa2071d48a99900f18e4eaec8f5%40thread.tacv2%22%7D&groupId=c1d382ae-bcc1-4f35-b1ba-8e312dd7bde0&tenantId=67610027-1ac3-49b6-8641-ccd83ce1b01f

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.

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
## (Sheet08_7010048_7010829_7012336.R)

###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(reshape)
library(ggplot2)

# a)There is (gender.Rdata) datasets on moodle.
#   Read in the data file (gender.Rdata) 
#   and assign it to a variable called "dat". 
#   See a description of the items in the datasets below.
dat <- read.table("gender.txt",header=TRUE)

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 


# b) Inspect "dat" and provide 2 plots. 
#    The first plot should provide insights about the relationship between WORD_TIME 
#    and ITEM_TYPE. 
#    For the second plot you should first subset the data using only RELWDINDEX == 0 and
#    then plot the WORD_TIME for the different conditions (ITEM_TYPE).
summary(dat)
head(dat)

ggplot(data=dat,aes(x=ITEM_TYPE,y=WORD_TIME))+
  geom_point() 

subdat <- subset(dat, RELWDINDEX == 0)
ggplot(data=subdat,aes(x=ITEM_TYPE,y=WORD_TIME))+
  geom_point() 

# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation). 
#    Note that we are evaluating WORD_TIME as our reponse variable. 
#    What time intervals make sense for such an experiment?
# Ans: The two outliers in plot1 (and the three outliers in plot2 RELWDINDEX == 0 analysis) could be excluded.
#      This is because being outliers they could distort the true relationship between the response variable and the features.


# d) Make a scatter plot where for each index word as the sentence progresses (RELWDINDEX),
#    the average reading time is shown for each of the two conditions (ITEM_TYPE).
#    Please use two different colours for the different conditions.

casted <-  cast(dat, RELWDINDEX + ITEM_TYPE ~ .,  mean, value = "WORD_TIME", na.RM = TRUE)
colnames(casted) <- c("RELWDINDEX", "ITEM_TYPE", "AVG_WORD_TIME")
ggplot(data=casted,aes(x=RELWDINDEX,y=AVG_WORD_TIME, color = ITEM_TYPE))+
  geom_point() 


# e) You do not need to use ggplot here, just follow the example below.
#    The code is a plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment 
help("xyplot")
print(xyplot(WORD_TIME ~ RELWDINDEX   | PARTICIPANT, dat, aspect = "xy",
             layout = c(1,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "RELWDINDEX",
             ylab = "AVG READ TIME"))

# f)  Explain the main need for switching to Linear mixed effect model for the study.
#And, report what could be the fixed and random effect structure.
# Linear mixed effect models allow us to determine both fixed and random effects, 
# as there is non independence in data. e.g. the participants in our data.
# Random effect includes categorical data like Item_Type
# Fixed effect includes variables that act as explanatory variable e.g. Relwdindex

# g) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions 
lmer1 <- lmer(WORD_TIME ~ RELWDINDEX + (1|ITEM_TYPE), data = dat)
summary(lmer1)
# Results
#Random effects:
#  Groups    Name        Variance Std.Dev.
#ITEM_TYPE (Intercept)    497.4  22.3   
#Residual              106492.6 326.3   
#Number of obs: 5782, groups:  ITEM_TYPE, 2
#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)  638.280     16.354  39.028
#RELWDINDEX     6.464      1.418   4.559
## The random effect part tells us how much variance we find amongst our grouping levels.
## Fixed effect part is similar to linear regression moedlling giving results in terms of slope and intercept.

# h} Describe how would you report and write up the analysis giving a detailed explanation for each model 
# The variance explained by our random effect is 497.4/(497.4 + 106492.7) = 0.4% 
# Therefore we can say that the difference between the Item_type explain 0.4% of the variance
# that is left after the variance explained by our fixed effects.

# i) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])

model = lmer(WORD_TIME ~ RELWDINDEX + (RELWDINDEX|PARTICIPANT), dat)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])
summary(model)
# In our plotted model we get Participant specific slopes and intercepts.
#Random effects:
#  Groups      Name        Variance Std.Dev. Corr 
#PARTICIPANT (Intercept) 19993    141.40        
#RELWDINDEX      113     10.63   -0.37
#Residual                86098    293.42        
#Number of obs: 5782, groups:  PARTICIPANT, 24
## We can see that model correctly identifies our participants as 24.
## Participant explains quite a lot of variance i.e. 19993/(19993+86098). 
## Thus random effect is also of significant value.
# Fixed effects: 
#Estimate       Std.       Error t value
#(Intercept)  637.951     29.125  21.904
#RELWDINDEX     6.519      2.517   2.591
# Fixed effect results can be interpreted directly like a linear regression model
# meaning a slope value estimate of 6.519 and an intercept of 637.951. 
