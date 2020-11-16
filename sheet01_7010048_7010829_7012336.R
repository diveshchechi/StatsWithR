### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, November 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name: Divesh Kumar
## Matriculation number: 7010048

## Name: Aleena Siji
## Matriculation number: 7010829 

## Name: Deepa Rani Mahato
## Matriculation number: 7012336

## Change the name of the file by adding your matriculation numbers
## (sheet01_7010048_7010829_7012336.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
## Using getwd() found the working directory to be "Users/mac"
getwd()

## b) Get help with this function.
help(getwd)

## c) Change your working directory to another directory.
## Using setwd(dir) set directory to be dir

###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a pack'https://cran.rstudio.com/bin/macosx/contrib/4.0/languageR_1.5.0.tgz'age.

## a) Install the package "languageR" and load it.
## Installed from 
library("languageR")

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?
data("dutchSpeakersDistMeta")
head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
(nrow(dutchSpeakersDistMeta))
# There are 165 rows.

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
boxplot(dutchSpeakersDistMeta$AgeYear~dutchSpeakersDistMeta$Sex, xlab = "Sex", ylab = "Age Year")

## e) Does it seem as if either of the two groups has more variability in age?
# Yes, The females seem to have more variability in age but outliers are less wheras ages of males have low 
# standard deviation but have a few outliers.

## f) Do you see any outliers in either of the two groups?
# Yes, we see outliers in the group of males.

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?
males <- subset(dutchSpeakersDistMeta, dutchSpeakersDistMeta$Sex == "male")
females <- subset(dutchSpeakersDistMeta, dutchSpeakersDistMeta$Sex == "female")
mean(males$AgeYear)
mean(females$AgeYear)
sd(males$AgeYear)
sd(females$AgeYear)
# Mean value for AgeYear for both the groups is as follows:
# Males = 1967.301 , Females = 1966.889
# There is not much difference in mean of both the groups.
# Standard deviation value for both the groups is as follows:
# Males = 14.66 , Females = 15.87 
# Thus Females have a higher standard deviation value.
# Same can be verified from the boxplots above.


## h) What do the whiskers of a boxplot mean?
# The upper whisker of the box plot is the largest dataset number smaller than 1.5IQR above the third quartile. and the lower whisker
# is the smallest number larger than 1.5IQR(Inter Quartile Range).

## i) What is the inter-quartile range in the boxplot?
# Inter quartile range is the range between 25th and 75th percentile. i.e. Q3- Q1

## j) Is the plot positively or negatively skewed?
# Both the plots are negatively skewed for Females vs AgeYear and Males vs AgeYear.


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
# It is DISCRETE data and the scale is ratio scale.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
# There are advantages in using data frame than using a matrix, e.g. it can store data of various types be it numeric, string etc.


## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
(pps <- c(1:25))

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 
(stories <- data.frame(pps,obs))

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
class(stories$pps)

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
stories$pps <- factor(stories$pps)
class(stories$pps)

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks = 8, xlab = "Observations", main = "Histogram of observations")


## i) Create a kernel density plot using density().
plot(density(stories$obs), main = "Kernel density plot")

## j) What is the difference between a histogram and a kernel density plot?


## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)
hist(stories$obs,
     prob=TRUE, 
     xlab="Observations",
     main="Density plot and histogram")
box(bty="l")
lines(density(stories$obs), col = "blue")


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(-5, 5, by=0.1)


## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
help(dnorm)
y <- dnorm(x,mean=0,sd=1,log=FALSE)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x,y,type="l",col="blue",ylim=c(0,0.8))


## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v=0,lty=2)
## adds lines to already existing plot


## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
beaver1
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
(b1mean <- mean(b1temp))
(b1sd <- sd(b1temp))
plot(b1temp,dnorm(b1temp, mean=b1mean, sd=b1sd, log = FALSE))
## DOUBT - what does this plot represent? densities of observed values as if it were a normal distribution and therefore, not the actual densities?? 

## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?
pnorm(36.91,mean=b1mean, sd=b1sd, log = FALSE)
## the cumulative probability (i.e, 36.91 or lower) is 0.5976 --> the probability of 36.91 or more extreme values is 1 - 0.5976 = 0.4024
## likely that this comes from a normal distribution
pnorm(38.13,mean=b1mean, sd=b1sd, log = FALSE)
## the cumulative probability (i.e, 38.13 or lower) is 1 --> the probability of 36.91 or more extreme values is 1 - 1 = 0 
## very unlikely that this comes from a normal distribution


## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?
## DOUBT - 20 samples or 5 samples of size 20?? (assuming the latter and proceeding)
b1sample <- rnorm(20,mean=b1mean, sd=b1sd)
hist(b1sample,breaks=10)
## the distributions vary widely between the different random samples


