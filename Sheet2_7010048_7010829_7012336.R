###############
### Cleaning Data - Exercise 1
###############

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a DataCamp tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number. 
## Name: Divesh Kumar
## Matriculation number: 7010048

## Name: Aleena Siji
## Matriculation number: 7010829 

## Name: Deepa Rani Mahato
## Matriculation number: 7012336

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 
setwd("/Users/diveshkumar/Desktop/Stats")
getwd()

# 2. Read in the data into a variable called "dat".
dat <- read.csv("digsym.csv")

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
# after installing using install.packages()
library("languageR")
library("stringr")
library("dplyr")
library("tidyr")

# 4. How many rows, how many columns does that data have?
nrow(dat) 
#3700
ncol(dat)
#11

# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.
head(dat, n= 20)
tail(dat, n= 20)

# 7. Is there any missing data in any of the columns?
sum(is.na(dat))
#370

# 8. Get rid of the row number column.
dat <- select(dat, -X)
colnames(dat)

# 9. Put the Sub_Age column second.
dat <- dat[,c(1,10,2,3,4,5,6,7,8,9)]
colnames(dat)

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.
dat <- replace(dat, 1, "Kopie")

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- dat
data2<-data2[!(data2$List == "Trial:1"),]
dat <- data2
rm(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat <- separate(dat, col = "Sub_Age", into = c("Sub","Age"), sep = " _ ")
colnames(dat)

# 13. Make subject a factor.
dat$Sub <- as.factor(dat$Sub)
str(dat)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
dat$File <- str_extract(dat$File,"[a-z]+")

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).
str_pad(dat$File, 8, "right", "0")

# 16. Remove the column "List".
dat <- select(dat, -List)
colnames(dat)

# 17. Change the data type of "Age" to integer.
dat$Age <- as.integer(dat$Age)
str(dat)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?
sum(is.na(dat))
# 0

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.
dat$accuracy <- if_else(dat$StimulDS1.RESP == dat$StimulDS1.CRESP, 1, 0 )

# 20. How many wrong answers do we have in total?
sum(dat$accuracy == 0)
# 185

# 21. What's the percentage of wrong responses?
(sum(dat$accuracy == 0) / nrow(dat)) * 100
# 5.555556

# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 
correctResponses <- dat[dat$accuracy == 1, ]

# 23. Create a boxplot of StimulDS1.RT - any outliers?
boxplot(correctResponses$StimulDS1.RT)

# 24. Create a histogram of StimulDS1.RT with bins set to 50.
hist(correctResponses$StimulDS1.RT, breaks=50)

# 25. Describe the two plots - any tails? any suspiciously large values?
# Plot seems to be positively skewed and a very large value 13852 is clearly an outlier

# 26. View summary of correct_RT.
summary(correctResponses$StimulDS1.RT)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".
cleaned <- filter(correctResponses, correctResponses$StimulDS1.RT != 13852)


###############
### Exercise 2: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR)
summary(dative)
help(dative)

## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
contingencyTable <- table(dative$LengthOfTheme)
contingencyTable


## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme, main = "Hist of LengthOfTheme")
boxplot(dative$LengthOfTheme)

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?
# Distribution of parameter of sample like mean from several samples called sampling distribution.
# Whereas Sample distribution is just the distribution of the data from the sample. 

## e) We are going to need a random sample of the variable 'LengthOfTheme'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'
randomsampleoflengths <- sample(dative$LengthOfTheme, 5)

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 
randomsampleoflengths2 <- sample(dative$LengthOfTheme, 5)

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.
mean1 <- mean(randomsampleoflengths)
mean2 <- mean(randomsampleoflengths2)
means5 <- c(mean1,mean2)

## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.

# Cleaning means5 of old means calculated in g
means5 <- c()
for(i in 1:1000){
  randomSampleOfFive <- sample(dative$LengthOfTheme,5)
  meani <- mean(randomSampleOfFive)
  means5 <- append(means5,meani)
}

## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- c()
for(i in 1:1000){
  randomSampleOfFifty <- sample(dative$LengthOfTheme,50)
  meani <- mean(randomSampleOfFifty)
  means50 <- append(means50,meani)
}

## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?
summary(means5)
summary(means50)

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?
hist(means5, breaks = 15)
hist(means50, breaks = 15)
# The means5 have a positive skew as can be seen from the histogram.

## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?
# Means5 has bigger numbers than means50 as small sample size leads to higher variability in data wheras bigger sample size
# is more representative of the entire population.

###############
### Exercise 3: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?
## Ans: We can understand confidence interval as the region (in terms of how many standard deviations away from the mean in the distribution)
##      within which the population mean can be expected to be in, with a said level of confidence. 


## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'
library(lsr)
library(sciplot)

## c) Look at the description of the function ciMean to see which arguments it takes.
?ciMean

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.
ciMean(dative, conf=0.95)
ciMean(dative$LengthOfTheme, conf=0.95)
mean(dative$LengthOfTheme)


## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
##    Ans: Yes, it falls within the confidence interval for 95%.
##         <what does it mean?>
## did they mean to calculate mean for a sample instead of the whole dataset and check if it falls within the confidence interval?


## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
?bargraph.CI
bargraph.CI(dative$AnimacyOfTheme, dative$LengthOfTheme, xlab="Animacy of Theme", ylab="Length of Theme", ylim=c(0,5))


## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?
bargraph.CI(dative$AnimacyOfTheme, response = dative$LengthOfTheme, xlab="Animacy of Theme", ylab="Length of Theme", ylim=c(0,5), ci.fun=function(response) ciMean(response))
## In this plot, the ciMean() function is used to compute the upper and lower limits of the confidence interval. 
## Hence, the new limits replace the default limits (based on standard error) used in previous question.

