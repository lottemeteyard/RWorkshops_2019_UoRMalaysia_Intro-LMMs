# R teaching Workshops.
# Lotte Meteyard, 2019, University of Reading

# Regression & modelling

# Reference reading:
# www.zoology.ubc.ca/~schluter/R/fit-model/
# personality-project.org/r/r.lm.html
# www.pitt.edu/~njc23/Lecture10.pdf
# cran.r-project.org/doc/contrib/Faraway-PRA.pdf (ANOVA & Regression ind detail)

# The following books assume a fair level of statistical knowledge already:
# Crawley, M.J. (2005) Statistics: An Introduction using R. Wiley.
# Baayen, R.H. (2008) Analyzing Linguistic Data: A Practical Introduction to Statistics using R. Cambridge UP.

################ Regression & Modelling in R ########################

# Regression is where you'll start to see explicit data modelling

# Model fitting = finding the 'least worst model'
# and optimising the fit of the model to the data
# you want the model that makes the data most likely

# NB: I'm not really covering how to fix problems with fitted models, just how to check for them
# See this webpage for a nice summary of assumptions and possible fixes
# people.duke.edu/~rnau/testing.htm


##########  Better coding practice ########################
# load all packages you need at the top of the script
library(car)
library(effects)



########## Load data ########################

#start by looking at data 
load("MyData.RData")

str(dat1.corr.a)

#This data set comes from Meteyard, L., & Holmes, N. (2018). 
#TMS SMART–scalp mapping of annoyance ratings and twitches caused by transcranial magnetic stimulation. 
#Journal of neuroscience methods, 299, 34-44.

#The full data set can be found here: https://tms-smart.info/

# SubNo      : participant / subject ID
# Hemisphere : hemisphere of the scalp that was stimulated (1 = left, 2 = right)
# Block      : block in th experiment
# Trial      : trial in the experiment
# ACC        : accuracy of response (this data is correct trials only, so all = 1)
# RT         : reaction time in seconds
# Twitches   : strength of twitch felt with TMS pulse 0 = none, 10 = very strong)
# Task       : Task completed - choice reaction time (CRT) or Flanker task
# Congruence : Congruence of stimuli in task (congruent or incongruent)
# Axes       : Orientation of the TMS coil (North-South = front to back of head, East-West = ear to ear / side to side)
# HomLocation: Location on the scalp that was stimulated (coded as brain region or EEG location)
# RTms       : Reaction time in milliseconds
# Out        : Whether data was coded as an RT outlier per subject (this data is all outliers removed)



##########  Model formulae in R ########################

# read through help to get an idea of options 
# and what is returned
help(lm)

# example with DV ~ IV
# predict RT by strength of muscle twitches
lm.1<-lm(RTms ~ Twitches, data=dat1.corr.a)
summary(lm.1)

# see what else is in the output
names(lm.1)
# get fitted values
lm.1$fit
fitted.values(lm.1)
# get residuals
head(lm.1$res)
head(residuals(lm.1))
# get confidence intervals for parameters (IVs) fitted in model
confint(lm.1)

#look at how R has built the regression model
str(model.matrix(lm.1))



##########  Model checks ########################
# see also: www.pitt.edu/~njc23/Lecture10.pdf

# produce diagnostic plots for model
plot(lm.1)
plot.lm(lm.1)
# Histograms of residuals from model (should be normal / evenly dispersed)
hist(resid(lm.1))

# tabulate influential/problematic data points
influence.measures(lm.1)
# this places an asterisk next to each influential data point
# These are data points that may be outliers / pulling the data around
# Consider removal

# a way of selecting out the important bits rather than having to see ALL data
inf<-influence.measures(lm.1)
names(inf)  # let's look at what this function gives you
inf$is.inf
head(inf$is.inf)
# select those that are influential
which(apply(inf$is.inf, 1, any))     
# compare to graphs / influence.measures full table



# We know that RTms is skewed, so let's model with log RT
hist(dat1.corr.a$RTms)
hist(log(dat1.corr.a$RTms))

# predict log RT by strength of muscle twitches
lm.1log<-lm(log(RTms) ~ Twitches, data=dat1.corr.a)
summary(lm.1log)
# note the esimates are now in log units
plot(lm.1log)
# fewer influential points on residual plots

# overwrite initial model and remove other one from workspace to tidy up
lm.1<-lm(log(RTms) ~ Twitches, data=dat1.corr.a)
rm(lm.1log)



##########  Building more complex models ########################
# Lets work with this data and build a more complex model

# build in more IVs
lm.2<-lm(log(RTms) ~ Twitches + Task + Axes, data=dat1.corr.a)
summary(lm.2)
# We can see that Task is also significant.
# For Axes R has automatically tested the slope at each level
# It has chosen a reference level (E.W.) and compared other levels to this

# Axes does not seem to be doing anything, so lets replace with congruence
lm.3<-lm(log(RTms) ~ Twitches + Task + Congruence, data=dat1.corr.a)
summary(lm.3)

# specify main effects and interaction of Task with Congruence
lm.4<-lm(log(RTms) ~ Twitches + (Task*Congruence), data=dat1.corr.a)
summary(lm.4)
# This notation gives you main effects and interactions

#Is a model with an interaction better than just main effect?
anova(lm.3,lm.4)
#nearly!
# we can use anova to compare nested models

#look at how R has built the regression model / coded the model for analysis
str(model.matrix(lm.4))
head(model.matrix(lm.4))


# Visit this page: www.zoology.ubc.ca/~schluter/R/fit-model/
# Find the alternative notation for adding in interaction alone
# Write a model with only the interaction for Task and Congruence, no main effects



########## Multicollinearity and auto-correlation ########################

# Multi-collinearity
# When predictor variables are correlated, the estimated regression coefficient 
# of any one variable depends on which other predictor variables are 
# included in the model. That is, you can't get a true partial effect.
# see onlinecourses.science.psu.edu/stat501/node/82

# Taken from www.statmethods.net/stats/rdiagnostics.html
# From the car package
vif(lm.4) # variance inflation factors
# If VIF is greater than 2, then potentially problems here

sqrt(vif(lm.4)) > 2  # any problems? 


# Temporal correlation in errors
# Autocorrelation in the residuals distorts the regression statistics, 
# (e.g. if positive, will inflate F statistics by fitting small SEs).
# It suggests the model is missing a useful predictor variable 
# or it needs a time series component.
durbinWatsonTest(lm.4)

#Looks like we have auto-correlation in the residuals
# Not that surprising for RT data
# We can add in a predictor for trial
str(dat1.corr.a)
lm.5<-lm(log(RTms) ~ Trial + Twitches + (Task*Congruence), data=dat1.corr.a)
summary(lm.5)
durbinWatsonTest(lm.5)
# Has reduced a bit but not much


############### Effects package #########################

# Now we'll see an example of how R becomes a real pleasure

# Using the effects package
# NB: nice intro to this package quantoid.net/IntroR/Handout6_2012.pdf
# some examples below taken from this document

eff.5<-allEffects(lm.5)

# make figure window bigger so you can see everything..
plot(eff.5)

# select specific effects to look at
unique(dat1.corr.a$Twitches)
# select Twitches effect, choose some sensible no. bins
eff<-effect("Twitches",lm.5,default.levels=10)
plot(eff)






################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")
