# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading

#Thanks to Rob Davies at Lancaster Uni, UK
#http://www.lancaster.ac.uk/psychology/contact-and-getting-here/people/robert-davies
# Nice site to explain basic concepts
# onlinecourses.science.psu.edu/stat504/node/157
# See also:
https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf


########################## Workspace ##########################
#If you completed the Regression script then load the workspace from that
#if you don't have it already
#See the 'navigating around R Studio' as a reminder


########################## Mixed models: lme4 & lmer ##########################
# We will use lme4, as it is generally better supported now
# and more widely used

#good coding practice to put all your packages at the top

#load packages we need
library(lattice)
library(lmerTest)
library(lme4)
library(effects)
library(pbkrtest)
library(optimx)
library(MuMIn)

# We'll use two here, to help us interpret what we get
# lme4 does not come with significance tests
# or support the 'step' function
#lmerTest gives p values/significance for fixed effects


# Brief powerpoint presentation here for basic concepts.


########################## Building a random intercepts LMM ##########################

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




# Let's do a bit of tidying up and make sure we have everything coded properly
# For things to work we need to code categorical variables as factors

# Change SubNo and ACC to factors/categories 
dat1.corr.a$SubNo<-as.factor(dat1.corr.a$SubNo)
dat1.corr.a$ACC<-as.factor(dat1.corr.a$ACC)
str(dat1.corr.a)

# Start with a regression model yesterday (e.g. Gelman & Hill recommend regression models first)
# It also helps to see how LMMs differ

lm.1<-lm(log(RTms) ~ Trial + Twitches + (Task*Congruence), data=dat1.corr.a)

summary(lm.1)
lm.1$call  #extract model equation

# modified to add random intercepts for subjects 
# now we use lmer()

lmer.1<-lmer(log(RT) ~ Trial + Twitches + (Task * Congruence)
             + (1|SubNo), data = dat1.corr.a)
            
#this notation tells R to allow intercepts to vary for Subjects 
# if you wanted to add random intercepts for items/words it would be + (1|Items)
# Note here the importance of having all relevant columns coded e.g. for items, subjects etc.

summary(lmer.1)
summary(lm.1)  
# what has changed?
# compare the t values and coefficients

# Direct comparison: can't use anova here, use AIC or BIC instead
# the lower the score the better
AIC(lm.1,lmer.1)
BIC(lm.1,lmer.1)
# we have added in a parameter (the random effect) and this
# has made the model much better



# Diagnostic plot of residuals
plot(lmer.1)
# This looks OK, with some reduced spread at lower values



########################## Looking at random effects ##########################

# Let's check that random effects are significant (i.e. doing work)
help(rand)
# does the random effect make the model better / data more likely?
rand(lmer.1)

# to extract the random effects
x<-ranef(lmer.1)  # extract random effects
str(x)
# Plot random effects from model (lattice package)

dotplot(ranef(lmer.1, whichel = "SubNo", condVar = TRUE))
# now you can see the variation across subjects for log RT
# e.g. subject 8 looks slow


########################## Looking at fixed effects / IVs ##########################

#best way to do this is via effects package (as for regression)
#plot fixed effects & interactions
eff<-allEffects(lmer.1)
plot(allEffects(lmer.1))

#plot interaction of Congruence and Task
plot(effect(term="Task*Congruence",mod=lmer.1),ylab="logRT",xlab=" ",ci.style="lines")





########################## Random intercepts & slopes ##########################

# modified to add random slopes
# This will allow the effect of Twitch strength to vary across subjects
# Look at this effect from previous model
plot(eff$Twitches)
# Adding random slope will mean that each subject can have their 'own' gradient for this slope

# add in notation for Twitches slope to vary across subjects
# the 1+ means that we think the intercepts and slopes will be correlated

lmer.2<-lmer(log(RT) ~ Trial + Twitches + (Task * Congruence)
             + (1 + Twitches|SubNo), data = dat1.corr.a)
summary(lmer.2)
# we can see a correlation of 0.33 between intercepts and slopes 
# so participants who have longer RTs show a greater effect of Twitch strength

AIC(lmer.1,lmer.2)
# this has improved our model a bit (AIC is more negative)

# likelihood ratio test (needs to fit with ML, and models must be nested)
anova(lmer.1,lmer.2)

#simple plot of random effects (left panel is intercept and right is slope for Twitches)
dotplot(ranef(lmer.2, condVar = TRUE))

# see how fit for Twitches varies over Subjects - this is the ranbdom effect Twitches|SubNo
# http://www.statmethods.net/advgraphs/trellis.html
lattice::xyplot(fitted(lmer.2) ~ Twitches|SubNo, data=dat1.corr.a, 
                main="Twitches by Subject Random Effects",
                ylab="Fitted log RT",xlab="Twitch strength")

# look at original data
lattice::xyplot(log(RTms) ~ Twitches|SubNo, data=dat1.corr.a, 
                main="Twitches by Subject Random Effects",ylab="Fitted log RT",xlab="Twitch strength")


# "By default, lme4 assumes that all coefficients associated with the same random-effects term
# are correlated" (p. 7; Bates lme4 document in workshop folder)
# If you don't want intercepts and slopes will be correlated
# use 0

lmer.3<-lmer(log(RT) ~ Trial + Twitches + (Task * Congruence)
             + (0 + Twitches|SubNo), data = dat1.corr.a)

AIC(lmer.2, lmer.3)
# What does it show for AIC values? Which model is better (has lower AIC)?

# Should we inlcude a correlation between intercepts and slopes?




######### Model building simple to complex & comparison with pbkrtest #########
#Thanks to Rob Davies at Lancaster Uni, UK
#http://www.lancaster.ac.uk/psychology/contact-and-getting-here/people/robert-davies

# to reduce collinearity and help interpretation, consider standardizing numeric predictors
# can use scale function to do this (creates z score of variables)
# Exampledata$zIV1 <- scale(Exampledata$IV1, scale = TRUE, center = TRUE)

# start with an intercept only model with random effects
# fit with maximum likelihood

lmer.0<-lmer(log(RT) ~ (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.0)

# add predictors of trial and block

lmer.1<-lmer(log(RT) ~ Block + Trial + (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.1)

# add predictors Task and Congruence

lmer.2<-lmer(log(RT) ~ Block + Trial + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.2)

# compare model fits:

BIC(lmer.0, lmer.1, lmer.2)
AIC(lmer.0, lmer.1, lmer.2)


# we can also use the likelihood ratio test comparison:-

anova(lmer.0, lmer.1)      
anova(lmer.1, lmer.2)	


# get confidence intervals for effects estimates for last model

summary(lmer.2)
confint(lmer.2, method = "Wald")  


# To do model comparisons with bootstrap methods
# PBmodcomp(largeModel, smallModel, nsim = 1000, ref = NULL, seed=NULL,
#          cl = NULL, details = 0)

PBmodcomp(lmer.2, lmer.0, nsim=50) 
#this may take some time, so I've reduced the number of simulations to 50
#in reality you would want to do a much higher number (e.g. 1000)

#using update instead of separate models - for nested comparisons
#can be useful to keep track of what comparison you are checking
#  e.g. mod<-lmer(DV~IV1+IV2+(1|trial:Subject), data=dat1, REML=FALSE)
#       mod_no.IV2 <- update(mod, .~.-IV1)

lmer.2<-lmer(log(RT) ~ Trial + Twitches + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)

#In this model, I'm removing the Congruence predictor to see whether it is needed
lmer.2.noCong <- update(lmer.2,.~.-Congruence)

#Likelihood ratio test
anova(lmer.2,lmer.2.noCong)
#so congruence is an important predictor



######## Building up and checking random effects #########
# Similar process here, but keep fixed effects constant now
# Now use REML = TRUE (will average across fe then calculate re)


lmer.0<-lmer(log(RT) ~ (1|SubNo), data = dat1.corr.a, REML = TRUE)
summary(lmer.0)

lmer.1<-lmer(log(RT) ~ (1|SubNo) + (1|HomLocation), data = dat1.corr.a, REML = TRUE)
summary(lmer.1)


#Likelihood ratio test
anova(lmer.0,lmer.1)
#Information criteria
AIC(lmer.0,lmer.1)
BIC(lmer.0,lmer.1)



######### Model checking #########
# Checks should be completed as for for Regression and ANOVA
# i.e. normality, outliers, collinearity etc.

# you can check these things by running regression models on the data (i.e. no random effects)
# if problems are present in these models, they will be present for LMMs.

# Plot residuals as quantile plots
qqnorm(resid(lmer.2))
qqline(resid(lmer.2))

#Plot residuals as histogram
hist(residuals(lmer.2))

# plot fitted values against residuals
plot(lmer.2)
plot(fitted(lmer.2), residuals(lmer.2),
     xlab = "Fitted Values", ylab = "Residuals")
#plot deviation of resiudals from normal
abline(h=0, lty=2)
lines(smooth.spline(fitted(lmer.2), residuals(lmer.2)))



#### INDEPENDENT PRACTICE ####

#Complete questions 1-3




##### Convergence ########
# If your model fails to converge...

# From stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge
# And stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
# stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4

# see also: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022127.html

#And this recent paper
#Brauer, M., & Curtin, J. J. (2017). 
#Linear mixed-effects models and the analysis of nonindependent data: 
#A unified framework to analyze categorical and continuous independent variables that vary within-subjects and/or within-items.
#Psychological Methods, 23(3), 389-411.
#http://dx.doi.org/10.1037/met0000159


# Check for: zero variance estimates or +/- 1.0 correlation estimates
# zero variance means that the random effect is not really adding anything
# +/-1 correlations mean that there is overfitting as intercepts and slopes are perfectly correlated
summary(lmer.2)

# Check gradient: should be <0.001
# https://github.com/lme4/lme4/issues/120
# Check gradient value
relgrad <- with(lmer.2@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

# Re run model with different optimizers (library optimx)
lmer2_bobyqa <- update(lmer.2, control = lmerControl(optimizer="bobyqa"))

lmer2_NM <- update(lmer.2, control = lmerControl(optimizer="Nelder_Mead"))

#models run with different optimizers may converge when the original didn't


# Compare parameter estimates across model fits
# Fixed effects
fixedEst<-matrix(nrow=length(fixef(lmer.2)),ncol=6)
rownames(fixedEst)<-names(fixef(lmer.2))
fixedEst[,1]<-fixef(lmer.2)
fixedEst[,2]<-fixef(lmer2_bobyqa)
fixedEst[,3]<-fixef(lmer2_NM)
fixedEst
# If they all look quite similar and seem sensible, then model is probably OK


######### Calculating significance for fixed effects & the model #########

# Time for a bit of reflection
# see glmm.wikidot.com/faq
help(pvalues)

#lmerTest comes with a number of options for getting p values
#we have been relying on the defaults
#also not standard practice yet for using coefficient or anova/LRT or both
#you should report both
#if planning to do LMMs, you should read Luke (2017) and follow guidance in there
#Luke, S. G. (2017). Evaluating significance in linear mixed-effects models in R. 
#Behavior Research Methods, 49(4), 1494-1502.

# Confidence intervals for fixed effects
confint.merMod(lmer.2, method = "Wald")
# and what is Wald?
# en.wikipedia.org/wiki/Wald_test

#alternative that may take longer...
confint.merMod(lmer.2, method = "profile")

#package MuMIn to get R squared for LMMs
#package MuMIN

lmer.0<-lmer(log(RT) ~ (1|SubNo), data = dat1.corr.a, REML = FALSE)
lmer.2<-lmer(log(RT) ~ Trial + Twitches + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)

#so comparing here against model with random effects only
r.squaredLR(lmer.2, null=lmer.0)


# R2 
# Based on comparing model to intercept only model
# From glmm.wikidot.com/faq and Jarrett Byrnes
# with a line of code from me to print the model name
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  print(c(deparse(substitute(m)), summary(lmfit)$r.squared))
}

# Compare the r2 values for the two models
r2.corr.mer(lmer.0)
r2.corr.mer(lmer.2)

########################## Reporting LMMs ##########################

#Mixed models are currently reported in a wide variety of formats
#In general, people are undereporting.

#You should report mixed model analyses like REGRESSION
#Therefore, for the final model, a table that includes both fixed AND random effects components
#Yes, this table may be large.
#Also include some measure of model fit (e.g. R2)
#Include as an index/note on this table the final model equation
#Include in appendices a complete list of the models you fit and the fitting process
#For example, did you go from simple to complex? or simplify due to convergance issues?
#How did you compare models? With ANOVA? With AI/BIC?
#Ideally submit the raw data and your analysis script with any publication



################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")




