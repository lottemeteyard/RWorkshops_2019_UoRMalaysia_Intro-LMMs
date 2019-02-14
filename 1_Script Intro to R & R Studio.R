# Intro to R Coding Club.
# Lotte Meteyard, 2019
# School of Psychology and Clinical Language Sciences,University of Reading

#### Key websites ###

http://r-statistics.co/
http://r-statistics.co/R-Tutorial.html
http://r-statistics.co/Statistical-Tests-in-R.html

####################  Welcome to R ###################################################

# We are using R Studio, a much friendlier way to use R
# If we use R itself, all you get is the Console (the window below)
# Script files can be used and annotated
# Look at the bottom of this script file, you can navigate to different parts

#R Studio
#Using the command line in the console (bottom panel)
#Running code from the top panels/R Script files
#Environment
#History
#Bottom right panel: files, plots, packages, Help


#################### Housekeeping #######################

#(1) ALWAYS set working directory
getwd()
filepath<-getwd()
filepath

# Copy and paste the whole Workshops folder somewhere sensible
filepath<-"C:/FolderPathHere/WorkshopsFolder"  
#write your preferred file path in here
setwd(filepath)

#a simpler way is to use the GUI in R Studio
#Bottom right, click the ... choose the file location
#then click 'More' and 'set as working directory'

######## Create an R script file / analysis log #######


# File - New File - R Script (or Ctrl+Shift+N). 
# This acts as a record of what you have done and a way to store useful code
# Save this and call it something sensible.
# Using # marks writing as not code / text for annotating 
# adding ## text ## around something makes it a heading (see below) 

# (1a) create your own new scripting file and store it somewhere.
# (1b) Copy and past the line of code from line 29 above into your script, run it
# (1c) What does it do? Annotate your script to explain what it does


#################### Run some code #######################
1 + 1 #place cursor on line and click 'Run'

#(2) Go to the History tab and run some previous code



######### Store data in objects/variables #######
temp <- c(1,3,5,6,17,8) #temp has now appeared in your Environment



######### Do things with them #######
str(temp)
summary(temp)
(sum(temp))/length(temp)
mean(temp)
plot(temp)
order(temp)  #gives you the index of the cells in order of magnitude

temp[6]
temp[6]<-20  #change cell 6 to hold the value 20
plot(temp)



############################ Importing Data #############################



### Importing text files #########

# R prefers to import data from .txt or .csv files

# With data in .txt or .csv format, there are two ways to get it into R.
# Import dataset in the Environment (top right panel)
# Read it in yourself
dat1 <- read.table(file="TMS_data.txt",header=TRUE)
# or 
dat1 <- read.delim("TMS_data.txt")
#This has appeared as dat1 (data) in your Environment


### Importing SPSS files #########

# It is possible to import directly from SPSS
#This requires us to use a 'package' - a suite of functions
#created by someone for us to use.

# Install package 'foreign'. Then set it running in R.
library(foreign)
# or
require(foreign)

# Read in SPSS file
dat2<-read.spss("SPSS_Data1.sav")
str(dat2)


# It is possible to import directly from excel, but this requires
# installing perl on the PCs (in mac, this is already done). So we won't cover 
# that in these workshops. For future reference, the package needed to do this
# is 'gdata'. NB: it works with .xls



### Data structures in R #########

# R works with different data structures. One particularly useful one is 
# the dataframe. A data frame is more general than a matrix, 
# in that different columns can have different modes (numeric, character, factor, etc.). 
# This is similar to SAS and SPSS datasets

# dat2 currently saved as a List object
dat2<-data.frame(dat2)
str(dat2)

# look at variable names and change them
names(dat2)
#rename second column
names(dat2)[2]<-"HeightCM"
head(dat2)
#rename all columns
names(dat2)[1:4]<-c("AgeYrs","HeightCM","ShoeSizeUK","HairColour")
names(dat2)
head(dat2)


#list objects in your Environment
ls()

# Remove objects
rm(dat2)
rm(filepath,temp)



#########  Looking at data #########

# We are going to work with dat1
# This is for a Flanker and Choice Reaction Time (CRT) Task
# whilst people had TMS across different scalp locations
# we recorded the reported strength of muscle twitches for each trial (0-10 in strength)
# (Meteyard and Holmes, submitted)

#Click on the blue arrow next to dat1 in the Environment panel
#Click on dat1 itself in the Environment panel

# Look at the data
str(dat1) 
# Notice that R automatically turns character/nominal variables into factors 
# with levels   

names(dat1)   # Look at all the variable names

head(dat1$Axes)  # The first few elements in a variable
head(dat1$SubNo)
tail(dat1$Twitches) 


# Use other commands on http://www.statmethods.net/input/contents.html
# To look at dat1 e.g...

dim(dat1)  #get the size (r/c), good check when combining variables/objects

#(3) Find another command to inspect the dat1 dataset and run it


############## Indexing variables ##################

# Roman Catholic - rows, columns
dat1[47,3]
dat1[3,9]

# When you want to look at some data use []
dat1[47,3:5]

# When you want to apply a function use ()
levels(dat1$Axes)

# When data is stored in a dataframe, use $column_name
names(dat1)
dat1$HomLocation


######## Basic descriptive statistics ##########

#explore a variable

mean(dat1$Twitches)
mean(dat1$Twitches,na.rm=TRUE) #to get this to work you need to remove missing values
#this also highlights the importance of NAs coded into data sets for R

sd(dat1$Twitches,na.rm=TRUE)


# frequency count for each value
# useful for checking for missing data / errors in computations
table(dat1$Twitches)

# overall summary statistics
summary(dat1$Twitches)

#checking normality

# (4) Find out how to create a histogram for the column of data dat1$Twitches




# quantile plots
qqnorm(dat1$RT)        #plot quantiles against theoretical normal distribution
# normal distribution should fall along line
qqline(dat1$RT)







################# Saving work #################

# The simplest thing is to save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  #check the Files tab bottom right, it should appear

# Save specific data
# e.g. save our summary data from above
save(dat1,file="TMSData.RData")

# Save a text file
write.table(dat2, "SPSS_demographics.txt", sep="\t")

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")

#And this R script is already saved!



################# General principles for R #################

# R means coding
# R demands that you problem solve. And be transparent.

# Persevere
# Look for answers
# Trial and error (lots of this)
# Store useful code
# Intelligent copy paste

# This all = TIME. 

### Citing / referencing R packages ###########
# When reporting analyses, you need to cite the specific packages you have used

# To get information to cite the base package
citation()
#check if installed and then give citation
if(nchar(system.file(package="psych"))) citation("psych")

# For a specific package
citation(package = "psych")
citation(package = "foreign")


################  Crib sheets & R helpers ######

# Read help documentation (not always useful, examples usually good)
help(setwd)
??setwd
# For example, to find statistical tests
help.search("anova")
# Return all available functions with 'test' in the name
apropos("test")   
# Return all functions in package 'psych'.
objects(package:psych)

# R reference card
# cran.r-project.org/doc/contrib/Short-refcard.pdf

# Quick R
# www.statmethods.net/

# Cookbook for R
# www.cookbook-r.com/

# Personality project (psychology specific)
# personality-project.org/r/


################ Things to remember ###########

# Stay updated
# R and R Studio will update regularly
# Update R first, and R Studio should automatically recongise the version you have
# Some packages may not run on old versions of R

# Packages and reliability:
# The current R repository (CRAN) has ~5604 available packages
# http://cran.r-project.org/web/packages/
# (1) Packages get updated, CHANGED and removed - stay up to date
# (2) Packages may do things you don't understand - stay educated 
# (3) Packages may have bugs - stay up to date & educated & ask for help
# (4) CITE THE PACKAGES AND VERSIONS YOU USE 

# Googling a problem in R will give you discussion boards, 
# web pages and other people's solutions. 
# Remember: there are usually multiple solutions. Find one that works for you.
# The online resources and support community are HUGE
# Luckily for us, it is populated by statisticians and programmers
# and they are happy to help
# Post to a discussion board yourself if you get stuck



#######################################################################
#######################################################################



