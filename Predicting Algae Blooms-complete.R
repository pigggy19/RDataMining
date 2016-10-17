##############################################
#####     PREDICTING ALGAE BLOOMS        #####
##############################################

# This case study will introduce you to some 
# basic tasks of data mining: data pre-processing
# exploratory data analysis, and predictive model
# construction.

# Is a small data mining problem: we are predicting
# the frequency occurrence of several harmful
# algae in water samples.

# High concentrations of certain harmful algae
# in rivers constitute a serious ecological
# problem that impacts not only river lifeforms,
# but also water quality. We want to be able to
# monitor and forecast algae blooms.

# With the goal of addressing this prediction 
# problem, several water samples were collected
# in different European rivers at different 
# times during a period of approximately 1 year.

# For each water sample, different chemical 
# properties were measured as well as the 
# frequency of occurrence of seven harmful algae.

# Some other characteristics of the water 
# collection process were also stored, such
# as the season of the year, the river size, 
# and the river speed.

### Datasets Described

# There are two main datasets for this problem. 

# The 'Real' Dataset
# The first consists of data for 200 water
# samples. To be more precise, each observation 
# in the available datasets is an aggregation
# of several water samples collected from the
# river over a period of 3 months, during the 
# same season of the year.

# Each observation contains information on 11 
# variables. Three of these variables are nominal
# and describe the season of the year when the 
# water samples were collected, and the size and 
# speed of the river in question. The eight
# remaining variables are values of different 
# chemical parameters measured in the water 
# samples forming the aggregation, namely:

# . Maximum pH value
# . Minimum value of O2 (oxygen)
# . Mean value of Cl (chloride)
# . Mean value of NO3 (nitrates)
# . Mean value of NH4(ammonium)
# . Mean of PO3 (orthophosphate)
# . Mean of total PO4 (phosphate)
# . Mean of chlorophyll

# Associated with each of these parameters are 
# seven frequency numbers of different harmful
# algae found in the respective water samples.

# The Test dataset
# The second dataset contains information on 
# 140 extra observations. It uses the same basic
# structure but it does not include information
# concerning the seven harmful algae frequencies.

# These extra observations can be regarded as
# a kind of test set.

#########################################
### Loading the Data into R
#########################################

# may need to install package
# install.packages("DMwR")

library("DMwR")

# To view the first six records:
data(algae)
head(algae)
names(algae)
ncol(algae)
nrow(algae)

# Is a dataframe
# alternatively, can read it in from disk.
# Check your current default directory:
getwd()
# We make sure it is there:
file.exists("Analysis.txt")

# Read it in under a different name
algae.2nd <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))

head(algae.2nd)

# can also do this, watch for user action prompt
dataset <- file.choose()
algae.3rd <- read.table(dataset,
                        header=F,
                        dec='.',
                        col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                    'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                    'a5','a6','a7'),
                        na.strings=c('XXXXXXX'))

###############################################
### Data Visualization and Summarization
###############################################

# Is always a good idea to visualize the
# statistical properties of the data.

# what is in algae? Season,
# size and speed are factors.
str(algae)

# summary() provides descriptive statistics.
# For the nominal variables (factors), summary()
# provides frequency counts for each possible
# value.
summary(algae)

# We see there are more water samples collected
# in winter than in the other seasons, that
# there are more medium sizes and fewest
# low speeds.

# For numeric variables, we have mean, median,
# quartiles and extreme values, is the first
# look at the distributional properties.

# Call a histogram of variable mxPH.
# prob = T gives probabilities along y axis:
hist(algae$mxPH, prob=T)

# looks nearly normal, but can get a more precise
# check with QQ plot. Use car package (John Fox)

# use car package
library(car)
# set multiframe, multirow value to par
par(mfrow=c(1,2))
# more detailed histogram
hist(algae$mxPH,
     prob=T,
     xlab='',
     main='Histogram of maximum pH value',
     ylim=0:1)
# add line in form of density plot
lines(density(algae$mxPH,na.rm=T))
# add a rug plot and jittering
rug(jitter(algae$mxPH))
# QQ plot
qqPlot(algae$mxPH,
        main='Normal QQ plot of maximum pH')
# reset par
par(mfrow=c(1,1))

# Note extensive use of function composition
# in previous example, calling several functions
# with the result of other functions (also known
# as "nested functions')

# We call the boxplot() function
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
# side=2 puts rug lines on y axis:
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)

# Box plots show key properties of a
# variable's distribution: 1st, 3rd quartiles,
# median.

# Also, small horizontal dash above the box
# is the largest observation that is less than
# or equal to the 3rd quartile plus 1.5 times
# the inter-quartile range.

# Can see that presence of outliers has
# distorted the mean relative to the median.

# Can see that variable oP04 has a distribution
# of the observed values concentrated on the low
# end, and with a positive skew.

### Outliers
# We look at two ways to visualize outliers
plot(algae$NH4,xlab='')
# draws line with mean:
abline(h=mean(algae$NH4,na.rm=T),lty=1)
# draws line with mean plus sd:
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)
# draws line with median:
abline(h=median(algae$NH4,na.rm=T),lty=3)
# puts R in interactive mode, click on a
# dot in graphic frame and shows row number:
identify(algae$NH4)

# Hit escape (in RStudio) to end the
# interactive mode, then it returns the row
# numbers of all of the points closest to
# where you clicked the graph.

# If we want to inspect the respective observa-
# tions we can:
plot(algae$NH4,xlab='')
# returns number of lines corresponding to
# the clicked points in the graph (note that
# number of lines returned in the R console
# AFTER you hit escape)
clicked.lines <- identify(algae$NH4)
algae[clicked.lines,]

# Can accomplish same identification
# non-graphically by, for example:
algae[!is.na(algae$NH4) & algae$NH4 > 19000,]

# Need !is.na() function call since some
# observations hold missing values which would
# otherwise cause an error for the comparison
# condition algae$NH4 > 19000

# Call to !is.na(algae$NH4) returns a vector
# of boolean values that are FALSE for missing.

# What if we want to know about a distribution,
# but also what variables it depend on? This is
# sometimes called a conditioned plot, where
# the conditioning variable is often a factor.

# A factor is a nominal variable with a set of
# finite values

# We call lattice
library(lattice)
# We want a box plot for each value of nominal
# variable a1 size (small, medium, large).
# Note first argument of bwplot() is a formula:
print(bwplot(size ~ a1, 
             data=algae,
             ylab='River Size',
             xlab='Algal A1'))
# So have higher frequencies of algal a1
# in smaller rivers.

# Variant are box-percentile plots:

library(Hmisc)
print(bwplot(size ~ a1, 
             data=algae,
             panel=panel.bpplot, 
             probs=seq(.01,.49,by=.01), 
             datadensity=TRUE,
             ylab='River Size',
             xlab='Algal A1'))

# dots are mean values. Vertical lines are
# 1st quartile, median and 3rd quartile. Dashes
# show actual values.

# Get more information, can see that that value
# of observed frequencies for small rivers is
# much more widespread across the domain of
# frequencies than for other types of rivers.

# This type of conditioning plot not restricted 
# to nominal variables, nor to a single factor.
# Can use continuous variables if you
# "discretize" them first:

# Variable mn02 is named 'minO2'
# Make continuous mn02 a factorized 'shingle'
minO2 <- equal.count(na.omit(algae$mnO2),
                     number=4,overlap=1/5)

# Note 20% overlapping intervals of shingle.
# The 'bins' are created such that they contain
# an equal number of observations
minO2

# Condition by season AND mn02 (continuous).
# Bins are ordered left-to-right and from
# bottom up. Note we have eliminated missing
# mnO2 values with na.omit(algae$mnO2)
stripplot(season ~ a3|minO2,
          data=algae[!is.na(algae$mnO2),])


####################################
### Unknown Values
####################################

# We have water samples with unknown values in some
# variables. So how do you deal with them?

# Common strategies are:

# Remove cases with unknowns.

# Fill in unknown values by exploring correlations
# between variables.

# Fill in unknown values by exploring the similarity
# between cases.

# Use tools that can accomodate missing values.

#############################################
# Removing Observations with Unknown Values #
#############################################

library(DMwR)
data(algae)

# Before you do anything, it is a good idea to
# to count them:

# this returns incomplete cases.
# uses boolean values, take a look.
algae[!complete.cases(algae),]

# this counts them, are 16 now
nrow(algae[!complete.cases(algae),])

# this omits them and writes over algae
algae <- na.omit(algae)

# filling them in (imputation) is unreliable. At
# a minimum we will delete some. We observe that
# 62 and 199 have 6 of 11 explanatory variables
# missing, take a look
algae[c(62,199),]

# Let's reload the original dataset
data(algae)

# apply family are 'meta-functions'...they can
# use another function against elements. Here we
# apply a function 'to the margin' which is rows
# here (2 is columns). This is an anonymous
# (temporary) function:
apply(algae,1,function(x) sum(is.na(x)))

# so we get rid of them:
algae <- algae[-c(62,199),]

# Use apply() again
apply(algae,1,function(x) sum(is.na(x)))

# When we call the data() function we restore
# the dataset
data(algae)

?manyNAs

# when a row is missing 20% it is flagged:
manyNAs(algae)

# this also eliminates them:
algae <- algae[-manyNAs(algae),]

# Let's see
apply(algae,1,function(x) sum(is.na(x)))

####################################################
### Filling in Unknowns with Most Frequent Values ##
####################################################

# An alternative to eliminating cases with unknown
# values is to try to find the most probable value
# for each of these unknowns.

# There are several strategies for doing this.

# Use Some Statistic of Centrality: Simplest
# and Fastest Way

# Can use mean, median, mode. Choice depends on
# distribution of the variable

# With approximately normal distributions, where
# all observations are clustered around the mean,
# this is a good choice

# For skewed distributions, or for variables with
# outliers, mean is not a good choice.

# For instance sample algae[48,] does not have a 
# value in the variable mxPH. As distribution is
# nearly normal (we produced a normal QQ plot 
# earlier) we could use its mean value to fill
# in the "hole":
summary(algae$mxPH)
algae[48,]
algae[48,'mxPH'] <- mean(algae$mxPH,na.rm=T)

# Usually, tho, are more interested in filling in
# all of the unknowns of a column instead of a case
# at a time. Look at variable Chla. It is unknown
# on 12 water samples. Mean would be a poor value
# to impute as Chla is skewed to lower values.
summary(algae$Chla)
# So we use median:
algae[is.na(algae$Chla),'Chla'] <- median(algae$Chla,na.rm=T)
?centralImputation
# The is a function centralImputation() in the book
# package which fills in all unknowns in a dataset
# using a statistic of centrality, using the
# median for numeric columns and the mode (most
# frequent value) for nominal variables:
data(algae)
algae <- algae[-manyNAs(algae),]
algae <- centralImputation(algae)

# However, these simple strategies, while seeming
# efficient are not a good idea. They can introduce
# a large bias. However, unbiased methods that find
# the optimal value to fill in an unknown are very
# complex and may be inadequate for many data mining
# problems.

#####################################################
# Filling in Unknown Values by Exploring Correlations
#####################################################

# An alternative for getting less biased estimates
# of the unknown values is to explore the relation-
# ships between variables. For example, using the
# correlation between the variable values, we could
# discover that a certain variable is highly
# correlated with mxPH so we could obtain other,
# more probable values for the sample variable 48.

# To obtain the variables correlation we derive
# correlation matrix on complete cases:
cor(algae[,4:18],use="complete.obs")

# We can make it more readable by symbolically
# encoding it with symnum()
?symnum

# This helps a lot, particularly for large 
# correlation matrices
symnum(cor(algae[,4:18],use="complete.obs"))

# In this data, the correlations are for the most
# part irrelevant with two exceptions:
# between variables NH4 and NO3 (0.72), and
# between variables PO4 and oPO4 (0.9).

# With high correlations like this, it is reasonable
# to fill them in with unknowns. However, if you
# have removed samples 62 and 199 there will be
# no water samples with unknown values on NH4 and
# NO3 so you cannot use these two directly. The
# correlation between PO4 and oPO4 warrant the use
# of a regression approach to fill in the missing
# values for one or the other.
?manyNAs
# So knowing this correlation, we can fill in the
# unknowns using a linear model

data(algae)
# manyNAs() scrapes away missing > 20%
algae <- algae[-manyNAs(algae),]
summary(lm(PO4 ~ oPO4,data=algae))

# Can fill in unknowns knowing this relationship
# as long as both are not unknown

# Is only one (sample 28) left with an unknown
# value on variable PO4, so we estimate

algae[28,'PO4'] <- 42.897 + 1.293 * algae[28,'oPO4']

# That is easy....however, let's assume that there
# are several samples with unknown values of PO4.
# How could we fill in missing values efficiently?
# Create a function that returns the value of PO4
# given a value of oPO4, and then apply this function
# to all unknown values:

data(algae)
algae <- algae[-manyNAs(algae),]
# This function has one argument assumed to be the
# value of oPO4. Returns NA if missing, otherwise
# 42.897 + 1.293 * oP
fillPO4 <- function(oP) {
   if (is.na(oP)) return(NA)
   else return(42.897 + 1.293 * oP)
}

# Given a value of oPO4, function returns value
# of PO4 according to discovered linear relation.
fillPO4(6.5) # return 51.3015

# sapply runs it down the vector of oPO4 values
# sapply is another meta-function. sapply() returns
# a vector of same length as oPO4. is.na() indexes
# the rows
algae[is.na(algae$PO4),'PO4'] <- 
    sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)

# Still there are several observations left with
# unknown values. We explore the correlation between
# the unknown variables and the nominal variable
# with a conditioned histogram. Is a histogram of
# variable mxPH conditioned by season:
histogram(~ mxPH | season,data=algae)

# You can change the ordering of the seasons
# if you want to before you call above histogram:
(algae$season<-factor(algae$season,
                     levels=c('spring',
                              'summer',
                              'autumn',
                              'winter')))

# is different, but the histograms are not so
# dissimilar from each other that we might
# conclude the season when samples were collected
# changes the values of mxPH
histogram(~ mxPH | season,data=algae)

# Here we just condition based on the size
# of the river, we see smaller rivers show lower
# values of mxPH:
histogram(~ mxPH | size,data=algae)

# We call histogram again, this time
# conditioned by size times speed and empty
# panel is sample 48
histogram(~ mxPH | size*speed,data=algae) 

# stripplot shows similar information but with
# the concrete values of mxPH
stripplot(size ~ mxPH | speed, data=algae, jitter=T)

# We could continue this process for the other
# variables with missing values but it is very
# tedious. However it can be a useful method when
# applied to small datasets with few nominal
# variables.

## Filling in Unknown Values by Exploring
## Similarities between Cases

# Instead of looking at correlation between columns
# (the variables), we can try to use the similarities
# between the rows to fill in unknown values

# Put data back the way it was by calling data()
data(algae)
algae <- algae[-manyNAs(algae),]

# The approach here assumes that if two water 
# samples are similar, and one of them has an
# unknown value in a variable, it is probable
# that this value is similar to the value of the
# other sample.

# Use distance measure for 'similarity', Euclidean
# distance over the multivariate variables.

?knnImputation
# We find ten most similar cases of any water sample
# with some unknown value in a variable, and then
# use their values to fill in the unknown using
# (1) mode or (2) weighted average of 10 neighbors
# values, with weights decreasing as distance 
# increases.
algae <- knnImputation(algae,k=10)

# Can also use median instead of mode
algae <- knnImputation(algae,k=10,meth='median')

# So now we have a dataframe free from NA values.

#########################################
###    Obtaining Prediction Models    ###
#########################################

# Main goal of this case study is to obtain
# predictions for frequency values of seven
# algae in 140 water samples. Since frequencies
# are numbers, we use a regression task.

# We could use such a model to:

# 1) Predict value of target variable for future
# observations of explanatory variables; or
# 2) to better understand the interactions among
# the variables in our problem domain.

# We explore two different predictive models:
# 1) multiple linear regression; and
# 2) regression trees.

# These models are good alternatives for regression
# because they entail very different assumptions
# regarding the "shape" of the regression function
# being approximated, they are easy to interpret, and
# they are easy to run on any computer.

# These two models handle missing values in different
# ways.

# Linear regression cannot handle datasets with
# unknown values, while regression trees handle such
# values quite naturally.

#########################################
###    Multiple Linear Regression     ###
#########################################

# These models obtain an additive function relating
# a target variable to a set of predictor variables.

# This additive function is a sum of terms of the
# form Bi x Xi where Xi is a predictor variable
# and Bi is a number.

# We load the data again to ensure it
# is "back the way it was":
data(algae)

# We get rid of the rows that have more
# than 20% missing (rows 62 and 199):
algae <- algae[-manyNAs(algae), ]

# We use knnImputation() to fill remaining
# missing values with 10 "most similar" records
clean.algae <- knnImputation(algae, k = 10)

?lm

# Now there are no missing values so we fit
# a linear regression model for predicting
# one of the algae:
lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12])

# lm.a1 is an object returned by lm(). We call
# generic functions summary() for results. For
# any categorical variable with k levels, R
# creates k-1 dummy variables. Formula means
# predict a1 based on all variables, columns
# 1 through 12
summary(lm.a1)

# Fit of model (adjusted R-square) is not
# terrific (32.2%)

# can check diagnostics with
plot(lm.a1)
# lots of problems...heteroscedascity, non-normal
# distribution in QQ, looks quadratic, and
# observation 153 has too much leverage.

# is easier to interpret the results calling
# analysis of variance...most predictors are
# significant but we note the assumption
# violations.

# anova() gives us sequential analysis of 
# variance of model fit, that is, how much
# does each sequential variable reduce the
# residual sums of squares as more terms
# are added to the model.
anova(lm.a1)

# We can get rid of season, has least
# contribution to reduction of sums of
# squares. Note use of update() where
# we just indicate change to the model:
lm2.a1 <- update(lm.a1, . ~ . - season)

# look at results:
summary(lm2.a1)

# Fit has improved a bit. Adjusted R-Square
# is 32.8%

# We can compare fit of nested models
anova(lm.a1,lm2.a1)

# They are not significantly different in
# their ability to predict, that is, the 
# increase in the adjusted sums of squares
# is not significant. Generally, this indi-
# cates that the simpler model is the best
# alternative (if there is no difference).

# we could apply 
anova(lm2.a1)
# to see if we can eliminate more variables
# one at a time, but R has a function that
# performs this backward elimination process:

# step() performs the backward elimination
# process to our original model fit. It uses
# Akaike Information Criterion to perform
# model search, systematically reducing AIC:
final.lm <- step(lm.a1)

# We obtain summary info for final.lm:
summary(final.lm)
anova(lm.a1,final.lm)
# R-squared numbers still not very large.
# The is consistent with the observed 
# violations in the assumptions earlier.

#########################################
### Regression Trees as Model
#########################################

# We now use regression trees to predict
# value of frequencies of algae a1.

# Regression trees are tolerant of missing
# values but still need to remove obs 62 and
# 199

# We need library rpart (acronym for Recursive
# Partitioning And Regression Trees)
library(rpart)

# We reload data set:
data(algae)

# Trim away record 62 and 199 only:
algae <- algae[-manyNAs(algae), ]

# fit the tree, note same function form as lm()
# tree estimating is simulation, to get consistent
# results, need to set.seed
set.seed(1234)
rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])

?rpart

# RT is hierarchy of logical tests on most
# relevant explanatory variables. But unless
# set seed, will get varying results

# We see root node has 198 samples, average
# value of a1 is 16.99 and deviance from this
# average is 90401.29

rt.a1

# Are two branches on each node.

# we examine content of object rt.a1 
# which is the regression tree. Each node is
# related to the outcome of a test on one of
# the predictor variables. At each node, shows
# average value for a1 and the variance.
# We continue testing until we reach a leaf
# node (marked with asterisk) and there we find
# predictions for the levels of a1

# To plot without calling plot() and text()
# over and over
prettyTree(rt.a1)

# summary() also works on rpart objects and produces
# quite a bit of information concerning the tests
# on the tree, the alternative tests that could
# be considered, and also the 'surrogate splits'
# which refer to alternative strategies to handle
# unknown values.
summary(rt.a1)

# Trees are usually obtained in two steps.
# 1) Grow a large, bushy tree and then
# 2) Prune this tree by deleting bottom nodes
# through a process of statistical elimination.

# Very large tree is overfitted; it fits the
# training data almost perfectly but may perform
# badly with a new data set used to obtain new
# predictions.

# rpart() simply grows the large tree, stopping
# when certain criteria are met: 1) decrease in
# deviance goes below a certain threshold (cp,
# default is 0.01);
# 2) number of samples in a node is less than
# some threshold (minsplit, defaults is 20); 
# 3) tree depth exceeds some threshold (maxdepth,
# default is 30).

# Then we prune the tree, usually based on cost
# complexity. The method uses the values of the
# parameter cp on each node. Use value of cp
# giving best compromise between predictive
# accuracy and tree size.

# Given a tree obtained with rpart, can estimate
# predictive performance of subtrees using
# printcp(), was also in summary() results:

printcp(rt.a1)

# Last tree produced has cp values of 0.01 after
# nine tests and a relative error (compared to
# root node of 0.354). Using an internal process
# of ten-fold cross-validation that this tree wil
# have an average relative error of 0.75211 +
# or - 0.11483.

# An alternative strategy is to choose tree with
# lowest cross-validation error (xerror). This
# is tree #3. It is usually also the smallest 
# tree with the fewest tests (nodes).

# We use the prune function to arrive at this
# tree

rt2.a1 <- prune(rt.a1,cp=0.08)
rt2.a1

# We see that best prediction is to look at PO4
# only. If PO4>=43.818, prediction of a1 is
# 8.979592; if PO4<43.818, prediction is 40.10392.

# Can do both in one step with rpartXse() which
# takes se as argument (default is se=1.0)

set.seed(1234) # Just to ensure  same results as in the book
(rt.a1 <- rpartXse(a1 ~ .,data=algae[,1:12]))

# R also allows interactive pruning of tree
# with function snip.part() which generates
# a pruned tree in two ways, where you can
# indicate the number of desired nodes at which
# you want to prune the tree (can print a tree
# object first to look at resulting numbers at
# each node):

first.tree <- rpart(a1 ~ .,data=algae[,1:12])
my.tree <- snip.rpart(first.tree,c(4,7))
my.tree

# Secondly, can use snip.part() is a graphical
# way....first plot the tree, then call the
# function without second argument.

# If you click on a node, R prints information
# about the node. If you click again, R prunes 
# the tree at that node. You escape by clicking
# the escape button
prettyTree(first.tree)
snip.rpart(first.tree)

###################################################
### Model Evaluation and Selection
###################################################

# So which one should we use? Multiple regression
# model or regression trees? Need to specify
# preference criteria over the space of models.

# Are many criteria for evaluating models. Some
# of the most popular are criteria that calculate
# the predictive performance of the models.

# Assess predictive performance of regression
# models by comparing model predictions with
# real values of target variables and then
# calculating some average error measure.

# We will use Mean Absolute Error (MAE) for both
# multiple regression and regression tree models.

# First step is to obtain model predictions for 
# set of cases over which we want to evaluate.

# Generic R function for this is predict(), takes
# a model and test data set as arguments and
# returns correspondent model predictions.

# get predictions from final regression model, we
# use clean.algae because of missing values:
lm.predictions.a1 <- predict(final.lm,clean.algae)

# get predictions for final regression tree model:
rt.predictions.a1 <- predict(rt.a1,algae)

# We calculate Mean Absolute Errors:
(mae.a1.lm <- mean(abs(lm.predictions.a1-algae[,'a1'])))
(mae.a1.rt <- mean(abs(rt.predictions.a1-algae[,'a1'])))

# Another popular error is Mean Squared Error:
(mse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2))
(mse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2))

# MSE is not measured in same units as target
# variable so interpretation is more difficult.

# So we use normalized mean squared error which
# calculates a ratio between the performance of
# the models and that of a baseline predictor,
# usually the mean value of the target variable:

# Calculate NMSE for regression model:
(nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))

# Calculate NMSE for regression tree model:
(nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))

# NMSE is a unit-less error measure with values
# usually ranging from zero to one. If the model
# performs better than very simple baseline (mean),
# NMSE should be less than one, and the smaller,
# the better. Values greater than one means the
# model predicts worse than the simple (mean)
# baseline measure.

# regr.eval() function calculates the value of a 
# set of regression evaluation metrics:
regr.eval(algae[,'a1'],rt.predictions.a1,
          train.y=algae[,'a1'])

# We want to visually inspect model predictions.

# This is a common R theme and practice, to
# inspect data before and then performance
# evaluation measures after.

# We go with a scatterplot of the errors.
# Reset graphics parameters to draw two plots
# side-by-side in one frame:
old.par <- par(mfrow=c(1,2))
# Call the scatterplot (is a high-level function)
plot(lm.predictions.a1,
     algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")
# Draw line (low-level base graphics function)
abline(0,1,lty=2)
# Call second plot (high-level graphics function)
plot(rt.predictions.a1,algae[,'a1'],main="Regression Tree",
     xlab="Predictions",ylab="True Values")
# Draws another line on the currently active plot:
abline(0,1,lty=2)
# resets the graphics parameters back to default:
par(old.par)

# Looking at the resulting plots we observe that the
# models have poor performance in several cases.
# Ideally, all circles lie on the dashed line which
# cross the origin and represent where X = Y.

# We can determine the sample numbers for the bad
# predictions with identify():

plot(lm.predictions.a1,algae[,'a1'],
     main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
algae[identify(lm.predictions.a1,algae[,'a1']),]

# So you identify the clicked circles by their rows
# which are then visible as  they are in a vector
# returned by identify() to index the algae dataframe.

# We see that some of the linear model predictions
# are negative algae frequencies. This does not make
# sense so we use this 'domain knowledge' and the
# minimum possible value of 0 algae frequency to
# improve the linear model performance, replacing the
# negative predictions with 0 predictions.

# Create new model 'sensible....':
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0,0,lm.predictions.a1)

# The previous model gave us:
regr.eval(algae[,'a1'],
          lm.predictions.a1,
          stats=c('mae','mse'))

# The sensible model gives us an improvement:
regr.eval(algae[,'a1'],
          sensible.lm.predictions.a1,
          stats=c('mae','mse'))

### k-fold CV

# We want to choose the best model for obtaining the
# predictions on the 140 test samples. As we do not
# know the target variables values for those samples,
# we have to estimate which of our models will perform
# better on these test samples.

# Our key issue here is to obtain a reliable estimate
# of model performance on data for which we do not
# know the true target value. You can overfit on
# training data, and predict that data perfectly, but
# that model will not necessarily generalize to new
# data.

# How do you achieve a reliable estimate of a model's
# performance on unseen data?

# k-fold Cross Validation is often used to obtain
# reliable estimates using small datasets: Obtain
# k equally-sized and random subsets of the training
# data. For each of the k subsets, build a model
# using the remaining k-1 sets and evaluate this
# model on the kth set. Store the performance of the
# model and repeat this process for all remaining
# subsets.

# In the end, we have k performance measures, all
# obtained by testing a model on data not used in
# the construction of the model.

# The k-fold Cross Validation estimate is the average
# of these k measures. Often k is set as = 10. Further,
# this overall k-fold CV process can be repeated to
# improve the reliability of the performance estimates.

### experimentalComparison() function in DMwR package

# In general, when faced with a predictive task, we
# 1) Select alternative models to consider;
# 2) Select the evaluation metrics which will be
#    used to compare the models; and
# 3) Choose among available experimental methodologies
#    for obtaining reliable estimates of these metrics.

# DMwR package has experimentalComparison() function.
# Result of the function is an object of class compExp

?experimentalComparison

class?compExp

# experimentalComparison() has 3 parameters: (1) data
# set to use; (2) alternative models; and (3) choice
# of experimental methodology for obtaining reliable
# performance evaluation metrics estimates.

# experimentalComparison() is generic in that it can
# be used for any model(s) and any dataset(s). The
# user supplies a set of functions implementing the
# models to be compared. Each function should imple-
# ment a full train+test+evaluate cycle for the
# given training and test datasets. The functions are
# called from the experimental routines on each
# iteration of the estimation process. They should
# return a vector with the values of whatever
# evaluation metrics the user wants to estimate by
# cross-validation.

# Here we construct such functions for two target
# models:

# user-defined cross validation function
# for regression tree model. The arguments are
# the formula, the training data, and the test data.
cv.rpart <- function(form,train,test,...) {
  # train:
  m <- rpartXse(form,train,...)
  # test:
  p <- predict(m,test)
  # evaluate:
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

# user-defined cross validation function
# for linear regression model. The arguments are
# the formula, the training data, and the test data.
cv.lm <- function(form,train,test,...) {
  # train:
  m <- lm(form,train,...)
  # test:
  p <- predict(m,test)
  # add our rule:
  p <- ifelse(p < 0,0,p)
  # evaluate:
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

# Note we assume the use of NMSE as evaluation metric.
# for both regression trees and linear models.

# Both functions carry out a train+test+evaluate
# cycle akthough each uses a different learning algo-
# rithm. Both return a named vector with the score
# in terms of NMSE.

# The '...' (triple dot argument) allows additional
# (variable number) of arguments to be passed to the
# function after the first three which are specified
# by name. So one may pass extra learning parameters
# to the learning function (to rpartXse() in one case
# and to lm() in another). The resp() function from
# DMwR obtains target variable values of a data set
# given a formula:

?resp

# Having defined our functions that will carry out
# the learning and testing phase of our models, we
# then carry out the cross-validation comparison:

res <- experimentalComparison(
  # first argument is vector of data sets in form
  # dataset(<formula>,<data frame>,<label>)
            c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
  # second argument is vector of learning system variants
  # with name of functions to carry out learn+test+evaluate cycle
            c(variants('cv.lm'), 
              variants('cv.rpart',
  # we allow different values for se:
                       se=c(0,0.5,1))),
  # third argument specifies 3 reps of k-fold cv process,
  # that k=10, and 1234 is random seed:
            cvSettings(3,10,1234))

# variants() function generates a set of alternative
# models resulting from all possible combinations of
# the parameter values. We use cv.lm() only with its
# default parameters and for cv.rpart() we specify
# different alternative values for parameter se. So
# we get three variants of regression trees. See info
# about third argument above.

# Result is complex object with all information
# about the experimental comparison

summary(res)

# We note the plots below shows that one of the
# variants of the regression tree achieves the
# best results.

# To visualize:
plot(res)

# experimental Comparison() assigns a label to 
# each model variant. If you want to know specific
# parameter settings for any one label:

getVariant('cv.rpart.v1',res)

# We can execute a similar comparative experiment
# for all seven prediction tasks with sapply():

# starts by creating vector of datasets to use
# for seven prediction tasks
DSs <- sapply(names(clean.algae)[12:18],
         function(x,names.attrs) { 
           # create a separate formula for each
           f <- as.formula(paste(x,"~ ."))
           dataset(f,clean.algae[,c(names.attrs,x)],x) 
         },
         names(clean.algae)[1:11])

# Now we use vector of datasets created above
res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',
                             se=c(0,0.5,1))
                   ),
        # carry out 5 reps of 10-fold CV
                  cvSettings(5,10,1234))

summary(res.all)

# Visualize results, we have some very bad results
# When NMSE > 1, that is baseline as competitive as
# predicting always the average target variable
# value for all test cases.
plot(res.all)

# To check which is the best model, we use the
# function bestScores()
?bestScores

bestScores(res.all)

# We see that except for algae 1, results are
# disappointing. There is so much variability (see
# previous plot) that a random forest ensemble
# approach might be a better candidate.

# Ensemble approaches generate a large set of
# alternative models and combine their predictions.

# We can generate a random forest of trees (too
# much to cover here but let's look at the results)

library(randomForest)
cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',
                             se=c(0,0.5,1)),
                    variants('cv.rf',
                             ntree=c(200,500,700))
                   ),
                  cvSettings(5,10,1234))

# We see the advantages on an ensemble approach

bestScores(res.all)

# In all cases except for algae 7, the best score
# is obtained by some variant of a random forest.
# But some results are still not very good.

# But how tell if the difference between the scores
# of the best models and the remaining alternatives
# is statistically significant?

?compAnalysis # gives us Wilcoxon tests (non-parametric)

compAnalysis(res.all,against='cv.rf.v3',
               datasets=c('a1','a2','a4','a6'))

############################################
### Predictions for the seven algae
############################################

# We need to obtain predictions for the seven algae
# on the 140 test samples. We will use the model 
# that our cross validation indicated as "best"
# in our call to the bestScores() function, either
# "cv.rf.v3", "cv.rf.v2", "cv.rf.v1" or "cv.rpart.v3".

# We begin by obtaining these models using all of
# the available training data so we can apply them 
# to the test set.

# Regression trees incorporate their own method for
# handling missing values. Random forests do not, so
# we use the clean.algae dataframe.

# Here we obtain all seven models:

# Obtain vector with names of winning variants:
bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse',
                                        'system'])

learners <- c(rf='randomForest',rpart='rpartXse') 

# Obtain names of R functions that
# learn these variants using strsplit():
funcs <- learners[sapply(strsplit(bestModelsNames,'\\.'),
                        function(x) x[2])]
# store parameter setting for each of winning
# variants, getVariant() function gives the model
# corresponding to a variant name:
parSetts <- lapply(bestModelsNames,
                   # 'pars' is a slot, a list with
                   # parameters of the variant:
                   function(x) getVariant(x,res.all)@pars)

# obtain models and stores them in bestModels list:
bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a],'~ .'))
  # do.call() allows us to call any function by
  # providing its name as a string on the first
  # argument, and then including the arguments of
  bestModels[[a]] <- do.call(funcs[a],
  # the call as a list in the second argument:
          c(list(form,clean.algae[,c(1:11,11+a)]),parSetts[[a]]))
}

# So now we have a list with seven models obtained
# for each algae and can use for making predictions
# for the test set.

# clean up missing values for random forests.
# df 'test.algae' contains 140 test samples:
clean.test.algae <- knnImputation(test.algae,k=10,distData=algae[,1:11])

# set up matrix to hold the predictions, all NAs:
preds <- matrix(ncol=7,nrow=140)
# obtain the predictions, store each one as one of
# the seven rows in the matrix 'preds'
for(i in 1:nrow(clean.test.algae)) 
  preds[i,] <- sapply(1:7,
                      function(x) 
                        predict(bestModels[[x]],clean.test.algae[i,])
                     )

# obtain average predictions 'on the margin' of
# columns
avg.preds <- apply(algae[,12:18],2,mean)
# We compare the predictions with the true values
# as QA on our approach to this prediction problem.
# 'True' values are in 'algae.sols' df. scale()
# function normalizes a dataset, it subtracts the
# second argument by the first and divides the
# result by the third argument, unless FALSE (as here).
# We subtract a vector, the average target value,
# from each line of the matrix
apply( ((algae.sols-preds)^2),2,mean) / apply( (scale(algae.sols,avg.preds,F)^2),2,mean)

# Average value of target variable is our prediction
# of the baseline model used to calculate the NMSE,
# which in our case consists of predicting the
# average value of the target variable.

# Then we calculate NMSEs for the seven models/algae.
# Our results are similar to the cv estimates
# previously obtained.

# Is difficult to obtain good scores for algae a7,
# much better with algae a1.

##################   FINI   ######################
