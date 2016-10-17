###################################################
#####                                         #####
#####      Model Selection for Regression     #####
#####                                         #####
###################################################

# In linear regression, the model selection problem is
# often stated as a variable selection problem. The step
# function provides both a statistic to optimize and
# useful algorithms for examining numerous subset models.

# For selecting subsets to examine, step has three
# choices: "backward" where we start with all the
# regressors in the model and continue to remove terms
# until removing another term makes the criterion of
# interest worse; "forward" where we start with no
# regressors and continue to add terms until adding
# another term makes the criterion of interest worse;
# or "both" where at each step we consider either an
# addition or a removal. The default is "backward".

# We use the dataset 'Highway1' in the car package
# with response variable log2(rate), the base-2
# logarithm of the automobile accident rate per
# million vehicle miles on 39 segments of Minnesota
# highways in 1973.

# Install (if necessary) and load the car package
# then access the Highway1 data set:

library("car")
data(Highway1)

# To understand what these variables are:
?Highway1

# Take a look at the Highway1 data. Note that we
# will be fitting a model that uses the base-2 log
# of some of the variable measures, rather than the
# measures themselves.

# Fit the following (overfitted) linear regression model:

highway.mod <- lm(log2(rate) ~ log2(len) + log2(ADT) + log2(trks) + log2(sigs1) + slim + shld + lane + acpt + itg + lwid + hwy, data=Highway1)

# Then look at the results:
summary(highway.mod)

# Note that the predictor variable hwy is a factor
# with 4 levels so we are predicting the base-2 log
# of the automobile accident rate using 11 predictors,
# of which 10 are numeric and one is a factor.

# Note the high R-squared (.79) with few small p-
# values for the individual coefficients, a common
# symptom of overfitting an elaborate model.

# To see which variables significantly reduce
# the sums of the squared error terms with an
# analysis of variance:
anova(highway.mod)

# Basically, all of the log2's and slim are
# significant, but note that the sequential order
# of presenting these variables to the anova
# function has an impact on the output, 
# especially with so many variables. In this
# case, one cannot rely on the anova() output
# to select variables because there are so many 
# and the order affects which ones "appear" to be
# the most "important" variables. With a smaller
# number of variables, using anova() for this
# purpose has more validity and is commonly used
# this way.

# SO YOU WOULD NOT USE ANOVA IN THIS CASE AS
# A GUIDELINE TO SIMPLIFY YOUR MODEL ! ! A
# much better approach would be to utilize
# the step function.

# Take a look at the step function:
?step

# Perform stepwise regression using backward
# elimination with the following model fit.
# Note we use all the default argument values
# except we set scope = list(lower=~log2(len))
# to force log2(len) into all of the models.
# We warned ! step() has a lot of output !
highway.backward <- step(highway.mod, scope=list(lower= ~ log2(len)))

# Each segment of output shows the result of
# dropping each predictor in turn from the
# current regression model. If a predictor is
# a factor, then step will drop as a group
# all of the regressors created from the
# factor.

# Note that each step successively drops the
# predictor corresponding to the smallest value
# of AIC (Akaike Information Criterion), provided
# that there is a subset model with an AIC
# that is less than the model with all of the
# predictors, marked below <none> in the
# output.

# Can you identify which variables are
# removed in each successive step? Why was
# That variable removed? How many steps are
# there?

# What is the final highway.backward model?

# Compare the fitted, predicted values of 
# the initial overfitted model to the final
# subset model, the one selected by backward
# elimination minimizing the AIC:

plot(fitted(highway.mod) ~ fitted(highway.backward))
abline(0, 1)

# What is the correlation between the two sets
# of fitted values?:
cor(fitted(highway.mod), fitted(highway.backward))

# What can you conclude about how 'good' the
# full model and the reduced model each is
# in predicting the automobile accident rate?

# Finally, you can also directly compare the
# two fitted models provided they are nested
# and provided the belong to the same class
# (lm in this example).

# What do you think the returned results of
# this command tell you about the 'relative
# predictive capabilities' of these two models?:
anova(highway.mod,highway.backward)
