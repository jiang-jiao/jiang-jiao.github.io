################################################
###  LAB 8.3.3 - Bagging and Random Forests  ###
################################################




# Start by clearing your global environment and removing any variables from your workspace.
rm(list=ls())


# Define a function that will install packages needed for lab that are missing or will load required packages if they are already installed.
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}


# 'needed' is a vector of strings that contains package names needed for this lab.
needed <- c("randomForest", "MASS")
# The MASS package contains Boston data , and the randomForest package is needed to perform random forests and bagging.

installIfAbsentAndLoad(needed)  # Call our function and pass our vector of needed package names as an argument.




##########################################
####           OVERVIEW               ####
##########################################
# Bagging#
## Can be used for Regression or Classification to reduce variance of learning method (model fit)

## Known as Bootstrap Aggregation --> The Bootstrap method resamples data with replacement (see page 187)
## 1) Take different samples from same training data set.
## 2) Train learning method on bootstrapped training sets 
## 3) Average all your predictions (regression) or find class predicted that is majority (classification)
## Note: m = p, where m is number of predictors selected at each split and p is total number of predictors.


# Random Forests #
# Strong predictors cause predictions from bagged trees to be highly correlated. 
# To prevent this, randomly select m predictors at each decision tree split (criteria for how you partition data set) 
# m = sqrt(p)  where p is # of predictors


# Goals of Lab:
# 1) Practice using random forest and bagging methods on actual data set in R
# 2) Compare results from methods
# 3) Become confident coding Tree-Based Methods


set.seed(1) # sets the seed for R's random number generator (makes results reproducible)


# Our data set is a dataframe of housing values in Boston Suburbs.

Boston

train <- sample(1:nrow(Boston), nrow(Boston)/2) # sample(data from which to choose, number of items to choose)
                                                # sample(x, size, replace = FALSE, prob = NULL

boston.test <- Boston[-train, "medv"] # We're trying to assess model performance at predicting medv(median value of owner-occupied homes in \$1000s.)
                                      



# We use the randomForest function for bagging as well. Only difference is what value m or mtry is. (Bagging m = p)
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE) 
bag.boston

#(formula describing model fitted, 
#data = data frame containing variables in model, 
#subset = which rows to be used,
#mtry = 13 is # of predictors randomly sampled at each split,
#importance = assesses importance of predictors)



# predict will determine values for our response variable based on our model fit.
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])  # predict(model fit used, newdata = all values from original data set not in train set)
plot(yhat.bag, boston.test)  # Shows relationship of yhat.bag (predictions using bagging) against boston test set
abline(0,1)
mean((yhat.bag-boston.test)^2)  # Calculates the Mean Squared Error for Test Set


#How do we change the number of trees to grow?  
# We specify ntree = # of trees


bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# What happens if we change the number of trees? 

# Can improve your model, but after a while will increase computation time.
# Ideally we want enough trees to predict all of our observations


# Default uses p/3  for regression trees and sqrt(p) for classification trees.
# Default shows mtry = 4 --> 13/3 is approximately equal to 4
rf.boston <- randomForest(medv~.,data=Boston,subset=train,importance=TRUE)
rf.boston$mtry

# Book changed mtry or m = 13 to mtry = 6 for our random forest (appears arbitrary)
rf.boston <- randomForest(medv~.,data=Boston,subset=train, mtry= 6, importance=TRUE)
yhat.rf <- predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)  # Random Forest MSE is lower than Bagging MSE --> yielded improvement

# View importance of each variable
importance(rf.boston)

# %IncMSE is based upon the mean decrease of accuracy in predictions on out of bag samples when given variables are not part of model
# Note: Each bagged tree uses around 2/3 of observations. Remaining 1/3 not used to fit a tree are called out-of-bag (OOB) observations.

#IncNodePurity is the total decrease in node impurity from decision splits over each specified predictor, averaged over all trees.


# Visualizes measures from importance function (%IncMSE and IncNodePurity) for each variable
varImpPlot(rf.boston)

#Note: higher %IncMSE means variable is more important
# rm and lstat are most important variables

