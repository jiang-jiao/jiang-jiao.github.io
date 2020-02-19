##############################
###  LAB 8.3.4 - Boosting  ###
##############################

rm(list=ls())


installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}


needed <- c("gbm", "MASS") 
# Use gbm() function to fit boosted regression trees
# MASS package contains Boston data
installIfAbsentAndLoad(needed)



# from lab 8.3.2
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
nrow(Boston)
boston.test <- Boston[-train, "medv"]
nrow(boston.test)
head(Boston)

boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
#distribution="gaussian" since this is a regression problem
#distribution="bernoulli" if it's a classfication problem
#n.trees: number of trees we want
#interaction.depth limits the depth of each tree
summary(boost.boston)  #provides relative influence plot and statistics

#rm and lstat are the most important variables
#produce partial dependence plots
par(mfrow=c(1,1))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

# use the boosted model to predict medv on the test set
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#19.37

#with shrinkage parameter
boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#18.69
#using λ = 0.2 leads to a slightly lower test MSE