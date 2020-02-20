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


needed <- c("randomForest", "MASS")
installIfAbsentAndLoad(needed)  # Call our function and pass our vector of needed package names as an argument.

set.seed(1) # sets the seed for R's random number generator (makes results reproducible)

train <- sample(1:nrow(Boston), nrow(Boston)/2) # sample(data from which to choose, number of items to choose)
# sample(x, size, replace = FALSE, prob = NULL

boston.test <- Boston[-train, "medv"] # We're trying to assess model performance at predicting medv(median value of owner-occupied homes in \$1000s.)



# mtry = 4 to mtry = 13
mtry.range <- 4:13

# ntree = 10 to ntree = 25
ntree.range <- 10:25

test.error <- c()

for (mtry in mtry.range){
mse <- c()
ntrees.used <- c()
  for (ntree in ntree.range){
    rf.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=ntree)
    yhat.rf <- predict(rf.boston,newdata=Boston[-train,])
    mse <- rbind(mse,mean((yhat.rf-boston.test)^2))  
    ntrees.used <- rbind(ntrees.used,ntree)
  }
  test.error<- cbind(test.error, mse,ntrees.used)

}


colnames(test.error) <- c("mse.4mtry", "numtrees.4mtry", "mse.5mtry", "numtrees.5mtry", "mse.6mtry", "numtrees.6mtry", "mse.7mtry", "numtrees.7mtry",
                          "mse.8mtry", "numtrees.8mtry", "mse.9mtry", "numtrees.9mtry", "mse.10mtry", "numtrees.10mtry", "mse.11mtry", "numtrees.11mtry",
                          "mse.12mtry", "numtrees.12mtry", "mse.13mtry", "numtrees.13mtry")



plot(test.error[,2],test.error[,1], type = 'l', main = 'Random Forest (mtry=4)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,4],test.error[,3], type = 'l', main = 'Random Forest (mtry=5)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,6],test.error[,5], type = 'l', main = 'Random Forest (mtry=6)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,8],test.error[,7], type = 'l', main = 'Random Forest (mtry=7)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,10],test.error[,9], type = 'l', main = 'Random Forest (mtry=8)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,12],test.error[,11], type = 'l', main = 'Random Forest (mtry=9)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,14],test.error[,13], type = 'l', main = 'Random Forest (mtry=10)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,16],test.error[,15], type = 'l', main = 'Random Forest (mtry=11)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,18],test.error[,17], type = 'l', main = 'Random Forest (mtry=12)', xlab = 'numtrees',ylab='test.error')
plot(test.error[,20],test.error[,19], type = 'l', main = 'Random Forest (mtry=13)', xlab = 'numtrees',ylab='test.error')



