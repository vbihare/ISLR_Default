library(ISLR)

data("Default")

#(a) Fit a logistic regression model that uses income and balance 
#to predict default

set.seed(100)
Lg.model <- glm(default ~ income + balance ,data=Default, family="binomial" )
summary(Lg.model)

#(b) Using the validation set approach, estimate the test error of this model. 
#In order to do this, you must perform the following steps: 
# i. Split the sample set into a training set and a validation set.

train <- sample(dim(Default)[1],dim(Default)/2)
#train

#ii. Fit a multiple logistic regression model using only the training 
#observations. 

fit.glm <- glm(default ~ income + balance,data = Default, family = "binomial",
              subset = train )
summary(fit.glm)

#iii. Obtain a prediction of default status for each individual in the 
#validation set by computing the posterior probability of default for that 
#individual, and classifying the individual to the default category if the 
#posterior probability is greater than 0.5.

probs <- predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No",length(probs))
pred.glm[probs>0.5] <- "Yes"

#iv. Compute the validation set error, which is the fraction of the observations in the 
#validation set that are misclassiﬁed.

mean(pred.glm != Default[-train,]$default)


#(c) Repeat the process in (b) three times, using three diﬀerent splits of the 
#observations into a training set and a validation set. Comment on the results obtained.

#1st time
train <- sample(dim(Default)[1],dim(Default)/2)
fit.glm <- glm(default ~ income + balance,data = Default, family = "binomial",
               subset = train )
probs <- predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No",length(probs))
pred.glm[probs>0.5] <- "Yes"
mean(pred.glm != Default[-train,]$default)

#2nd Time
train <- sample(dim(Default)[1],dim(Default)/2)
fit.glm <- glm(default ~ income + balance,data = Default, family = "binomial",
               subset = train )
probs <- predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No",length(probs))
pred.glm[probs>0.5] <- "Yes"
mean(pred.glm != Default[-train,]$default)

#3rd Time
train <- sample(dim(Default)[1],dim(Default)/2)
fit.glm <- glm(default ~ income + balance,data = Default, family = "binomial",
               subset = train )
probs <- predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No",length(probs))
pred.glm[probs>0.5] <- "Yes"
mean(pred.glm != Default[-train,]$default)

#(d) Now consider a logistic regression model that predicts the probability of default 
#using income, balance, and a dummy variable for student. Estimate the test error for 
#this model using the validation set approach. Comment on whether or not including a 
#dummy variable for student leads to a reduction in the test error rate.

train <- sample(dim(Default)[1],dim(Default)/2)
fit.glm <- glm(default ~ income + balance + student ,data = Default, family = "binomial",
               subset = train )
probs <- predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm <- rep("No",length(probs))
pred.glm[probs>0.5] <- "Yes"
mean(pred.glm != Default[-train,]$default)

#It doesn’t seem that adding the “student” dummy variable leads to a reduction in the 
#validation set estimate of the test error rate.




