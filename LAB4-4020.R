# Paige Reynolds
# MATH 4020
# Lab 4: CROSS VALIDATION AND BOOTSTRAP
# VALIDATION (HOLDOUT) SETS #

rm(list= ls())
library(ISLR) # Load package
library(boot)
head(Auto)

# Randomly select the training indices and the the corresponding data to fit
# the mpg ~ hp model
trn.ind <- sample(dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ horsepower, data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma
# The approximate MSPE for the test is fairly close to the residual
# standard error

# Second time
trn.ind <- sample(dim(Auto)[1], dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ horsepower + I(horsepower^2), data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma

# Third time
trn.ind <- sample(dim(Auto)[1], dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ horsepower + I(horsepower^2), data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma

# The MSPE bounces around in the neighborhood 4.0-5.0, but it remains close
# to the residual standard error

# cubic?
trn.ind <- sample(dim(Auto)[1], dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ poly(horsepower, 5), data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma

# A second time
trn.ind <- sample(dim(Auto)[1], dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ poly(horsepower, 5), data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma

# A third time
trn.ind <- sample(dim(Auto)[1], dim(Auto)[1]/2)
auto.mod <- lm(mpg ~ poly(horsepower, 5), data= Auto, subset= trn.ind)
summary(auto.mod)

# MSE for the test set
mspe <- mean((Auto$mpg - predict(auto.mod, Auto))[-trn.ind]^2)
sqrt(mspe)
summary(auto.mod)$sigma

# LEAVE ONE OUT CROSS VALIDATION #
# After loading the boot package, fit the same SLR as about, but via the glm
# f-n so that we can pass it directly to cv.glm
(auto.glm <- glm(mpg ~ horsepower, data= Auto))
loo.cv <- cv.glm(Auto, auto.glm)
loo.cv$delta

# Loop over several polynomial orders to see the predictors
cee.vees <- rep(0, times= 10)
for (i in 1:10){
  cee.vees[i] <- cv.glm(Auto, glm(mpg ~ poly(horsepower, i),
                                  data= Auto))$delta[1]
  
}
cee.vees

plot(x= 1:10, y= cee.vees, lwd= 2, xlab= "Order", ylab= "CV Error",
     main= "")

# Run LOO again, but track the system time. Then use K=10 and track the time
# as well
system.time(
  
  for(i in 1:10){
    cee.vees[i] <- cv.glm(Auto, glm(mpg ~ poly(horsepower, i),
                                    data= Auto))$delta[1]
    
  } #end loop
) # end system timer

cv.k10 <- rep(0, times= 10)
system.time(
  for(i in 1:10){
    cv.k10[i] <- cv.glm(Auto, glm(mpg ~ poly(horsepower, i),
                                  data= Auto), K= 10)$delta[1]
  }
  
)

plot(x= 1:10, y= cv.k10, lwd=2, xlab= "Order", ylab= "CV Error",
     main= "")

# THE BOOTSTRAP: 5.3.4 #
alpha.fn <- function(data, index){
  X= data$X[index]
  Y= data$Y[index]
  return((var(Y)- cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)))
  
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace= TRUE))

boot(Portfolio, alpha.fn, R= 1000)

boot.fn <- function(data, index)
  return(coef(lm(mpg ~ horsepower, data= data, subset= index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace= TRUE))

boot.fn(Auto, sample(392, 392, replace= TRUE))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower, data= Auto))$coef

boot.fn <- function(data, index)
coefficients(lm(mpg ~ horsepower + I(horsepower^2), data= data,
                subset= index))
set.seed(1)
boot(Auto, boot.fn, 1000)
