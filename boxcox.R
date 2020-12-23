library(MASS)   #Contains function boxcox

X <- seq(0.01,2,by=0.01)   #Evenly spaced data
epsilon <- 0.2 * rnorm(length(X))   #N(0, 0.2^2)

#Transform X#
sqrtX <- sqrt(X)
invX <- 1/X
logX <- log(X)
X2 <- X^2

#Generate Y#
Y2 <- 3*X2 + epsilon
Y3 <- logX + epsilon
Y4 <- exp(X + epsilon)
Y5 <- 1/(1 + X + epsilon)

#Plot data#
par(mfrow=c(2,2))
plot(X, Y2, xlab="X", ylab="Y2")
plot(X, Y3, xlab="X", ylab="Y3")
plot(X, Y4, xlab="X", ylab="Y4")
plot(X, Y5, xlab="X", ylab="Y5")

#####Transformations in X#####
par(mfrow=c(2,2))
plot(X, Y2, xlab="X", ylab="Y2")
plot(X2, Y2, xlab="X^2", ylab="Y2")
plot(X, Y3, xlab="X", ylab="Y3")
plot(logX, Y3, xlab="logX", ylab="Y3")
###Fit data to models###
#X2#
fit.X.Y2 <- lm(Y2~X); plot(X, fit.X.Y2$residuals, xlab="X", ylab="Estimated residuals", main="Looks quadratic")
fit.X2.Y2 <- lm(Y2~X2); plot(X2, fit.X2.Y2$residuals, xlab="X2", ylab="Estimated residuals", main="Looks better")
#logX#
fit.X.Y3 <- lm(Y3~X); plot(X, fit.X.Y3$residuals, xlab="X", ylab="Estimated residuals", main="Looks logistic")
fit.logX.Y3 <- lm(Y3~logX); plot(logX, fit.logX.Y3$residuals, xlab="logX", ylab="Estimated residuals", main="Looks better")
sd(fit.logX.Y3$residuals[logX < -1]); sd(fit.logX.Y3$residuals[logX >= -1])

#####Transformations of Y, i.e. Box-Cox#####
#Y5#
data.X.Y5 <- data.frame(X=X, Y5=Y5)
fit.X.Y5 <- lm(Y5 ~ X, data=data.X.Y5)
plot(X, fit.X.Y5$residuals, xlab="X", ylab="Estimated residuals")  #Non-linear AND non-constant variance

boxcox.X.Y5 <- boxcox(fit.X.Y5, plotit = T)  #Looks like lambda=-1 is reasonable and is in 95% CI
lambda <- -1
Y5.tilde <- (Y5^lambda - 1)/lambda
fit.X.Y5.boxcox <- lm(Y5.tilde ~ X)
plot(X, fit.X.Y5.boxcox$residuals, xlab="X", ylab="Box-Cox residuals")

#Y4#
data.X.Y4 <- data.frame(X=X, Y5=Y4)
fit.X.Y4 <- lm(Y4 ~ X, data=data.X.Y4)
plot(X, fit.X.Y4$residuals, xlab="X", ylab="Estimated residuals")  #Non-linear AND non-constant variance

boxcox.X.Y4 <- boxcox(fit.X.Y4, plotit = T)  #Looks like lambda=0 is reasonable
lambda <- 0   #Box-Cox is defined as log when this is the case
Y4.tilde <- log(Y4)
fit.X.Y4.boxcox <- lm(Y4.tilde ~ X)
plot(X, fit.X.Y4.boxcox$residuals, xlab="X", ylab="Box-Cox residuals")
