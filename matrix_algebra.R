
# Introduction to Linear Models

install.packages("UsingR")
library(UsingR)
data("father.son",package="UsingR")

#What is the average height of the sons (don't round off)?
mean(father.son$sheight)

#What is the mean of the son heights for fathers that have a height of 71 inches (don't round off your answer)?
library(dplyr)
data = filter(father.son, round(fheight)==71) %>% select(sheight) %>% unlist
mean(data)

#scaler : numbers
#vector: series of numbers
#matrix: series of vectors

#create vector
c(1,5,3,4)
# we can turn vectors into matrices using below functions:
# rbind() , cbind() , matrix()

X = matrix(1:1000, 100,10)
#What is the entry in row 25, column 3 ?
X[25,3]

# Matrix Notation Exercises #2
first_col <- 1:10
y <- cbind(x1=first_col, x2=first_col*2,x3=first_col*3,x4=first_col*4,x5=first_col*5)
sum(y[7,])

#Whats the last element of the vector returned by seq(10,1,-2)?
seq(10,1,-2)


# Matrix Operations
X <- matrix(1:12,4,3)
print(X *2)
t(X)#transpose

#inverse * original matrix = 1
X <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y <- matrix(c(6,2,1),3,1)
solve(X) %*%y # or use solve(x,y)

#Matrix Operation Exercises #2
X <- matrix(c(3,2,1,5,4,2,-1,0,-5,2,5,0,1,-1,-5,1),4,4)
y <- matrix(c(10,5,7,4),4,1)
solve(X,y)

#Matrix Operation Exercises #3
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
# What is the value in the 3rd row and the 2nd column of the matrix product of a and b?
c <- a %*% b # multiplication
c[3,2]

#Matrix Operation Exercises #4
#Multiply the 3rd row of a with the 2nd column of b, using the element-wise vector multiplication with *.
sum(a[3,] * b[,2])

#
# The sample mean
library(UsingR)
y <- father.son$sheight
print(mean(y))
N <- length(y)
Y <- matrix(y,N,1)
A <- matrix(1,N,1)
barY <- t(A) %*%Y / N
barY <- crossprod(A,Y) / N
print(barY)

## The Variance
r <- y - barY
crossprod(r) / N
# below is the same
var(y) * (N-1) / N


## Minimizing RSS
## Estimates of unknown parameters in linear model (beta)
# y = beta0 + beta1 (X1) + c :linear mode
library(UsingR)
x=father.son$fheight
y=father.son$sheight
X <- cbind(1,x)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
#t(X): transpose of X (X^T)
# %*% : multiply 
##or
betahat <- solve(crossprod(X))%*%crossprod(X,y)


## Matrix Algebra
g <- 9.8 ##meters per second
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
f <- 56.67  - 0.5*g*tt^2
y <-  f + rnorm(n,sd=1)
plot(tt,y,ylab="Distance in meters",xlab="Time in seconds")
lines(tt,f,col=2)
##
#function that computes the RSS for any vector
## RSS is a measure of the amount of variability or "error" in the observed values of the response variable that is not explained by the regression model. It quantifies the sum of the squared differences between the observed values (actual responses) and the values predicted by the regression model.


rss <- function(Beta0,Beta1,Beta2){
  r <- y - (Beta0+Beta1*tt+Beta2*tt^2)
  return(sum(r^2))
}
Beta2s <- seq(-10,0,len=100)
RSS <- sapply(Beta2s, rss, Beta0=55, Beta1=0)
plot(Beta2s, RSS, type="l") #plot the minimum
#The Ordinary Least Squares (OLS) solution provides the coefficients that minimize the RSS, resulting in a regression line that best fits the observed data.


##
tt2 <- tt^2
fit <- lm(y~tt+tt2)
summary(fit)$coef

## 
X <- cbind(1,length(tt),tt^2)# three vector, 1st:all ones
head(X)
Beta <-  matrix(c(55,0,5),3,1)
r <- y-X %*% Beta
RSS <- t(r) %*% r 
rss(55,0,5)#check this 
RSS# check thsi
#both should be same

RSS <- crossprod(r)#same answer


##
betahat <- crossprod(X) %*% crossprod(X,y)

QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
backsolve(R,crossprod(Q,y))

## Matrix Algebra Examples Exercises
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
#Suppose that the fitted parameters for a linear model
beta <- c(5, 2)
# Question 1
#What is the fitted value for the A samples? (The fitted Y values.)
beta <- matrix(c(5,2),nrow=2,ncol=1)
X[1:2,] %*% beta
#Question 2
#What is the fitted value for the B samples? (The fitted Y values.)
X[3:4,] %*% beta


## Inference Review 
#Exercises #1
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
#Now we act as if we didn't know  h0 , v0 and  - 0.5* g
#and use regression to estimate these. We can rewrite the model as
# y= b0 +b1+b2 t^2 + e and obtain the LSE we have used in this class
# Note that g = -2*b2
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

# Exercises #2
set.seed(1)
B = 100000
g = 9.8
n =25
tt = seq(0,3.4,len=n)
X = cbind(1,tt,tt^2) 
A = solve(crossprod(X))%*%t(X)
betahat = replicate(B,{
  y=56.67 - 0.5*g*tt^2 + rnorm(n,sd=1)
  betahats = -2*A%*%y
  return(betahats[3])
  })
sqrt(mean((betahat-mean(betahat))^2))


# Standard Errors Exercises
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef

#Standard Errors Exercises #1
#fitted values Yhat from a linear model can be obtained with:
fit = lm(y~x)
fit$fitted.values

# What is the sum of the squared residuals (where residuals are given by ri = yi - yhat
# SSR = sum((y - fit$fitted.values)^2)

#sigma2 = SSR / 48 # 48 because 50 sample - 2

#Standard Errors Exercises #2
X = cbind(rep(1,N),x)
#the inverse of X transpose times X. Use the solve() function for the inverse and t() for the transpose. What is the element in the first row, first column?
a = solve(t(X)%*% X)
a[1,1]

#Standard Errors Exercises #3
fit = lm(y ~x)
sigma2 = sum((y - fit$fitted.values)^2) / (N-2)
sqrt(sigma2 * diag(solve(t(X) %*% X)))# will give use two numbers, the standard error for the intercept and the standard error for the slope.


# Week 2 Quiz
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)

#Question #1
#What is the fitted value for the B samples?
X[3:4,] %*% beta
#Question #2
#What is the fitted value for the C samples?
X[5:6,] %*% beta

#Question #3
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
#Now let's run a Monte Carlo simulation in which we take a sample of size 50 over and over again. Here is how we obtain one sample:
N =  50
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat =  lm(y~x)$coef
#Use the function replicate() to take 10,000 samples
N = 50
B = 10000
set.seed(1)
betahat = replicate(B,{
  index = sample(n,N)
  sampledat = father.son[index,]
  x = sampledat$fheight
  y = sampledat$sheight
  lm(y~x)$coef[2]
})
sqrt ( mean( (betahat - mean(betahat) )^2 )) #What is the standard error of the slope estimate?

# Question #4
mean((y - mean(y))*(x-mean(x)))


