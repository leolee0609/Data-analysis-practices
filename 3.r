#### Problem 1: Poisson interval estimates #################################
set.seed(20866008) #replace XXXXXXXX with your student ID
# generate a random value of theta from a Uniform(5,10) distribution
theta<-round(runif(1,5,10) ,digits=1)
n <-80
y<-rpois(n,theta) # 80 observations from Poisson(theta) distribution
thetahat<-mean(y) # maximum likelihood estimate of theta
s<-(thetahat/n)^0.5 # estimate for std error of estimator
#determine an interval of values for plotting relative likelihood function
th<-seq(max(0,thetahat-4*s), thetahat+4*s,0.001)
# You can use th as the x values for your plot once you create the RLF function in R

# Q1d
#  Create a function for the Poisson relative likelihood function in R and plot the function
MyPoisson <- function (theta){
  Rtheta <- exp(n * (thetahat - theta)) * ((theta/thetahat) ^ (n * thetahat))
  
  return (Rtheta)  #object can be any data type
}
Rtheta <- MyPoisson(th)

plot(th,Rtheta,
     main = "Poisson relative likelihood function plot",
     type='l', 
     lwd='3', 
     col="darkblue",
     ylab=expression(paste("R(",theta,")")),
     xlab=expression(theta),
     cex.axis = 1.25,
     cex.lab=1.5,
     mgp = c(2.75,1,0)
)

#plot the 12% likelihood line
abline(a = 0.12,b=0,col="red",lwd=2)

# Q1f Use the uniroot command in R to determine the solved 12% likelihood interval for 𝜃
uniroot(function(th) MyPoisson(th)-0.12, lower=6.0, upper=7.0)  # lower bound
uniroot(function(th) MyPoisson(th)-0.12, lower=7.0, upper=8.0)  # upper bound

# Q2
# load the dataset
dataset = read.csv(file = "D:/滑铁卢大学/S21/STAT 231/week 1/A1/dataset.csv")
ROT <- dataset$Reached_on_Time

# Q2a Get the maximum likelihood estimate
mean(ROT)

# Q2b plot the relative likelihood function for θ based on my dataset
n <- length(ROT)  # the size of the dataset
y <- length(ROT[which(ROT == 1)])  # get the y
MyBino <- function (theta){
  Rtheta <- ((theta / 0.572) ** y) * (((1 - theta) / (1 - 0.572)) ** (n - y))
  
  return (Rtheta)  #object can be any data type
}

th_bino <- seq(0, 1,0.001)  # construct the bonimial possible theta values

Rela_ROT <- MyBino(th_bino)

plot(th_bino, Rela_ROT,
     main = expression(paste("Plot of the relative likelihood function for ", theta)),
     type='l', 
     lwd='3', 
     col="darkblue",
     ylab=expression(paste("R(",theta,")")),
     xlab=expression(theta),
     cex.axis = 1.25,
     cex.lab=1.5,
     mgp = c(2.75,1,0)
)

# Q2c Use the function uniroot to obtain a 15% likelihood interval (approximate
#   95% confidence interval) for θ
uniroot(function(th_bino) MyBino(th_bino)-0.15, lower=0.4, upper=0.572)  # lower bound
uniroot(function(th_bino) MyBino(th_bino)-0.15, lower=0.572, upper=0.7)  # upper bound


# Q3
#   a
pchisq(12.5, 10)
pnorm(12.5, 10, sqrt(20))


#   b
pchisq(52.9, 60, lower.tail = FALSE)
pnorm(52.9, 60, sqrt(120), lower.tail = FALSE)

#   c
pchisq(4.3, 2)
pexp(4.3, 1 / 2)

#   d
qchisq(0.05, 15)  # get the value of a
qchisq(0.05, 15, lower.tail = FALSE)  # get the value of b

#   e
qchisq(0.025, 10)  # get the value of a
qchisq(0.025, 10, lower.tail = FALSE)  # get the value of b
