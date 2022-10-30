dataset = read.csv(file="D:/滑铁卢大学/S21/STAT 231/midterm/dataset.csv")

# Q1a plot the relative frequency histogram for the customer rating
dataset_omitted <- dataset
dataset_omitted$Cost_of_the_Product[dataset_omitted$Cost_of_the_Product == ""] <- NA
dataset_omitted <- as.vector(na.omit(dataset_omitted))

dataset_omitted$Weight_in_gms[dataset_omitted$Weight_in_gms == ""] <- NA
dataset_omitted <- as.vector(na.omit(dataset_omitted))


plot(dataset_omitted$Weight_in_gms, dataset_omitted$Cost_of_the_Product,
     xlab="Weight of the product (g)",
     ylab="Cost of the product ($)",
     main = "Scatterplot of weight vs. cost of product with a fitting line",
     pch=19,
     col="darkblue",
     cex.axis=1.25,
     cex.lab=1.5
)
abline(lm(dataset_omitted$Cost_of_the_Product~dataset_omitted$Weight_in_gms), 
       col="darkred",
       lwd='3' 
)

cor(dataset_omitted$Weight_in_gms, dataset_omitted$Cost_of_the_Product)


# Q1b
RegModel <- lm(dataset_omitted$Cost_of_the_Product~dataset_omitted$Weight_in_gms)
RegModel
summary(RegModel)
mean(dataset_omitted$Weight_in_gms)  # mean x
mean(dataset_omitted$Cost_of_the_Product)  # mean y
alpha <- 226.196874
beta <- -0.004467

# Q1d
Se2 <- function (alpha, beta, y, x){
  y <- y - alpha - beta * x
  y <- y ** 2
  n <- length(x)
  return ((1 / (n - 2)) * sum(y))
}
Se <- sqrt(Se2(alpha, beta, dataset_omitted$Cost_of_the_Product, dataset_omitted$Weight_in_gms))

Sxx <- function (x){
  sxx <- sum((x - mean(x))^2)
  return (sxx)
}
sxx <- Sxx(dataset_omitted$Weight_in_gms)
d <- abs(beta - 0) / (Se / sqrt(sxx))
d

# Q1e
plot(dataset_omitted$Weight_in_gms, dataset_omitted$Cost_of_the_Product,
     xlab="Weight of the product (g)",
     ylab="Cost of the product ($)",
     main = "Scatterplot of weight vs. cost of product with a fitting line",
     pch=19,
     col="darkblue",
     cex.axis=1.25,
     cex.lab=1.5
)
abline(lm(dataset_omitted$Cost_of_the_Product~dataset_omitted$Weight_in_gms), 
       col="darkred",
       lwd='3' 
)
legend("topright",
       c("y = 226.196874 - 0.004467x"),
       col=c("darkred"),
       cex=1.5,
       pch=c(NA), lty=1
)

# Standardized residual plot vs. the expanatory variate
r<- RegModel$residuals # get residuals
rstar <- r/Se # the standardized residuals
plot(dataset_omitted$Weight_in_gms,rstar,xlab="weight (g)",ylab="Standardized Residual")
title(main="Standardized residual plot vs. weight in grams")
abline(0,0, col="darkblue")

# Standardized residual plot vs. the fitted values
r<- RegModel$residuals # get residuals
rstar <- r/Se # the standardized residuals
plot(RegModel$fitted.values,rstar,xlab="Fitted cost ($)",ylab="Standardized Residual")
title(main="Standardized residual plot vs. fitted cost of purchases")
abline(0,0, col="darkblue")

# Qqplot
qqnorm(rstar,main="")
title(main="Qqplot of the standardized Residuals")
qqline(rstar,col="darkblue")


# Q1g
confint(RegModel,level=0.95)

# Q1h
x <- dataset_omitted$Weight_in_gms
y <- dataset_omitted$Cost_of_the_Product
RegModel <- lm(y~x)
predict(RegModel,data.frame("x"=3000),interval="prediction",level=0.90)

# Q1i
predict(RegModel,data.frame("x"=3000),interval="prediction",level=0.99)

# Q1j
n <- length(dataset_omitted)
sigma_lower <- sqrt(((n-2)*Se^2)/(qchisq((1-0.95)/2,n-2,lower.tail=FALSE)))
sigma_upper <- sqrt(((n-2)*Se^2)/(qchisq((1-0.95)/2,n-2,lower.tail=TRUE)))
c(sigma_lower, sigma_upper)


# Q2
dataset_omitted1 <- dataset
dataset_omitted1$Gender[dataset_omitted1$Gender == ""] <- NA
dataset_omitted1 <- as.vector(na.omit(dataset_omitted1))

dataset_omitted1$Discount_offered[dataset_omitted1$Discount_offered == ""] <- NA
dataset_omitted1 <- as.vector(na.omit(dataset_omitted1))

# Q2a
qqnorm(dataset_omitted1$Discount_offered,main="")
title(main="Qqplot of the standardized Residuals")
qqline(dataset_omitted1$Discount_offered,col="darkblue")

qqnorm(sqrt(dataset_omitted1$Discount_offered),main="")
title(main="Qqplot of the standardized Residuals")
qqline(sqrt(dataset_omitted1$Discount_offered),col="darkblue")
