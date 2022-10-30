getSsd <- function (sigma_y, sigma_y2, n){
  var <- (1 / (n-1))*(sigma_y2 - (1/n)*(sigma_y)**2)
  return (sqrt(var))  #object can be any data type
}

LItoCI <- function (p){
  q <- 2 * pnorm(sqrt(-2 * log(p))) - 1
  
  return (q)  #object can be any data type
}

CItoLI <- function (p){
  a <- qnorm((p + 1) / 2, 0, 1)
  q <- exp(-a**2 / 2)
  
  return (q)  #object can be any data type
}

GaussianPvalueknownS <- function (y_bar, theta_null, sd, n){
  d <- abs(y_bar - theta_null) / (sd / sqrt(n))
  pvalue <- 2 * (1 - pnorm(d, 0, 1))
  
  return (pvalue)  #object can be any data type
}

GaussianPvalueUnknownS <- function (y_bar, theta_null, sd, n){
  d <- abs(y_bar - theta_null) / (sd / sqrt(n))
  pvalue <- 2 * (1 - pt(d, n - 1))
  
  return (pvalue)  #object can be any data type
}

GaussianPvalueUnknownS_upper <- function (y_bar, theta_null, sd, n){
  d <- abs(y_bar - theta_null) / (sd / sqrt(n))
  pvalue <- 1 - pt(d, n - 1)
  
  return (pvalue)  #object can be any data type
}

GaussianPvalue_forSigma <- function (sigma_null, sd, n){
  u <- ((n - 1) * (sd ** 2)) / (sigma_null ** 2)
  pvalue1 <- 2 * pchisq(u, n - 1, lower.tail = TRUE)
  pvalue2 <- 2 * pchisq(u, n - 1, lower.tail = FALSE)
  pvalue <- min(pvalue1, pvalue2)
  
  return (pvalue)  #object can be any data type
}


GaussianPvalueUnknownS_lower <- function (y_bar, theta_null, sd, n){
  d <- abs(y_bar - theta_null) / (sd / sqrt(n))
  pvalue <- pt(d, n - 1)
  
  return (pvalue)  #object can be any data type
}

BinoRela <- function (theta, y, n){
  theta_hat_bino <- y / n
  Rtheta <- ((theta / theta_hat_bino) ** y) * (((1 - theta) / (1 - theta_hat_bino)) ** (n - y))
  
  return (Rtheta)  #object can be any data type
}

BinoZv <- function (theta, y, n){
  rela <- BinoRela(theta, y, n)
  zv <- sqrt(-2 * log(rela))
  return (zv)  #object can be any data type
}

BinoPvalue <- function (theta, y, n){
  zv <- BinoZv(theta, y, n)
  pvalue <- 2 * (1 - pnorm(zv, 0, 1))
  return (pvalue)  #object can be any data type
}

ExpoPivD <- function (theta, theta_hat, n){
  d <- abs(theta_hat - theta) / (theta / sqrt(n))
  return (d)  #object can be any data type
}

ExpoPivPvalue <- function (theta, theta_hat, n){
  d <- ExpoPivD(theta, theta_hat, n)
  pvalue <- 2 * (1 - pnorm(d, 0, 1))
  return (pvalue)  #object can be any data type
}

ExpoPivPvalue_upper <- function (theta, theta_hat, n){
  d <- ExpoPivD(theta, theta_hat, n)
  pvalue <- 1 - pnorm(d, 0, 1)
  return (pvalue)  #object can be any data type
}

ExpoPivPvalue_lower <- function (theta, theta_hat, n){
  d <- ExpoPivD(theta, theta_hat, n)
  pvalue <- pnorm(d, 0, 1)
  return (pvalue)  #object can be any data type
}

ExpoRela <- function (theta, theta_hat, n){
  Rtheta <- (theta_hat / theta) ** n * exp(n * (1 - theta_hat / theta))
  
  return (Rtheta)  #object can be any data type
}

ExpoZv <- function (theta, theta_hat, n){
  rela <- ExpoRela(theta, theta_hat, n)
  zv <- sqrt(-2 * log(rela))
  return (zv)  #object can be any data type
}

ExpoPvalue <- function (theta, theta_hat, n){
  zv <- ExpoZv(theta, theta_hat, n)
  pvalue <- 2 * (1 - pnorm(zv, 0, 1))
  return (pvalue)  #object can be any data type
}

PoissonRela <- function (theta, thetahat, n){
  Rtheta <- exp(n * (thetahat - theta)) * ((theta/thetahat) ^ (n * thetahat))
  
  return (Rtheta)  #object can be any data type
}

PoissonZv <- function (theta, thetahat, n){
  rela <- PoissonRela(theta, thetahat, n)
  zv <- sqrt(-2 * log(rela))
  return (zv)  #object can be any data type
}

PoissonPvalue <- function (theta, thetahat, n){
  zv <- PoissonZv(theta, thetahat, n)
  pvalue <- 2 * (1 - pnorm(zv, 0, 1))
  return (pvalue)  #object can be any data type
}


PoissonPivd <- function (theta, thetahat, n){
  d <- abs(thetahat - theta) / sqrt(theta / n)
  return (d)  #object can be any data type
}

PoissonPivPvalue <- function (theta, thetahat, n){
  d <- PoissonPivd(theta, thetahat, n)
  pvalue <- 2 * (1 - pnorm(d, 0, 1))
  return (pvalue)  #object can be any data type
}



NormalKnVarD <- function (y_bar, mean, sd, n){
  d <- abs(y_bar - mean) / (sd / sqrt(n))
  return (d)  #object can be any data type
}

# CI
PoissonPivCI <- function (p, thetahat, n){
  lowb <- thetahat - qnorm((1 + p) / 2) * sqrt(thetahat / n)
  highb <- thetahat + qnorm((1 + p) / 2) * sqrt(thetahat / n)
  return (c(lowb, highb))  #object can be any data type
}

BinoPivCI <- function (p, thetahat, n){
  lowb <- thetahat - qnorm((1 + p) / 2, 0, 1) * sqrt((thetahat * (1 - thetahat)) / n)
  highb <- thetahat + qnorm((1 + p) / 2, 0, 1) * sqrt((thetahat * (1 - thetahat)) / n)
  return (c(lowb, highb))  #object can be any data type
}

PoisPivCI <- function (p, thetahat, n){
  lowb <- thetahat - qnorm((1 + p) / 2, 0, 1) * sqrt(thetahat / n)
  highb <- thetahat + qnorm((1 + p) / 2, 0, 1) * sqrt(thetahat / n)
  return (c(lowb, highb))  #object can be any data type
}

ExpPivCI <- function (p, thetahat, n){
  lowb <- thetahat - qnorm((1 + p) / 2, 0, 1) * thetahat / sqrt(n)
  highb <- thetahat + qnorm((1 + p) / 2, 0, 1) * thetahat / sqrt(n)
  return (c(lowb, highb))  #object can be any data type
}

GauPivCIknownForMu <- function (p, y_bar, sd, n){
  lowb <- y_bar - qnorm((1 + p) / 2, 0, 1) * sd / sqrt(n)
  highb <- y_bar + qnorm((1 + p) / 2, 0, 1) * sd / sqrt(n)
  return (c(lowb, highb))  #object can be any data type
}

GauPivCIunknownForMu <- function (p, y_bar, sd, n){
  b <- qt((1 + p) / 2, n - 1)
  lowb <- y_bar - b * sd / sqrt(n)
  highb <- y_bar + b * sd / sqrt(n)
  return (c(lowb, highb))  #object can be any data type
}

GauPivCIunknownForVar <- function (p, sd, n){
  d <- qchisq((1 + p) / 2, n - 1)
  c <- qchisq((1 - p) / 2, n - 1)
  lowb <- ((n - 1) * (sd ** 2)) / d
  highb <- ((n - 1) * (sd ** 2)) / c
  return (c(lowb, highb))  #object can be any data type
}

GauPivCIunknownForsd <- function (p, sd, n){
  d <- qchisq((1 + p) / 2, n - 1)
  c <- qchisq((1 - p) / 2, n - 1)
  lowb <- ((n - 1) * (sd ** 2)) / d
  highb <- ((n - 1) * (sd ** 2)) / c
  return (c(sqrt(lowb), sqrt(highb)))  #object can be any data type
}

ExpoPivCIforTheta <- function (p, y_bar, n){
  d <- qchisq((1 + p) / 2, 2 * n)
  c <- qchisq((1 - p) / 2, 2 * n)
  lowb <- (2 * n * y_bar) / d
  highb <- (2 * n * y_bar) / c
  return (c(lowb, highb))  #object can be any data type
}

GauPivCIunknownForn <- function (p, width, y_bar, sd){
  d <- width / 2
  a <- qnorm((1 + p) / 2)
  n <- ((a * sd) / d) ** 2
  return (n)  #object can be any data type
}

GauPivCIunknownForsd_upperbound <- function (p, sd, n){
  a <- qchisq(1 - p, n - 1)
  lowb <- 0
  highb <- sd * sqrt((n - 1) / a)
  return (c(sqrt(lowb), sqrt(highb)))  #object can be any data type
}

BinoPivZv <- function (thetahat, theta_null, n){
  z <- abs(thetahat - theta_null) / sqrt((theta_null * (1 - theta_null)) / n)
  return (z)  #object can be any data type
}


BinoPivPvalue <- function (thetahat, theta_null, n){
  z <- abs(thetahat - theta_null) / sqrt((theta_null * (1 - theta_null)) / n)
  pvalue <- 2 * (1 - pnorm(z, 0, 1))
    return (pvalue)  #object can be any data type
}

regressPivCIforBeta <- function (p, se, Sxx, beta, n){
  a <- qt((1 + p) / 2, n - 2)
  low <- beta - a * se / sqrt(Sxx)
  up <- beta + a * se / sqrt(Sxx)
  return (c(low, up))
}

Se2 <- function (alpha, beta, y, x){
  y <- y - alpha - beta * x
  y <- y ** 2
  n <- length(x)
  return ((1 / (n - 2)) * sum(y))
}

regressPivCIforYatx <- function (p, x, alpha, beta, se, x_bar, sxx, n){
  a <- qt((1 + p) / 2, n - 2)
  mu <- alpha + beta * x
  low <- mu - a * se * sqrt(1 / n + (x - x_bar)**2 / sxx)
  up <- mu + a * se * sqrt(1 / n + (x - x_bar)**2 / sxx)
  return (c(low, up))
}

regressPivPIforYatx <- function (p, x, alpha, beta, se, x_bar, sxx, n){
  a <- qt((1 + p) / 2, n - 2)
  mu <- alpha + beta * x
  low <- mu - a * se * sqrt(1 + 1 / n + (x - x_bar)**2 / sxx)
  up <- mu + a * se * sqrt(1 + 1 / n + (x - x_bar)**2 / sxx)
  return (c(low, up))
}


Sxx <- function (x){
  sxx <- sum((x - mean(x))^2)
  return (sxx)
}

multinomLambda <- function (obs, expected){
  Lambda <- 2*sum(obs * log(obs / expected))
  return (Lambda)
}

RegreSlopeCI <- function (p, beta, Se, Sxx){
  Lambda <- 2*sum(obs * log(obs / expected))
  return (Lambda)
}

LinearFit <- function (Sxy, Sxx, x_bar, y_bar, n){
  beta <- (Sxy + n * x_bar * y_bar)/(Sxx + n * x_bar ^ 2)
  return (beta)
}
