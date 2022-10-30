dataset = read.csv(file="D:/滑铁卢大学/S21/STAT 231/midterm/dataset.csv")
n <- 490

# Q1a plot the relative frequency histogram for the customer rating
dataset_omitted <- dataset  # store all the customer rating data
#  eliminate the missing data using the R code provided on Piazza
dataset_omitted$Customer_rating[dataset_omitted$Customer_rating == ""] <- NA   #label any blank entries NA
dataset_omitted <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them

x_axis <- c(1: 5)
y_axis <- c()
for (i in x_axis) {
  y_axis <- c(y_axis, length(dataset_omitted$Customer_rating[which(dataset_omitted$Customer_rating == i)]) / n)
}
barplot(y_axis,
        names.arg = x_axis, 
     col="seashell", 
     main="Relative frequency histogram of Customer rating", 
     xlab="Customer rating",
     ylab="Relative frequency"
)

# Q1b
#   eliminate the missing data for gender
dataset_omitted$Gender[dataset_omitted$Gender == ""] <- NA   #label any blank entries NA
dataset_omitted <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them
counts <- table(dataset_omitted$Gender, dataset_omitted$Customer_rating)
counts

# Q1c
Bar<-barplot(counts, main="Side-by-side bar graphs for Customer rating by Gender",
             xlab="Customer rating",ylab="Frequency", col=c("lightblue","red"),
             legend = c("F", "M"), beside=TRUE)
text(x =Bar , y = counts, label = counts, pos = 1.5, cex = 0.8, col = "black")

# Additional analysis
#   get the male ratings
male_rating <- dataset_omitted$Customer_rating[which(dataset_omitted$Gender == "M")]
y_axis_male <- c()
for (i in x_axis) {
  y_axis_male <- c(y_axis_male, length(male_rating[which(male_rating == i)]) / n)
}
#   get the female ratings
female_rating <- dataset_omitted$Customer_rating[which(dataset_omitted$Gender == "F")]
y_axis_female <- c()
for (i in x_axis) {
  y_axis_female <- c(y_axis_female, length(female_rating[which(female_rating == i)]) / n)
}
#   plot the relative frequency barplots for males and females
barplot(y_axis_male,
        names.arg = x_axis, 
        col="seashell", 
        main="Relative frequency barplot of males' customer rating", 
        xlab="Customer rating",
        ylab="Relative frequency"
)

barplot(y_axis_female,
        names.arg = x_axis, 11.19591, 13.3508
        col="seashell", 
        main="Relative frequency barplot of females' customer rating", 
        xlab="Customer rating",
        ylab="Relative frequency"
)


# Q1d
#   convert the customer rating variate into a binary variate
dataset_omitted$Customer_rating <- as.factor(ifelse(dataset_omitted$Customer_rating > 3,1,0))
table(dataset_omitted$Customer_rating)
# rename the attribute
colnames(dataset_omitted)[5] <- "Rating"

# Q1e
table(dataset_omitted$Reached_on_Time, dataset_omitted$Rating) / n

# Q1f
table_of_rating <- table(dataset_omitted$Rating) / n
theta_hat_bino <- table_of_rating[2]

# Q1g
y <- length(dataset_omitted$Rating[which(dataset_omitted$Rating == 1)])  # get the y
MyBino <- function (theta){
  Rtheta <- ((theta / theta_hat_bino) ** y) * (((1 - theta) / (1 - theta_hat_bino)) ** (n - y))
  
  return (Rtheta)  #object can be any data type
}

th_bino <- seq(0.25, 0.5,0.001)  # construct the bonimial possible theta values

Rela_rating <- MyBino(th_bino)

plot(th_bino, Rela_rating,
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
#plot the 15% likelihood line
abline(a = 0.15,b=0,col="red",lwd=2)

# Use the function uniroot to obtain a 15% likelihood interval (approximate
#   95% confidence interval) for θ
uniroot(function(th_bino) MyBino(th_bino)-0.15, lower=0.25, upper=theta_hat_bino)  # lower bound
uniroot(function(th_bino) MyBino(th_bino)-0.15, lower=theta_hat_bino, upper=0.5)  # upper bound


#  Q2b
dataset_omitted2 <- dataset  # store all the customer rating data
#  eliminate the missing data using the R code provided on Piazza
dataset_omitted2$Discount_offered[dataset_omitted$Discount_offered == ""] <- NA   #label any blank entries NA
dataset_omitted2 <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them 

library("moments")  # activate the package required to get skewness
sd(dataset_omitted2$Discount_offered)  # sample standard deviation
IQR(dataset_omitted2$Discount_offered)  # sample IQR
max(dataset_omitted2$Discount_offered) - min(dataset_omitted2$Discount_offered)  # sample range
skewness(dataset_omitted2$Discount_offered) #  sample skewness
summary(dataset_omitted2$Discount_offered)  # other summaries

# Q2c
theta_hat <- mean(dataset_omitted2$Discount_offered)  # get theta_hat
MyExpn <- function (theta){
  Rtheta <- (theta_hat / theta) ** n * exp(n * (1 - theta_hat / theta))
  
  return (Rtheta)  #object can be any data type
}
s <- sd(dataset_omitted2$Discount_offered)  # get the sample sd
th_expn <- seq(max(0,theta_hat-2*s), theta_hat+2*s,0.001)  # construct the exponential possible theta values

Rela_disc <- MyExpn(th_expn)

plot(th_expn, Rela_disc,
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
#plot the 15% likelihood line
abline(a = 0.15,b=0,col="red",lwd=2)

# Use the function uniroot to obtain a 15% likelihood interval (approximate
#   95% confidence interval) for θ
uniroot(function(th_expn) MyExpn(th_expn)-0.15, lower=10, upper=theta_hat)  # lower bound
uniroot(function(th_expn) MyExpn(th_expn)-0.15, lower=theta_hat, upper=18)  # upper bound

# Q3b
dataset_omitted3 <- dataset  # store all the customer rating data
#  eliminate the missing data using the R code provided on Piazza
dataset_omitted3$Customer_care_calls[dataset_omitted$Customer_care_calls == ""] <- NA   #label any blank entries NA
dataset_omitted3 <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them 

library("moments")  # activate the package required to get skewness
sd(dataset_omitted3$Customer_care_calls)  # sample standard deviation
IQR(dataset_omitted3$Customer_care_calls)  # sample IQR
max(dataset_omitted3$Customer_care_calls) - min(dataset_omitted3$Customer_care_calls)  # sample range
skewness(dataset_omitted3$Customer_care_calls) #  sample skewness
summary(dataset_omitted3$Customer_care_calls)  # other summaries

# Q3c
theta_hat_poi <- mean(dataset_omitted3$Customer_care_calls)  # get the MLE
times <- c(0: 4)
freq_of_time <- c()
expected_freq <- c()
for (i in times) {
  freq <- length(dataset_omitted3$Customer_care_calls[which(dataset_omitted3$Customer_care_calls == i)])
  expected <- dpois(i, theta_hat_poi) * n
  freq_of_time <- c(freq_of_time, freq)
  expected_freq <- c(expected_freq, expected)
}
times <- c(times, 5)
freq_of_time <- c(freq_of_time, n - sum(freq_of_time))
expected_freq <- c(expected_freq, (1 - ppois(4, theta_hat_poi)) * n)
table(freq_of_time, expected_freq)

tab <- data.frame(freq_of_time, expected_freq)
row.names(tab)[6] <- ">= 5"

# Q4b
dataset_omitted4 <- dataset  # store all the customer rating data
#  eliminate the missing data using the R code provided on Piazza
dataset_omitted4$Weight_in_gms[dataset_omitted4$Weight_in_gms == ""] <- NA   #label any blank entries NA
dataset_omitted4 <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them 
# get all shipped purchases
ship_weight <- dataset_omitted4$Weight_in_gms[which(dataset_omitted4$Mode_of_Shipment == "Ship")]
library("moments")  # activate the package required to get skewness
sd(ship_weight)  # sample standard deviation
IQR(ship_weight)  # sample IQR
max(ship_weight) - min(ship_weight)  # sample range
skewness(ship_weight) #  sample skewness
kurtosis(ship_weight)  # sample kurtosis
summary(ship_weight)  # other summaries


# Q4c
# Create a qqplot for the weight of shipped purchases
qqnorm(ship_weight,
       lwd="3", 
       col="darkblue",
       pch=19, 
       main="QQ plot for the weight products shipped by Ship only",
       cex.axis=1.25,
       cex.lab=1.5
)


qqline (ship_weight, 
        lwd="3", 
        col="darkred"
)


# Q4f
# get all purchases shipped by road
road_weight <- dataset_omitted4$Weight_in_gms[which(dataset_omitted4$Mode_of_Shipment == "Road")]
library("moments")  # activate the package required to get skewness
sd(road_weight )  # sample standard deviation
IQR(road_weight )  # sample IQR
max(road_weight ) - min(road_weight )  # sample range
skewness(road_weight ) #  sample skewness
kurtosis(road_weight )  # sample kurtosis
summary(road_weight )  # other summaries


# Q4g
# Create a qqplot for the weight of purchases shipped by Road
qqnorm(road_weight,
       lwd="3", 
       col="darkblue",
       pch=19, 
       main="QQ plot for the weight products shipped by Road only",
       cex.axis=1.25,
       cex.lab=1.5
)


qqline (road_weight, 
        lwd="3", 
        col="darkred"
)


# Q4j
# Compare the weight of the purchase (in grams) between the different shipping 
#  methods by creating 3 side by side boxplots for the weight in grams for each of 
#  Flight, Ship, and Road
# make the boxplot and combine them together
boxplot(formula = dataset_omitted4$Weight_in_gms ~ dataset_omitted4$Mode_of_Shipment,
        data = dataset,
        outline=TRUE,
        frame=T,
        col="seashell",
        main = "The weight of the purchase between the different shipping methods",
        xlab = "Mode of shipment",
        ylab="Weight of the purchase (g)",
        rm.NA=TRUE,
        cex.axis=1.15,
        cex.lab=1.5
)

# Q5b
dataset_omitted5 <- dataset  # store all the customer rating data
#  eliminate the missing data using the R code provided on Piazza
dataset_omitted5$Reached_on_Time[dataset_omitted5$Reached_on_Time == ""] <- NA   #label any blank entries NA
dataset_omitted5 <- as.vector(na.omit(dataset_omitted)) #remove all rows that have values of NA in them 

theta_hat_bino2 <- length(dataset_omitted5$Reached_on_Time[which(dataset_omitted5$Reached_on_Time == 1)]) / n
theta_hat_bino2


# Q5e
high_impor_Reach <- dataset_omitted5$Reached_on_Time[which(dataset_omitted5$Product_importance == "high")]
table(high_impor_Reach) / length(high_impor_Reach)

# Q5f
tab_del_impor <- table(dataset_omitted5$Reached_on_Time, dataset_omitted5$Product_importance)
tab_del_impor<-prop.table(tab_del_impor, 2) #solving for column proportions
tab_del_impor

# Q5g
Bar<-barplot(tab_del_impor, main="Side-by-side bar graphs for the proportion of products delivered on time by product importance",
             xlab="proportion of products delivered on time",ylab="Relative frequency", col=c("lightblue","red"),
             legend = c("Late", "On time"), beside=TRUE)
text(x =Bar , y = tab_del_impor, label = tab_del_impor, pos = 1.5, cex = 0.8, col = "black")

