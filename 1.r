dataset = read.csv(file="D:/滑铁卢大学/S21/STAT 231/week 4/dataset.csv")


table(dataset$Mode_of_Shipment, dataset$Product_importance)/500










# Q4a
# Create a qqplot for the cost of the product variate
qqnorm(dataset$Cost_of_the_Product,
       lwd="3", 
       col="darkblue",
       pch=19, 
       main="QQ plot for the cost of the product",
       cex.axis=1.25,
       cex.lab=1.5
)


qqline (dataset$Cost_of_the_Product, 
        lwd="3", 
        col="darkred"
)

# Q5c
# Compute the estimates of 𝜃 = (𝜃1, 𝜃2, 𝜃3, 𝜃4, 𝜃5) for my dataset
n = length(dataset$Warehouse_block)  # number of observations
theta_1 <- length(dataset$Warehouse_block[which(dataset$Warehouse_block == "A")]) / n
theta_2 <- length(dataset$Warehouse_block[which(dataset$Warehouse_block == "B")]) / n
theta_3 <- length(dataset$Warehouse_block[which(dataset$Warehouse_block == "C")]) / n
theta_4 <- length(dataset$Warehouse_block[which(dataset$Warehouse_block == "D")]) / n
theta_5 <- length(dataset$Warehouse_block[which(dataset$Warehouse_block == "E")]) / n
theta_vector <- c(theta_1, theta_2, theta_3, theta_4, theta_5)
theta_vector


# Q5d
coe <- factorial(10) / (factorial(4) * factorial(4) * factorial(2))  # compute the constant coefficient
Z_theta_bar <- coe * theta_1 ** 4 * theta_2 ** 2 * (theta_3 ** 4 + theta_4 ** 4 + theta_5 ** 4)
Z_theta_bar

