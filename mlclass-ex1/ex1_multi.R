# ================ Preparing =====================
setwd("~/Coding/R/CMPUT466/mlclass-ex1")
library(ggplot2)
library(MASS)

# ================= Feature Normalization ================
data <- read.csv("ex1data2.txt", header=FALSE)
X <- as.matrix(data[,c("V1", "V2")])
y <- as.matrix(data$V3)
m <- length(y)

X <- scale(X)
mu <- attr(X, "scaled:center") 
sigma <- attr(X, "scaled:scale")

X <- cbind(1, X)

# ================== Gradient Descent ====================
computeCost = function(X, y, theta) {
  m = length(X)
  return(sum(((X %*% theta) - y)^2)/(2*m))
}

alpha = c(0.01, 0.03, 0.1, 0.3)
num_iters = 400
theta = matrix(0, 3, 4)
J_history <- matrix(0, num_iters, 4)
for(i in 1:num_iters) 
  for(j in 1:4) {
  theta[,j] <- theta[,j] - alpha[j]/m*t(X) %*% (X %*% theta[,j] - y)
  J_history[i, j] <- computeCost(X, y, theta[,j])
}

df <- data.frame(x=1:num_iters, y=J_history)
ggplot(df, aes(x)) + labs(x="Number of iterations", y="Cost J") +
  geom_line(aes(y=y.1, col="0.01")) +
  geom_line(aes(y=y.2, col="0.03")) +
  geom_line(aes(y=y.3, col="0.1")) + 
  geom_line(aes(y=y.4, col="0.3")) + 
  scale_color_discrete(name="Learning Rate")

X_predict <- (c(1650, 3) - mu)/sigma
price <- c(1, X_predict) %*% theta
price

# ================== Normal Equations ====================
data <- read.csv('ex1data2.txt', header=FALSE)
X <- as.matrix(data[, c("V1", "V2")])
y <- as.matrix(data$V3)
m <- length(y)

X <- cbind(1, X)

theta = ginv(t(X) %*% X) %*% t(X) %*% y

price <- c(1, 1650, 3) %*% theta0
price
