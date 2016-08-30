# ================ Preparing =====================
setwd("~/Coding/R/CMPUT466/mlclass-ex1")
library(ggplot2)

# ================= Plotting =====================
data <- read.csv("ex1data1.txt", header=FALSE)
p <- ggplot(data, aes(V1, V2)) + geom_point(color=I("red"), shape=I(4), size=I(3)) + 
  labs(x="Population of City in 10,000s", y="Profit in $10,000s")

# =============== Gradient Descent ================
computeCost = function(X, y, theta) {
  m = length(X)
  return(sum(((X %*% theta) - y)^2)/(2*m))
}

data$V3 <- 1
X <- as.matrix(data[, c("V3", "V1")])
y <- as.matrix(data$V2)
theta <- matrix(0, 2, 1)
computeCost(X, y, theta)

iterations <- 1500
alpha <- 0.01
m <- length(y)
J_history <- matrix(0, iterations, 1)
for(i in 1:iterations) {
  theta <- theta - alpha/m*t(X) %*% (X %*% theta - y)
  J_history[i] <- computeCost(X, y, theta)
}

p + geom_abline(color=I("Blue")) + 
  annotate("text", x=10, y=13, color=I("Blue"), label="optimal approx.") +
  geom_line(aes(X[,2], X %*% theta), color=I("Green")) + 
  annotate("text", x=15, y=10, color=I("Green"), label="gradient descent approx.")
