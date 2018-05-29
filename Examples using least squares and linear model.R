#--------------Mcdonalds Sales vs Price---------------
setwd("C:/Users/sean/Documents/GitHub/ML")
happymeal = data.frame(price=c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5),
                       sales=c(420, 450, 420, 380, 440, 380, 360, 360))
attach(happymeal)
s2.marg <- mean( (sales-390)^2 )
s2.marg

s2.guess <- mean((sales - 500 + 60*price)^2)
s2.guess

r <- cor(sales, price)
r

b1 <- r*sd(sales)/sd(price)
b1

b0 <- mean(sales) - b1*mean(price)
b0

plot(price, sales, pch=20, main = "sales vs price regression")
abline(a=b0, b=b1, col=2)
legend("topright", "fitted line", col=2, lty=1)

#--------------Estimating the maintence costs---------------
tractor <- read.csv("tractor.csv")
attach(tractor)
plot(age, cost, main="tractor maintenance")


fit <- lm(cost ~ age)
fit$coef

b1 <- cor(cost,age)*sd(cost)/sd(age)
b0 <- mean(cost) - mean(age)*b1
c(b0, b1)

plot(age, cost, main="tractor maintenance")
abline(b0, b1)
legend("topleft", "LS fit", lty=1)