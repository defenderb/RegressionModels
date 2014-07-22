## q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)$coefficients
## q2
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 *mean(x)
ee <- y - beta0 - beta1*x
sigma <- sqrt(sum(ee^2)/(length(x)-2))
sigma
## q3
data(mtcars)
y<- mtcars$mpg
x <- mtcars$wt
fit <- lm(y ~ x)
sumCoef <- summary(fit)$coefficients
x0 <- mean(x)
predict(fit, data.frame(x=x0), interval = ("confidence"))
## q5
x0<-3
predict(fit, data.frame(x=x0), interval = ("prediction"))
## q6
xx <- x/2
fit2 <- lm(y ~ xx)
sumCoef2 <- summary(fit2)$coefficients
sumCoef2[2,1] + c(-1, 1) * qt(.975, df = fit2$df) * sumCoef2[2, 2]
## q9
sum((fit$residuals)^2)/sum((y-mean(y))^2)


