# q1
data(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt ,data=mtcars)
coef(fit)
# q2
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl, pch =19)
fit <- lm(mpg ~ factor(cyl) + wt ,data=mtcars)
coef(fit)
fit2 <- lm(mpg ~ factor(cyl) ,data=mtcars)
coef(fit2)
# q3
fit3 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt ,data=mtcars)
coef(fit3)
temp <- update(fit, mpg ~ factor(cyl) + wt + factor(cyl)*wt)
anova(fit, temp)
summary(temp)
# q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x,y, pch=19)
fit4 <- lm(y~x)
influence.measures(fit4)
# q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit4 <- lm(y~x)
influence.measures(fit4)
dfbetas(fit4)
