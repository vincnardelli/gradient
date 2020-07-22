
library(gradient)

n = 1000
x1 = rnorm(n)
x2 = rnorm(n)

#input
y = 1 + .5*x1 + .2*x2 + rnorm(n)
stepsize <- 1e-5
tolerance <- 1e-10
maxit <- 1e5
b <- c(0.1, 0.1, 0.1)
verbose <- TRUE
data = data.frame(y=y, x1=x1, x2=x2)

# new
sd <- lm_gradient(b, y~x1+x2, data=data, maxit,
                  tolerance, fun="sd")
gd <- lm_gradient(b, y~x1+x2, data=data, maxit,
                  tolerance, stepsize, fun="gd")

print(sd)
print(gd)

summary(sd)
summary(gd)

coef(sd)
coef(gd)

plot(sd)
plot(gd)





