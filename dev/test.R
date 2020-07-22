library(gradient)

n = 1000
x1 = rnorm(n)
x2 = rnorm(n)

#input
y = 1 + .5*x1 + .2*x2 + rnorm(n)
x <- cbind(x1, x2)
stepsize <- 1e-5
tolerance <- 1e-10
maxit <- 1000
b <- c(0.1, 0.1, 0.1)
verbose <- TRUE
data = data.frame(y=y, x1=x1, x2=x2)

# new
a <- lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd")
b <- lm_gradient(b, y~x1+x2, data, maxit, tolerance, stepsize, fun="gd")

print(a)
print(b)

summary(a)
summary(b)

coef(a)
coef(b)
