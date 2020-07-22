
library(gradient)

n = 10000
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

# Convergence plot

plot(sd)
plot(gd)


# Predict

n1 <- 200
x11 <- rnorm(200)
x21 <- rnorm(200)
x31 <- rnorm(200)

new <- data.frame(x1=x11, x2=x21, x3=x31)

y1 = 1 + .5*x1 + .2*x2 + rnorm(n1)

y_pred <- predict(sd, new)
predict(gd, new)


# Validate

validate(lm_gradient(b, y~x1+x2, data=data, maxit,
                     tolerance, fun="sd"), new, y1)


cv <- lm_gradient_cv(10, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = TRUE)

print(cv)

summary(cv)

plot(cv)


# bench::mark(
#   parallel=lm_gradient_cv(2500, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = TRUE),
#   sequential = lm_gradient_cv(2500, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = FALSE),
#   check = FALSE
# )
#
# lm_gradient_looc(b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = FALSE)

