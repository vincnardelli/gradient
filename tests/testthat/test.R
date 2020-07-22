set.seed(123)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)

y = 1 + .5*x1 + .2*x2 + rnorm(n)
x <- cbind(x1, x2)
stepsize <- 1e-5
tolerance <- 1e-10
maxit <- 1
b <- c(0.1, 0.1, 0.1)
data <- data.frame(y=y, x1=x1, x2=x2)

context("Gradient Descent")

test_that("gd", {
  expect_equal(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, stepsize, fun="gd")$b,
               structure(c(0.117811702875545, 0.107945369401251, 0.104008346008317
               ), .Dim = c(3L, 1L)))
})

test_that("gd tolerance reached", {
  expect_warning(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit=1000, tolerance, stepsize, fun="gd"),
               "Tolerance reached", fixed=TRUE)
})

context("Steepest Descent")

test_that("st", {
  expect_equal(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd")$b,
               structure(c(0.528624332625307, 0.291198937062253, 0.196457629276134
               ), .Dim = c(3L, 1L)))
})

test_that("st tolerance reached", {
  expect_warning(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit=1000, tolerance, fun="sd"),
                 "Tolerance reached", fixed=TRUE)
})

# check error < x
# check tolerance

context("Inputs Checks")

test_that("gd tolerance error", {
  expect_error(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance="ok", stepsize, fun="gd"),
               "Tolerance must be numeric", fixed=TRUE)
})

test_that("gd step error", {
  expect_error(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, stepsize="ok", fun="gd"),
               "Step size must be numeric", fixed=TRUE)
})

test_that("gd step error", {
  expect_error(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit="ok", tolerance, stepsize, fun="gd"),
               "Max Iteration (maxit) must be numeric", fixed=TRUE)
})

test_that("gd beta large", {
  expect_error(lm_gradient(b=c(100000, 100000, 100000), formula=y~x1+x2, data=data, maxit=50, tolerance, stepsize=1000, fun="gd"),
               "Beta too large: decrease the step size!", fixed=TRUE)
})

test_that("sd beta large", {
  expect_error(lm_gradient(b=c(1e400, 1e400, 1e400), formula=y~x1+x2, data=data, maxit=50, tolerance, fun="sd"),
               "Beta too large: decrease the step size!", fixed=TRUE)
})

test_that("gd formula without data", {
  expect_equal(lm_gradient(b=b, formula=y~x1+x2, maxit=maxit, tolerance=tolerance, stepsize=stepsize, fun="gd")$b,
               structure(c(0.117811702875545, 0.107945369401251, 0.104008346008317
               ), .Dim = c(3L, 1L)))
})

context("Class Checks")

test_that("gd class", {
  expect_equal(class(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, stepsize, fun="gd")),
               "gradient")
})

context("Methods")
a <- lm_gradient(b=b, formula=y~x1+x2, maxit=maxit, tolerance=tolerance, stepsize=stepsize, fun="gd")

test_that("print", {
  expect_equal(capture.output(print(a)),
               "beta0 0.11781  beta1 0.10795  beta2 0.10401  ")
})

test_that("summary", {
  expect_equal(capture.output(summary(a)),
               c("  coeff estimate sd t.value p.value", "1 beta0  0.11781 NA      NA      NA",
                 "2 beta1  0.10795 NA      NA      NA", "3 beta2  0.10401 NA      NA      NA",
                 ""))
})

test_that("coef", {
  expect_equal(coef(a),
               c(0.117811702875545, 0.107945369401251, 0.104008346008317)
  )
})
