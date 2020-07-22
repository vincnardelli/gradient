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

test_that("gd", {
  expect_equal(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, stepsize, fun="gd")$b,
               structure(c(0.117811702875545, 0.107945369401251, 0.104008346008317
               ), .Dim = c(3L, 1L)))
})

test_that("gd tolerance reached", {
  expect_warning(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit=1000, tolerance, stepsize, fun="gd"),
               "Tolerance reached", fixed=TRUE)
})

# check error < x
# check tolerance

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

test_that("gd class", {
  expect_equal(class(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, stepsize, fun="gd")),
               "gradient")
})
