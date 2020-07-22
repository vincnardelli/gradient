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


test_that("gd", {
  expect_equal(gd(b, y~x1+x2, stepsize, maxit, tolerance)$b,
               structure(c(0.117811702875545, 0.107945369401251, 0.104008346008317
               ), .Dim = c(3L, 1L)))
})

test_that("gd tolerance reached", {
  expect_warning(gd(b, y~x1+x2, stepsize, maxit=1000, tolerance)$b,
               "Tolerance reached", fixed=TRUE)
})

# check error < tolerance

test_that("gd tolerance error", {
  expect_error(gd(b, y~x1+x2, stepsize, maxit, tolerance="ok"),
               "Tolerance must be numeric", fixed=TRUE)
})

test_that("gd step error", {
  expect_error(gd(b, y~x1+x2, stepsize="ok", maxit, tolerance),
               "Step size must be numeric", fixed=TRUE)
})

test_that("gd step error", {
  expect_error(gd(b, y~x1+x2, stepsize, maxit="ok", tolerance),
               "Max Iteration (maxit) must be numeric", fixed=TRUE)
})

test_that("gd beta large", {
  expect_error(gd(b=c(100000, 100000, 100000), y~x1+x2, stepsize=1000, maxit=50, tolerance)$b,
               "Beta too large: decrease the step size!", fixed=TRUE)
})

test_that("gd class", {
  expect_equal(class(gd(b, y~x1+x2, stepsize, maxit, tolerance)),
               "gradient")
})
