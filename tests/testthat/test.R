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

test_that("sd", {
  expect_equal(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd")$b,
               structure(c(0.528624332625307, 0.291198937062253, 0.196457629276134
               ), .Dim = c(3L, 1L)))
})

test_that("sd tolerance reached", {
  expect_warning(lm_gradient(b=b, formula=y~x1+x2, data=data, maxit=1000, tolerance, fun="sd"),
                 "Tolerance reached", fixed=TRUE)
})



context("kfold Cross validation")
test_that("gd cv sequential", {
  expect_equal(lm_gradient_cv(5, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="gd", parallel = FALSE)$b,
               c(beta0 = 0.242493623004362, beta1 = 0.163562955210011, beta2 = 0.132066768066533
               ))
})

test_that("gd cv sequential without data", {
  expect_equal(lm_gradient_cv(5, b=b, formula=y~x1+x2, maxit=maxit, tolerance=tolerance, fun="gd", parallel = FALSE)$b,
               c(beta0 = 0.242493623004362, beta1 = 0.163562955210011, beta2 = 0.132066768066533
               ))
})

test_that("sd cv sequential", {
  set.seed(123)
  expect <- lm_gradient_cv(5, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = FALSE)$b
  expect_equal(expect,
               c(beta0 = 0.528313287442981, beta1 = 0.291059616808143, beta2 = 0.196367399429546
               ))
})

# test_that("sd cv parallel", {
#   set.seed(123)
#   expect <- lm_gradient_cv(5, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = TRUE)$b
#   expect_equal(expect,
#                c(beta0 = 0.528313287442981, beta1 = 0.291059616808143, beta2 = 0.196367399429546
#                ))
# })
# https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions

context("looc Cross validation")
test_that("sd looc sequential", {
  set.seed(123)
  expect <- lm_gradient_looc(b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="sd", parallel = FALSE)$b
  expect_equal(expect,
               c(beta0 = 0.528623245104453, beta1 = 0.291198083525709, beta2 = 0.196457041946565
               ))
})

test_that("gd looc sequential without data", {
  set.seed(123)
  expect <- lm_gradient_looc(b=b, formula=y~x1+x2, maxit=maxit, tolerance=tolerance, fun="gd", parallel = FALSE)$b
  expect_equal(expect,
               c(beta0 = 0.277938911726697, beta1 = 0.179374240318502, beta2 = 0.140043376623082
               ))
})

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

context("Methods gradient")
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


test_that("plot", {
  expect_true("ggplot" %in% class(plot(a)))
})

test_that("predict", {
    expect_equal(nrow(predict(a, data.frame(x1=1, x2=1))),
                 1
)
})


context("Methods gradient_cv")
a <- lm_gradient_cv(5, b=b, formula=y~x1+x2, data=data, maxit, tolerance, fun="gd", parallel = FALSE)

test_that("print", {
  expect_equal(capture.output(print(a)),
               c("", "5-fold cross validation ", "", "beta0 0.24249  beta1 0.16356  beta2 0.13207  "
               ))
})

test_that("summary", {
  expect_equal(capture.output(summary(a)),
               c("", "5-fold cross validation ", "", "      beta0     beta1     beta2     RMSE       MAE  MedianAE",
                 "1 0.2448897 0.1642169 0.1344469 1.185091 0.9507563 0.8232038",
                 "2 0.2457771 0.1620997 0.1313954 1.204117 0.9495751 0.7679286",
                 "3 0.2420357 0.1634702 0.1335895 1.328761 1.0622781 0.9263917",
                 "4 0.2384119 0.1638333 0.1334501 1.316033 1.0785496 0.9209200",
                 "5 0.2413538 0.1641947 0.1274519 1.329328 1.0540838 0.8986626"
               ))
})

test_that("plot", {
  expect_true("gtable" %in% class(plot(a)))
})


