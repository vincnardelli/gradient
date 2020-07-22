set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)

maxit = 1000;

y = 1 + .5*x1 + .2*x2 + rnorm(n)

b <- c(0.1, 0.1, 0.1)
stepsize <- 1e-5
tolerance <- 1e-3


linear_gd_optim <- function(par,             # beta(0)
                            formula,         # y~x
                            data,            # data
                            tolerance=1e-3,  # tolerance
                            maxit=1000,      # max iteration, not to run forever
                            stepsize=1e-3,   # stepsize parameter
                            verbose=T        # should the function write messages during
) {

  # controls:
  if(!is.numeric(tolerance)){
    stop("Tolerance must be numeric")
  }

  if(!is.numeric(maxit)){
    stop("Max Iteration (maxit) must be numeric")
  }

  if(!is.numeric(stepsize)){
    stop("Step size must be numeric")
  }

  formula <- formula(formula)
  mod <- model.frame.default(formula)
  y <- as.numeric(model.response(mod, type="numeric"))
  x <- as.matrix(cbind(1,subset(mod, select = -y )))

  i <- 0

  while(i < maxit){

    if(abs(max(b)) > 1e300) stop("Beta too large: decrease the step size!")

    grad <- 2*t(x)%*%(x%*%b-y)
    bp <- b
    b <- bp - grad*stepsize

    if(max(abs(b - bp)) < tolerance){
      if(verbose) message("Tolerance reached")
      break()
    }

    i <- i + 1
  }

  return(b)
}




linear_sd_optim <- function(par,             # beta(0)
                            formula,         # y~x
                            tolerance=1e-3,  # tolerance
                            maxit=1000,      # max iteration, not to run forever
                            verbose=T        # should the function write messages during
) {

  # controls:
  if(!is.numeric(tolerance)){
    stop("Tolerance must be numeric")
  }

  if(!is.numeric(maxit)){
    stop("Max Iteration (maxit) must be numeric")
  }

  formula <- formula(formula)
  mod <- model.frame.default(formula)
  y <- as.numeric(model.response(mod, type="numeric"))
  x <- as.matrix(cbind(1,subset(mod, select = -y )))

  i <- 0

  while(i < maxit){

    if(abs(max(b)) > 1e300) stop("Beta too large: decrease the step size!")

    hessian <- 4*t(x)%*%x
    grad <- 2*t(x)%*%(x%*%b-y)
    bp <- b
    step <- sum(grad^2)/(t(grad)%*%hessian%*%grad)
    b <- bp - grad%*%step

    if(max(abs(b - bp)) < tolerance){
      if(verbose) message("Tolerance reached")
      break()
    }

    i <- i + 1
  }

  return(list(b,i))
}


bm <- bench::mark(R_gd = linear_gd_optim(b, y~x1+x2, tolerance, maxit, stepsize),
                  R_sd = linear_sd_optim(b, y~x1+x2, tolerance, maxit),
                  Cpp_gd = gd(b, y~x1+x2, stepsize, maxit, tolerance),
                  Cpp_sd = sd(b, y~x1+x2, maxit, tolerance),
                  check=FALSE)
print(bm)
ggplot2::autoplot(bm)





