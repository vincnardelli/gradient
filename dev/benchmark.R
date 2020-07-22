
library(gradient)

n = 1000
x1 = rnorm(n)
x2 = rnorm(n)

#input
y = 1 + .5*x1 + .2*x2 + rnorm(n)
x <- cbind(x1, x2)
stepsize <- 1e-4
tolerance <- 1e-5
maxit <- 1e3
b <- c(0.1, 0.1, 0.1)
verbose <- TRUE
data <-  data.frame(y=y, x1=x1, x2=x2)

# new
sd <- lm_gradient(b, y~x1+x2, data, maxit = maxit,
                   tolerance = tolerance, fun="sd")
gd <- lm_gradient(b, y~x1+x2, data, maxit = maxit,
                   tolerance = tolerance, stepsize = stepsize, fun="gd")



linear_optim <- function(par,                    # beta(0)
                            formula,             # y~x
                            data = NULL,         # data
                            tolerance=1e-5,      # tolerance
                            stepsize = 1e-4,     # stepsize
                            maxit=1e3,          # max iteration, not to run forever
                            fun="sd",            # function
                            verbose=T            # should the function write messages during
) {

  # controls:
  .controls(tolerance, maxit, stepsize)

  if(is.null(data)){

    mod <- model.frame.default(formula)
    y <- model.response(mod, type="numeric")
    x <- as.matrix(cbind(1,subset(mod, select = -y )))

  }else{

    response <- as.character(attr(terms(formula), "variables"))[-1][attr(terms(formula), "response")] #[1] is the list call
    y <- data[,match(response, names(data))]
    vars <- all.vars(formula)[which(all.vars(formula)!= response)]
    mt <- match(vars, names(data))
    x <- model.matrix(~., data[mt])


  }

  i <- 0
  A <- matrix(0, nrow= maxit, ncol=3)

  if(fun=="gd"){
    while(i < maxit){

      if(abs(max(b)) > 1e300) stop("Beta too large: decrease the step size!")

      grad <- 2*t(x)%*%(x%*%b-y)
      bp <- b
      b <- bp - grad*stepsize

      if(max(abs(b - bp)) < tolerance){
        if(verbose) message("Tolerance reached")
        break()
      }

      A[i,] <- b
      i <- i + 1

    }
  }
  if(fun=="sd"){
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

    A[i, ] <- b
    i <- i + 1
    }
  }
  return(list(b, A, i))
}

linear_optim(b, y~x1+x2, data, fun="gd")

lm_gradient(b, y~x1+x2, data, fun="gd")

lm_gradient(b, y~x1+x2, data, fun="sd")

bm <- bench::mark(gd_R = linear_optim(b, y~x1+x2, data, fun="gd"),
                  sd_R = linear_optim(b, y~x1+x2, data, fun="sd"),
                  gd_C = lm_gradient(b, y~x1+x2, data, fun="gd"),
                  sd_C = lm_gradient(b, y~x1+x2, data, fun="sd"),
                  check=FALSE)
print(bm)
ggplot2::autoplot(bm)











