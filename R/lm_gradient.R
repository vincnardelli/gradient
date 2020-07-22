.controls <- function(tolerance, maxit, stepsize){

  if(!is.numeric(tolerance)){
    stop("Tolerance must be numeric")
  }

  if(!is.numeric(maxit)){
    stop("Max Iteration (maxit) must be numeric")
  }

  if(!is.numeric(stepsize)){
    stop("Step size must be numeric")
  }
}



#' Fitting Linear Models with gradient
#'
#' \code{lm_gradient} is used to fit linear model with gradient or steepest descent.
#'
#' @param b initial values for beta
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data an optional data frame, list or environment. If not found in data, the variables are taken from environment(formula)
#' @param maxit number of max iterations of the algorithm
#' @param tolerance the algorithm will stop if tolerance is reached
#' @param stepsize size of each step (only for gradient descent)
#' @param fun function can be \code{sd} for steepest descent or \code{gd} for gradient descent
#' @param verbose verbose
#'
#' @return the function returns an object of class \code{"gradient"}
#' @export

lm_gradient <- function(b,                  # beta(0)
                            formula,        # y~x
                            data=NULL,      # data.frame
                            maxit=1000,     # max iteration, not to run forever
                            tolerance=1e-5, # tolerance parameter
                            stepsize=1e-4,  # stepsize parameter
                            fun = "sd",     # method to use
                            verbose=T       # should the function write messages during
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

  if(fun=="gd"){
    out <- .Call(`_gradient_gd`, x, y, b, stepsize, maxit, tolerance)
  }else{
    out <- .Call(`_gradient_sd`, x, y, b, maxit, tolerance)

  }
  out$call <- formula
  out$fun <- fun
  class(out) <- "gradient"

  return(out)
}


