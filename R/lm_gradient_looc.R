#' Leave one out cross validation for Linear Models with gradient
#'
#' \code{lm_gradient_looc} is used to perform a leave one out cross validation to a linear model with gradient or steepest descent.
#'
#' @param b initial values for beta
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data an optional data frame, list or environment. If not found in data, the variables are taken from environment(formula)
#' @param maxit number of max iterations of the algorithm
#' @param tolerance the algorithm will stop if tolerance is reached
#' @param stepsize size of each step (only for gradient descent)
#' @param parallel perform the cross validation in multi-core
#' @param fun function can be \code{sd} for steepest descent or \code{gd} for gradient descent
#'
#' @return the function returns an object of class \code{"gradient"}
#' @export
lm_gradient_looc <- function(b, formula, data=NULL, maxit=1000,     # max iteration, not to run forever
                             tolerance=1e-5, # tolerance parameter
                             stepsize=1e-4,  # stepsize parameter
                             fun = "sd",
                             parallel=FALSE){

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

  out <- lm_gradient_cv(n.folds=nrow(x),
                                    b=b,
                                    formula=formula,
                                    data=data,
                                    maxit=maxit,     # max iteration, not to run forever
                                   tolerance=tolerance, # tolerance parameter
                                   stepsize=stepsize,  # stepsize parameter
                                   fun = fun,
                                   parallel=parallel)

  return(out)
}
