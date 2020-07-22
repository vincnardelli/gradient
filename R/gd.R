#' Il wrap fatto a mano
#'
#' @param b Beta init
#' @param formula formula
#' @param stepsize stepsize
#' @param maxit max iteration
#' @param tolerance tolerance
#' @param verbose verbose
#'
#' @export
gd <- function(b,              # beta(0)
              formula,        # y~x
              stepsize=1e-3,  # tolerance
              maxit=1000,     # max iteration, not to run forever
              tolerance=1e-3, # stepsize parameter
              verbose=T       # should the function write messages during
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
  out <- .Call(`_gradient_gd`, x, y, b, stepsize, maxit, tolerance)
  class(out) <- "gradient"
  return(out)
}
