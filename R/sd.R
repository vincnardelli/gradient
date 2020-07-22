#' Il wrap fatto a mano (SD però)
#'
#' @param b Beta init
#' @param formula formula
#' @param maxit max iteration
#' @param tolerance tole
#' @param verbose verbose
#'
#' @export
sd <- function(b,              # beta(0)
               formula,        # y~x
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
  out <- .Call(`_gradient_sd`, x, y, b, maxit, tolerance)
  class(out) <- "gradient"
  return(out)
}
