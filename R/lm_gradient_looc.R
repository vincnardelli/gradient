#' Title
#'
#' @param b b
#' @param formula formula
#' @param data data
#' @param maxit maxit
#' @param tolerance tol
#' @param stepsize a
#' @param fun a
#' @param parallel a
#'
#' @return a
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
  class(out) <- "gradient_cv"

  return(out)
}
