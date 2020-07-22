#' Linear model gradient cross validation
#'
#' @param n.folds nfold
#' @param formula formula
#' @param data data
#' @param fun fun
#' @param b b
#' @param maxit max ite
#' @param tolerance tol
#' @param stepsize step size
#' @param parallel parallel
#'
#' @return something
#' @export
#'

lm_gradient_cv <- function(n.folds, b, formula, data=NULL, maxit=1000,     # max iteration, not to run forever
                           tolerance=1e-5, # tolerance parameter
                           stepsize=1e-4,  # stepsize parameter
                           fun = "sd",
                           parallel=FALSE) {

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


  folds_i <- sample(rep_len(1:n.folds, length.out = length(y)))
  k <- NULL # solves global variable node in package check

  fold <- function(k){
    test_i <- which(folds_i == k)
    train_x <- x[-test_i, ]
    test_x <- x[test_i, ]
    y_test <- y[test_i]
    y_train <- y[-test_i]

    if(fun=="gd"){
      out <- .Call(`_gradient_gd`, train_x, y_train, b, stepsize, maxit, tolerance)
    }
    if(fun=="sd"){
      out <- .Call(`_gradient_sd`, train_x, y_train, b, maxit, tolerance)
    }

    beta <- out$b
    pred <- test_x%*%beta
    return(c(out$b,
             sqrt(mean((y_test - pred)^2)),
             mean(abs(y_test - pred)),
             median(abs(y_test - pred))))
  }


  if(parallel){
    cl <- parallel::makeCluster(parallel::detectCores())

    doParallel::registerDoParallel(cl)
    validation <- foreach::foreach(k = 1:n.folds, .combine = 'rbind') %dopar% {
      fold(k)
    }
    parallel::stopCluster(cl)
  }else{
    validation <- foreach::foreach(k = 1:n.folds, .combine = 'rbind') %do% {
      fold(k)
    }
  }

  colnames(validation) <- c(sapply(1:length(b), function(x) paste0("beta", x-1)),
                        "RMSE", "MAE", "MedianAE")
  rownames(validation) <- 1:n.folds


  out <- list(b = apply(validation[, 1:length(b)], 2, mean),
              validation = validation,
              call = formula,
              fun = fun)
return(out)
}
