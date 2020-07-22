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



linear_gradient <- function(b,              # beta(0)
                            formula,        # y~x
                            data=NULL,           # data.frame
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

    list <- gd(x=x, y=y, b, stepsize, maxit, tolerance)

  }else{
    list <- sd(x=x, y=y, b, maxit, tolerance)
  }

  return(list)
}


