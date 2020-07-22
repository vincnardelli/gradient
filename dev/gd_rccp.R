
setwd("E:/PhD/R for Data Science/prova_cpp")
library(Rcpp)
sourceCpp('gd.cpp')
sourceCpp('sd.cpp')

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



linear_gd_wrap <- function( b,              # beta(0)
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
  list <- gd(x=x, y=y, b, stepsize, maxit, tolerance)
  
  return(list)
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


linear_sd_wrap <- function( b,              # beta(0)
                            formula,        # y~x
                            data,           # data.frame
                            maxit=1000,     # max iteration, not to run forever
                            tolerance=1e-5, # stepsize parameter
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
  list <- sd(x=x, y=y, b, maxit, tolerance)
  
  return(list)
}

x <- cbind(1, x1, x2)

bm <- bench::mark(gd_cpp = gd(x, y, b, stepsize, maxit, tolerance), 
                  sd_cpp = sd(x, y, b, maxit, tolerance),
                  linear_gd = linear_gd_optim(b, y~x1+x2, tolerance, maxit, stepsize), 
                  linear_sd = linear_sd_optim(b, y~x1+x2, tolerance, maxit),
                  linear_gd_wrap = linear_gd_wrap(b, y~x1+x2, stepsize, maxit, tolerance),
                  linear_sd_wrap = linear_sd_wrap(b, y~x1+x2, maxit, tolerance),
                  check=FALSE)
print(bm)
ggplot2::autoplot(bm)


# cross-validation --------------------------------------------------------

library(tictoc)

# Sequential:

cv.perform <- function(n.folds, formula, data, fun = "sd") {
  
  formula <- formula(formula)
  mod <- model.frame.default(formula)
  y <- as.numeric(model.response(mod, type="numeric"))
  x <- as.matrix(cbind(1,subset(mod, select = -y )))

  folds_i <- sample(rep_len(1:n.folds, length.out = length(y)))
  cv_tmp <- matrix(NA, nrow = n.folds, ncol = 3)
  colnames(cv_tmp) <- c("RMSE", "MAE", "MedianAE")
  
  for (k in 1:n.folds) {
    
    test_i <- which(folds_i == k)
    train_x <- x[-test_i, ]
    test_x <- x[test_i, ]
    y_test <- y[test_i]
    y_train <- y[-test_i]
    
    if(fun=="gd"){
      
      method <- gd(train_x, y_train, b, stepsize, maxit, tolerance)
      
    }
    
    if(fun=="sd"){
      
      method <- sd(x = train_x, y = y_train, b, maxit = maxit, tolerance = tolerance)
      
    }
    
    beta <- method$b
    pred <- test_x%*%beta
    cv_tmp[k, 1] <- sqrt(mean((y_test - pred)^2))
    cv_tmp[k, 2] <- mean(abs(y_test - pred))
    cv_tmp[k, 3] <- median(abs(y_test - pred))
    
    
  }
  
  return(cv_tmp)
}

tic()
cv.seq <- cv.perform(10, formula(y~x1+x2), "gd")
toc()

tic()
cv.perform(10, formula(y~x1+x2), "sd")
toc()


# Parallel:

cv.parallel <- function(n.folds, formula, fun = "sd", data, stepsize = 1e-3, maxit=10000, tolerance=1e-3) {
  cluster <-  parallel::makeCluster(parallel::detectCores(), type = "SOCK")
  doSNOW::registerDoSNOW(cluster)
  
  k <- foreach::foreach(i = 1:n.folds,
               .combine = '+',
               .export = c("gd","sd")
               ) %dopar% {
  
  mod <- model.frame.default(formula)
  y <- as.numeric(model.response(mod, type="numeric"))
  x <- as.matrix(cbind(1,subset(mod, select = -y )))
  
  folds_i <- sample(rep_len(1:n.folds, length.out = length(y)))
  cv_tmp <- matrix(NA, nrow = n.folds, ncol = 3)
  colnames(cv_tmp) <- c("RMSE", "MAE", "MedianAE")
  
  for (k in 1:n.folds) {
    
    test_i <- which(folds_i == k)
    train_x <- x[-test_i, ]
    test_x <- x[test_i, ]
    y_test <- y[test_i]
    y_train <- y[-test_i]
    
    if(fun=="gd"){
      
      method <- gd(train_x, y_train, b, stepsize, maxit, tolerance)
      
    }
    
    if(fun=="sd"){
      
      method <- sd(x = train_x, y = y_train, b, maxit = maxit, tolerance = tolerance)
      
    }
    
    beta <- method$b
    pred <- test_x%*%beta
    cv_tmp[k, 1] <- sqrt(mean((y_test - pred)^2))
    cv_tmp[k, 2] <- mean(abs(y_test - pred))
    cv_tmp[k, 3] <- median(abs(y_test - pred))
    
    
  }
  parallel::stopCluster(cluster)
  return(cv_tmp)
  }
  
}


cv.parallel(10, formula(y~x1+x2))

tic()

cv.perform(10, formula(y~x1+x2), "gd")

toc()

tic()

cv.perform(10, formula(y~x1+x2), "sd")

toc()


par.gd <- system.time(cv.perform(10, formula(y~x1+x2), "gd"))[3]
par.sd <- system.time(cv.perform(10, formula(y~x1+x2), "sd"))[3]


stopCluster(cluster)



seq.gd <- system.time(cv.perform(10, formula(y~x1+x2), "gd"))[3]
seq.sd <- system.time(cv.perform(10, formula(y~x1+x2), "sd"))[3]

cat(seq.gd, "time sequential cv method gd\n")
cat(par.gd, "time parallel cv method gd\n")
cat(seq.sd, "time sequential cv method sd\n")
cat(par.sd, "time parallel cv method sd\n")



# Classi ------------------------------------------------------------------


mod.gd <- linear_gd_wrap(b, formula(y~x1+x2), stepsize, maxit, tolerance)

class(mod.gd) <- "gd"

class(mod.gd)


print.gd <- function(obj) {
  for (i in 0:(length(obj$b)-1)){
    cat(paste0(expression(beta), i), round(obj$b[i+1], 5), ' ')
    }
}

print(mod.gd)


summary.gd <- function(obj){
  
  tabella <- data.frame(coeff = rep(NA, length(obj$b)), 
                        estimate = rep(NA, length(obj$b)),
                        sd = rep(NA, length(obj$b)),
                        t.value = rep(NA, length(obj$b)),
                        p.value = rep(NA, length(obj$b))
                        )

  for (i in 0:(length(obj$b)-1)){
    tabella[i+1, 1] <- paste0("beta", i)
    tabella[i+1, 2] <-  round(obj$b[i+1], 5)
    tabella[i+1, 3] <- round(sqrt(var((obj$A[1:obj$i, i+1]))), 5)
    tabella[i+1, 4] <- tabella[i+1, 2]/tabella[i+1, 3]
    tabella[i+1, 5] <- 2*(1-pt(tabella[i+1, 4], nrow(obj$A) - length(obj$b) - 1))
  }
  
  tabella
}

summary(mod.gd)




