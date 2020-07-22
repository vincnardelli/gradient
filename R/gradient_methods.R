#' Print
#' prints its argument
#' @param x  object of gradient class
#' @param ... further arguments passed to or from other methods
#'
#' @return object of class gradient
print.gradient <- function(x, ...) {
  for (i in 0:(length(x$b)-1)){
    cat(paste0(expression(beta), i), round(x$b[i+1], 5), ' ')
  }
  cat("\n")
}

#' Summary
#'
#' Summarize an analysis of the fitted model
#' @param object  object of gradient class
#' @param ... further arguments passed to or from other methods
#'
#' @return object of class gradient
#' @export
summary.gradient <- function(object, ...){

  tabella <- data.frame(coeff = rep(NA, length(object$b)),
                        estimate = rep(NA, length(object$b)),
                        sd = rep(NA, length(object$b)),
                        t.value = rep(NA, length(object$b)),
                        p.value = rep(NA, length(object$b))
  )

  for (i in 0:(length(object$b)-1)){
    tabella[i+1, 1] <- paste0("beta", i)
    tabella[i+1, 2] <- round(object$b[i+1], 5)
    tabella[i+1, 3] <- round(sqrt(var((object$A[1:object$i, i+1]))), 5)
    tabella[i+1, 4] <- tabella[i+1, 2]/tabella[i+1, 3]
    tabella[i+1, 5] <- 2*(1-pt(tabella[i+1, 4], nrow(object$A) - length(object$b) - 1))
  }
  print(tabella)
  cat("\n")

}


#' Extract Model Coefficients
#'
#' generic function which extracts model coefficients from objects returned by modeling functions
#' @param object object
#' @param ... further arguments passed to or from other methods
#'
#' @return list with coefficients
#' @export
coef.gradient <- function(object, ...){
  as.vector(object$b)
}


#' Plot gradient convergence
#'
#' @param ... further arguments passed to or from other methods
#' @param x object of gradient class
#'
#' @return graphics in ggplot
#' @export
plot.gradient <- function(x, ...){

  i <- b <- value <- variable <- NULL # solve global variable note
  dg <- as.data.frame(x$A)[1:x$i,]
  dg$i <- 1:nrow(dg)
  dg <- rbind(c(b, 0), dg)

  dg <- dg %>%
    tidyr::pivot_longer(cols=-i, names_to = "variable", values_to = "value")

  gg <- ggplot2::ggplot(dg, ggplot2::aes(x = i, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = variable, linetype = variable))

  gg
}


#' Predict
#'
#' @param object  object of gradient class
#' @param ... further arguments passed to or from other methods.
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#'
#' @return  vector of predictions or a matrix of predictions
#' @export
predict.gradient <- function(object, newdata, ...){

  formula <- object$call
  coeff <- object$b
  mm <- model.matrix(formula)
  tt <- terms(formula)
  aa <- attr(mm, "assign")
  ll <- attr(tt, "term.labels")
  mat <- match(ll, names(newdata))

  y_pred <- apply(coeff*newdata[, mat], 1, sum)

  return(as.data.frame(y_pred))
}
