#' Title
#'
#' @param x object
#' @param ... othjers
#'
#' @return
#' @export

print.gradient <- function(x, ...) {
  for (i in 0:(length(x$b)-1)){
    cat(paste0(expression(beta), i), round(x$b[i+1], 5), ' ')
  }
  cat("\n")
}

#' Title
#'
#' @param object oggetto
#' @param ... others
#'
#' @return
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


#' Title
#'
#' @param object object
#' @param ... others
#'
#' @return
#' @export
coef.gradient <- function(object, ...){
  as.vector(object$b)
}
