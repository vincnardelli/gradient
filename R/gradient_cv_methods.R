#' Print
#' prints its argument
#' @param x  object of gradient_cv class
#' @param ... further arguments passed to or from other methods
#'
#' @return object of class gradient_cv
print.gradient_cv <- function(x, ...) {
  cat('\n')
  cat(paste0(nrow(x$validation),"-fold cross validation", ""), "\n")
  cat('\n')
  for (i in 0:(length(x$b)-1)){
    cat(paste0(expression(beta), i), round(x$b[i+1], 5), ' ')
  }
  cat("\n")
}

#' Summary
#'
#' Summarize an analysis of the fitted model
#' @param object  object of gradient_cv class
#' @param ... further arguments passed to or from other methods
#'
#' @return object of class gradient_cv
#' @export
summary.gradient_cv <- function(object, ...){
  cat('\n')
  cat(paste0(nrow(object$validation),"-fold cross validation", ""), "\n")
  cat('\n')
}

#' Plot gradient CV
#'
#' @param ... further arguments passed to or from other methods
#' @param x object of gradient_cv class
#'
#' @return graphics in ggplot
#' @export
plot.gradient_cv <- function(x, ...){

  i <- b <- value <- variable <- NULL # solve global variable note
  vali <- x$validation
  dg <- as.data.frame(vali)[, 1:(ncol(vali)-3)]
  dg$i <- 1:nrow(dg)
  dg <- dg %>%
    tidyr::pivot_longer(cols=-i, names_to = "variable", values_to = "value")

  gg <- ggplot2::ggplot(dg, ggplot2::aes(x = i, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = variable, linetype = variable))

  dg1 <- as.data.frame(vali)[, (ncol(vali)-2):ncol(vali)]
  dg1$i <- 1:nrow(dg1)

  dg1 <- dg1 %>%
    tidyr::pivot_longer(cols=-i, names_to = "variable", values_to = "value")

  gg1 <- ggplot2::ggplot(dg1, ggplot2::aes(x = i, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = variable, linetype = variable))+
    ggplot2::scale_color_manual(values = c("darkred", "blue", "darkmagenta"))

  plot1 <- gg
  plot2 <- gg1
  gridExtra::grid.arrange(plot1, plot2, ncol=2)

}
