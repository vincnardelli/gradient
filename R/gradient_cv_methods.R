#' Title
#'
#' @param x object
#' @param ... othjers
#'
#' @return a
#' @export
print.gradient_cv <- function(x, ...) {
  cat('\n')
  cat(paste0(nrow(x$validation),"-fold cross validation", ""), "\n")
  cat('\n')
  for (i in 0:(length(x$b)-1)){
    cat(paste0(expression(beta), i), round(x$b[i+1], 5), ' ')
  }
  cat("\n")
}

#' Title
#'
#' @param x object
#' @param ... othjers
#'
#' @return a
#' @export
summary.gradient_cv <- function(x, ...){
  cat('\n')
  cat(paste0(nrow(x$validation),"-fold cross validation", ""), "\n")
  cat('\n')
  x$validation
}

#' Title
#'
#' @param x object
#' @param ... othjers
#'
#' @return a
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
