## AOV functions

#' Get p-value from an ANOVA model
#' @param model anova model
#' @return p-value
#' @examples
#' pval_aov(aov(mpg ~ cyl, mtcars))
#'

pval_aov <- function(model) {
  x <- summary(model)
  return(unlist(x[[1]][,5][1]))
}

#' Get R-squared from an ANOVA model
#' @param model anova model
#' @return R-squared
#' @examples
#' r2_aov(aov(mpg ~ cyl, mtcars))
#'

r2_aov <- function(model) {
  x <- summary(model)
  SST <- sum(x[[1]][,2])
  SSR <- x[[1]][,2][1]
  return(SSR/SST)
}
