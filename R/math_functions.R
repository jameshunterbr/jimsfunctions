## AOV functions
pval_aov <- function(model) {
  x <- summary(model)
  return(unlist(x[[1]][,5][1]))
}

r2_aov <- function(model) {
  x <- summary(model)
  SST <- sum(x[[1]][,2])
  SSR <- x[[1]][,2][1]
  return(SSR/SST)
}
