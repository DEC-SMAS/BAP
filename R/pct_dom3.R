#==============================================================================
#'Percentage of the 3rd Dominant Family
#'
#'@param Taxa.df = Wide data frame format of taxonomic counts.
#'@return Percent of individuals that represent the first, second, and third
#' most abundant taxa.
#'@export
#'
pct_dom3 <- function(Taxa.df) {
  maxn <- function(n) function(p) order(p, decreasing = TRUE)[n]
  taxa.col <- Taxa.df[, 6:ncol(Taxa.df)]
  #n <- colnames(taxa.col[(apply(taxa.col, 1, maxn(1)))])
  DOM1 <- apply(taxa.col, 1, function(p) p[maxn(1)(p)])
  DOM2 <- apply(taxa.col, 1, function(p) p[maxn(2)(p)])
  DOM3 <- apply(taxa.col, 1, function(p) p[maxn(3)(p)])

  final.df <- (DOM1 + DOM2 + DOM3) / rowSums(taxa.col) * 100
  return(round(final.df, digits = 2))
}
