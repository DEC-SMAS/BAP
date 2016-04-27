#==============================================================================
#'Percentage of a Taxon
#'
#'@param Taxon = The taxon of interest in quotations and in all caps
#' (e.g. "EPEORUS")
#'@param Level = The taxonomic level (rank) the taxon of intrest can be found.
#'@param Long = Taxonomic count data in a long data format.
#'@return Calculate the percentage of a particular taxon.
#'@export

pct_taxon <- function(Taxon, Level, Long) {
  Long[, Taxon] <- ifelse(Long[, Level] %in% Taxon, Long$REPORTING_VALUE, 0)
  sum.total <- aggregate(REPORTING_VALUE ~ EVENT_ID,
                         FUN = sum, na.rm = TRUE, data = Long)
  sum.taxon <- aggregate(Long[, Taxon] ~ EVENT_ID,
                         FUN = sum, na.rm = TRUE, data = Long)
  merged <- merge(sum.taxon, sum.total, by = "EVENT_ID")
  names(merged) <- c("EVENT_ID", Taxon, "TOTAL")
  merged$FINAL <- (merged[, Taxon] / merged$TOTAL) * 100

  return(merged$FINAL)
}

#==============================================================================
#'Percent Model Affinity (PMA)
#'
#'@param Long = Taxonomic count data in a long data format.
#'@return The similarity of the observed community to a model community.
#'@export

pma <- function(Long){
  new <- data.frame(unique(Long$EVENT_ID))
  new$OLIGOCHAETA <- pct_taxon("OLIGOCHAETA", "CLASS", Long)
  new$EPHEMEROPTERA <- pct_taxon("EPHEMEROPTERA", "ORDER", Long)
  new$PLECOPTERA <- pct_taxon("PLECOPTERA", "ORDER", Long)
  new$COLEOPTERA <- pct_taxon("COLEOPTERA", "ORDER", Long)
  new$TRICHOPTERA <- pct_taxon("TRICHOPTERA", "ORDER", Long)
  new$CHIRONOMIDAE <- pct_taxon("CHIRONOMIDAE", "FAMILY", Long)
  taxa.list <- c("OLIGOCHAETA", "EPHEMEROPTERA", "PLECOPTERA",
                 "COLEOPTERA", "TRICHOPTERA", "CHIRONOMIDAE")
  new$OTHER <- 100 - rowSums(new[, taxa.list])

  pma.final <- pmin(pma.df[pma.df$ORDER == "OLIGOCHAETA", 2], new$OLIGOCHAETA) +
    pmin(pma.df[pma.df$ORDER == "EPHEMEROPTERA", 2], new$EPHEMEROPTERA) +
    pmin(pma.df[pma.df$ORDER == "PLECOPTERA", 2], new$PLECOPTERA) +
    pmin(pma.df[pma.df$ORDER == "COLEOPTERA", 2], new$COLEOPTERA) +
    pmin(pma.df[pma.df$ORDER == "TRICHOPTERA", 2], new$TRICHOPTERA) +
    pmin(pma.df[pma.df$ORDER == "CHIRONOMIDAE", 2], new$CHIRONOMIDAE) +
    pmin(pma.df[pma.df$ORDER == "OTHER", 2], new$OTHER)

  return(round(pma.final, digits = 0))
}
