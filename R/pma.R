#==============================================================================
#'Percentage of a Taxon
#'
#'@param Taxon = The taxon of interest in quotations and in all caps
#' (e.g. "EPEORUS")
#'@param Level = The taxonomic level (rank) the taxon of intrest can be found.
#'@param long = Taxonomic count data in a long data format.
#'@return Calculate the percentage of a particular taxon.
#'@export

pct_taxon <- function(Taxon, Level, long) {
  long[, Taxon] <- ifelse(long[, Level] %in% Taxon, long$REPORTING_VALUE, 0)
  sum.total <- aggregate(REPORTING_VALUE ~ EVENT_ID,
                         FUN = sum, na.rm = TRUE, data = long)
  sum.taxon <- aggregate(long[, Taxon] ~ EVENT_ID,
                         FUN = sum, na.rm = TRUE, data = long)
  merged <- merge(sum.taxon, sum.total, by = "EVENT_ID")
  names(merged) <- c("EVENT_ID", Taxon, "TOTAL")
  merged$FINAL <- (merged[, Taxon] / merged$TOTAL) * 100

  return(merged$FINAL)
}

#==============================================================================
#'Percent Model Affinity (PMA)
#'
#'@param long = Taxonomic count data in a long data format.
#'@param pma.df = The PMA table.
#'@return The similarity of the observed community to a model community.
#'@export

pma <- function(long, pma.df = pma.model){
  new <- data.frame(unique(long$EVENT_ID))
  new$OLIGOCHAETA <- pct_taxon("OLIGOCHAETA", "CLASS", long)
  new$EPHEMEROPTERA <- pct_taxon("EPHEMEROPTERA", "ORDER", long)
  new$PLECOPTERA <- pct_taxon("PLECOPTERA", "ORDER", long)
  new$COLEOPTERA <- pct_taxon("COLEOPTERA", "ORDER", long)
  new$TRICHOPTERA <- pct_taxon("TRICHOPTERA", "ORDER", long)
  new$CHIRONOMIDAE <- pct_taxon("CHIRONOMIDAE", "FAMILY", long)
  taxa.list <- c("OLIGOCHAETA", "EPHEMEROPTERA", "PLECOPTERA",
                 "COLEOPTERA", "TRICHOPTERA", "CHIRONOMIDAE")
  new$OTHER <- 100 - rowSums(new[, taxa.list])
  #============================================================================
  pma.oligo  <- pma.df[pma.df$ORDER %in% "OLIGOCHAETA", 2]
  pma.ephem  <- pma.df[pma.df$ORDER %in% "EPHEMEROPTERA", 2]
  pma.plecop  <- pma.df[pma.df$ORDER %in% "PLECOPTERA", 2]
  pma.coleop  <- pma.df[pma.df$ORDER %in% "COLEOPTERA", 2]
  pma.trichop  <- pma.df[pma.df$ORDER %in% "TRICHOPTERA", 2]
  pma.chiro  <- pma.df[pma.df$ORDER %in% "CHIRONOMIDAE", 2]
  pma.other  <- pma.df[pma.df$ORDER %in% "OTHER", 2]
  #============================================================================

  pma.final <- pmin(pma.oligo, new$OLIGOCHAETA) +
    pmin(pma.ephem, new$EPHEMEROPTERA) +
    pmin(pma.plecop, new$PLECOPTERA) +
    pmin(pma.coleop, new$COLEOPTERA) +
    pmin(pma.trichop, new$TRICHOPTERA) +
    pmin(pma.chiro, new$CHIRONOMIDAE) +
    pmin(pma.other, new$OTHER)

  return(round(pma.final, digits = 0))
}

#==============================================================================
#'Ponar Percent Model Affinity (PMA)
#'
#'@param long = Taxonomic count data in a long data format.
#'@param pma.df = The PMA table.
#'@return The similarity of the observed community to a model community.
#'@export

pma_ponar <- function(long, pma.df = pma.ponar){
  new <- data.frame(unique(long$EVENT_ID))
  new$OLIGOCHAETA <- pct_taxon("OLIGOCHAETA", "CLASS", long)
  new$MOLLUSCA <- pct_taxon("MOLLUSCA", "PHYLUM", long)
  new$CRUSTACEA <- pct_taxon("CRUSTACEA", "CLASS", long)
  new$INSECTA <- pct_taxon("INSECTA", "CLASS", long)
  new$CHIRONOMIDAE <- pct_taxon("CHIRONOMIDAE", "FAMILY", long)
  new$NON_CHIRO_INSECTA <- new$INSECTA - new$CHIRONOMIDAE
  taxa.list <- c("OLIGOCHAETA", "MOLLUSCA", "CRUSTACEA",
                 "NON_CHIRO_INSECTA", "CHIRONOMIDAE")
  new$OTHER <- 100 - rowSums(new[, taxa.list])
  #============================================================================
  pma.oligo  <- pma.df[pma.df$ORDER %in% "OLIGOCHAETA", 2]
  pma.moll  <- pma.df[pma.df$ORDER %in% "MOLLUSCA", 2]
  pma.crust <- pma.df[pma.df$ORDER %in% "CRUSTACEA", 2]
  pma.nci  <- pma.df[pma.df$ORDER %in% "NON_CHIRO_INSECTA", 2]
  pma.chiro  <- pma.df[pma.df$ORDER %in% "CHIRONOMIDAE", 2]
  pma.other  <- pma.df[pma.df$ORDER %in% "OTHER", 2]
  #============================================================================
  
  pma.final <- pmin(pma.oligo, new$OLIGOCHAETA) +
    pmin(pma.moll, new$MOLLUSCA) +
    pmin(pma.crust, new$CRUSTACEA) +
    pmin(pma.nci, new$NON_CHIRO_INSECTA) +
    pmin(pma.chiro, new$CHIRONOMIDAE) +
    pmin(pma.other, new$OTHER)
  
  return(round(pma.final, digits = 0))
}
