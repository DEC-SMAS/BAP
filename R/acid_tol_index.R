#==============================================================================
#'Acid Tolerance Index (ATI)
#'
#'@param Genus = Genus level taxa count data.
#'@return The percentage of Acid Tolerant Individuals (ATI).
#'@export

pct_acid_tol <- function(Genus){
  acid_taxa <- c("EPEORUS", "AMPHINEMURA", "LEUCTRA", "ISOPERLA", "RHYACOPHILA",
                 "SIMULIUM", "CONCHAPELOPIA", "CRICOTOPUS", "EUKIEFFERIELLA",
                 "HETEROTRISSOCLADIUS")
  final.df <- (rowSums(Genus[, names(Genus) %in% acid_taxa]) /
                 rowSums(Genus[, 6:ncol(Genus)])) * 100
  return(round(final.df, digits = 2))
}
