#============================================================================
#'Ephemeroptera Richness
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param Level = The taxonomic level to preform the analysis.  This metric must
#'specify a taxonomic rank below order.
#'@return The number of Ephemeroptera taxa.
#'@export

rich_ephemeroptera <- function(Long, Level = "GENUS"){
  Order <- split(Long[, Level], Long$ORDER)
  taxa.list <- unique(Order$EPHEMEROPTERA)
  taxa.df <- wide(Long, Level)
  group.rich <- group_rich(taxa.list, taxa.df)
  return(group.rich)
}

#==============================================================================
#'Percent Ephemeroptera Richness minus Epeorus
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param Genus = Genus level taxa count data
#'@return The percentage of the taxa observed  represented by Ephemeroptera taxa
#' minus the genus Epeorus.
#'@export

pct_rich_ephem_epeorus <- function(Long, Genus){
  ephem.df <- ifelse(Genus$EPEORUS > 0,
                     rich_ephemeroptera(Long, "GENUS") - 1,
                     rich_ephemeroptera(Long, "GENUS"))
  final.df <- (ephem.df / vegan::specnumber(Genus[, 6:ncol(Genus)])) * 100
  return(round(final.df, digits = 2))
}
