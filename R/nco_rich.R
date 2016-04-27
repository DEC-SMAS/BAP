#==============================================================================
#'Non-Chironomidae and Oligochaeta Taxa Richness
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param Level = The taxonomic level to preform the analysis.  This metric must
#'specify a taxonomic rank below family.
#'@return Taxa richness excluding Chironomidae or Oligochaeta taxa.
#'@export
#'

rich_nco <- function(Long, Level = "GENUS"){
  Taxa.df <- wide(Long, Level)
  FAM <- split(Long[, Level], Long$FAMILY)
  taxa.list_c <- unique(FAM$CHIRONOMIDAE)
  CLASS <- split(Long[, Level], Long$CLASS)
  taxa.list_o <- unique(CLASS$OLIGOCHAETA)
  taxa.list <- list(taxa.list_c, taxa.list_o)
  group.rich <- group_rich(taxa.list, Taxa.df)
  total.rich <- vegan::specnumber(Taxa.df[, 6:ncol(Taxa.df)])
  return(total.rich - group.rich)
}
