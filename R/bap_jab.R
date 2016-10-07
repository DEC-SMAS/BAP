#==============================================================================
#'BAP Jab
#'
#'@param Long = Taxonomic count data in a long data format.
#'@return Calculate and score the metrics in the BAP created for jabs.
#'@export

bap_jab <- function(Long){

  final_id.df <- wide(Long, "FINAL_ID")
  # Create a new data frame to store metrics
  metrics <- data.frame(unique(final_id.df[, 1:5]))
  #==============================================================================
  # Species Richness
  metrics$RICHNESS <- vegan::specnumber(final_id.df[, 6:ncol(final_id.df)])
  metrics$RICH_SCORE <- score_rich_jab(metrics)
  #==============================================================================
  # EPT Richness
  metrics$EPT_RICH <- ept_rich(Long, "FINAL_ID")
  metrics$EPT_SCORE <- score_ept_jab(metrics)
  #==============================================================================
  # Hilsenhoff's Biotic Index (HBI)
  metrics$HBI <- tol_index(Long, Index = "TOLERANCE", Level = "FINAL_ID")
  metrics$HBI_SCORE <- score_hbi_jab(metrics)
  #==============================================================================
  # Non-Chironomidae/Oligochaeta Richness (NCO Richenss)
  metrics$NCO_RICH <- rich_nco(Long, Level = "GENUS")
  metrics$NCO_RICH_SCORE <- score_nco_jab(metrics)
  #==============================================================================
  # Find the mean score of all the metrics for each sample
  metrics$FINAL_SCORE <- apply(metrics[, c("RICH_SCORE", "EPT_SCORE", "HBI_SCORE",
                                           "NCO_RICH_SCORE")], 1, FUN = mean)
  # Round to the hundredths place
  metrics$FINAL_SCORE <- round(metrics$FINAL_SCORE, digits = 2)

  return(metrics)
}
