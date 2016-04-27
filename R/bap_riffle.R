#==============================================================================
#'BAP Riffle
#'
#'@param Long = Taxonomic count data in a long data format.
#'@return Calculate and score the metrics in the BAP created for riffles.
#'@export

bap_riffle <- function(Long){

  final_id.df <- wide(Long, "SAMPLE_GENUS_SPECIES")
  # Create a new data frame to store metrics
  metrics <- data.frame(unique(Long[, 1:5]))
  #==============================================================================
  # Species Richness
  metrics$RICHNESS <- vegan::specnumber(final_id.df[, 6:ncol(final_id.df)])
  metrics$RICH_SCORE <- score_rich_riffle(metrics)
  #==============================================================================
  # EPT Richness
  metrics$EPT_RICH <- ept_rich(Long, "SAMPLE_GENUS_SPECIES")
  metrics$EPT_SCORE <- score_ept_riffle(metrics)
  #==============================================================================
  # Hilsenhoff's Biotic Index (HBI)
  metrics$HBI <- tol_index(Long, Index = "TOLERANCE", Level = "SAMPLE_GENUS_SPECIES")
  metrics$HBI_SCORE <- score_hbi_riffle(metrics)
  #==============================================================================
  # Percent Model Affinity (PMA)
  metrics$PMA <- pma(Long)
  metrics$PMA_SCORE <- score_pma_riffle(metrics)
  #==============================================================================
  # Nutrient Biotic Index (NBI)
  metrics$NBI_P <- tol_index(Long, Index = "NBI_P_TOLERANCE",
                             Level = "SAMPLE_GENUS_SPECIES")

  metrics$NBI_P_SCORE <- score_nbip_riffle(metrics)
  #==============================================================================
  # Find the mean score of all the metrics for each sample
  metrics$FINAL_SCORE <- apply(metrics[, c("RICH_SCORE", "EPT_SCORE", "HBI_SCORE",
                                           "PMA_SCORE", "NBI_P_SCORE")], 1, FUN = mean)
  # Round to the hundredths place
  metrics$FINAL_SCORE <- round(metrics$FINAL_SCORE, digits = 2)

  return(metrics)
}
