#==============================================================================
#'BAP Ponar
#'
#'@param long = Taxonomic count data in a long data format.
#'@return Calculate and score the metrics in the BAP created for ponars.
#'@export

bap_ponar <- function(long){

  final_id.df <- wide(long, "FINAL_ID")
  # Create a new data frame to store metrics
  metrics <- data.frame(unique(final_id.df[, 1:5]))
  #==============================================================================
  # Species Richness
  metrics$RICHNESS <- vegan::specnumber(final_id.df[, 6:ncol(final_id.df)])
  metrics$RICH_SCORE <- score_rich_ponar(metrics)
  #==============================================================================
  # Hilsenhoff's Biotic Index (HBI)
  metrics$HBI <- tol_index(long, Index = "TOLERANCE", Level = "FINAL_ID")
  metrics$HBI_SCORE <- score_hbi_ponar(metrics)
  #==============================================================================
  # Percent Model Affinity (PMA)
  metrics$PMA <- pma_ponar(long)
  metrics$PMA_SCORE <- score_pma_ponar(metrics)
  #==============================================================================
  # Shannon-Wiener Species Diversity
  metrics$SHANNON <- vegan::diversity(final_id.df[, 6:ncol(final_id.df)], base = 2)
  metrics$SHANNON_SCORE <- score_shannon_ponar(metrics)
  #==============================================================================
  # DOM-3
  metrics$PCT_DOM3 <- pct_dom3(final_id.df)
  metrics$PCT_DOM3_SCORE <- score_pct_dom3_ponar(metrics)
  #==============================================================================
  # Find the mean score of all the metrics for each sample
  metrics$FINAL_SCORE <- apply(metrics[, c("RICH_SCORE", "SHANNON_SCORE",
                                           "HBI_SCORE", "PMA_SCORE",
                                           "PCT_DOM3_SCORE")], 1, FUN = mean)
  # Round to the hundredths place
  metrics$FINAL_SCORE <- round(metrics$FINAL_SCORE, digits = 2)

  return(metrics)
}
