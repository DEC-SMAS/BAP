#==============================================================================
#'BAP Multiple-Plate Samples Non-Navigable Waters
#'
#'@param Long = Taxonomic count data in a long data format.
#'@return Calculate and score the metrics in the BAP created for
#'multiple-plate samples collected in non-navigable waters.
#'@export

bap_mp_non_nav_water <- function(Long){

  final_id.df <- wide(Long, "SAMPLE_GENUS_SPECIES")
  # Create a new data frame to store metrics
  metrics <- data.frame(unique(Long[, 1:5]))
  #==============================================================================
  # Species Richness
  metrics$RICHNESS <- vegan::specnumber(final_id.df[, 6:ncol(final_id.df)])
  metrics$RICH_SCORE <- score_rich_mp_non_nav_water(metrics)
  #==============================================================================
  # EPT Richness
  metrics$EPT_RICH <- ept_rich(Long, "SAMPLE_GENUS_SPECIES")
  metrics$EPT_SCORE <- score_ept_mp_non_nav_water(metrics)
  #==============================================================================
  # Hilsenhoff's Biotic Index (HBI)
  metrics$HBI <- tol_index(Long, Index = "TOLERANCE", Level = "SAMPLE_GENUS_SPECIES")
  metrics$HBI_SCORE <- score_hbi_mp_non_nav_water(metrics)
  #==============================================================================
  # Shannon-Wiener Species Diversity
  metrics$SHANNON <- vegan::diversity(final_id.df[, 6:ncol(final_id.df)])
  metrics$SHANNON_SCORE <- score_shannon_mp_non_nav_water(metrics)
  #==============================================================================
  # Find the mean score of all metrics for each sample
  metrics$FINAL_SCORE <- apply(metrics[, c("RICH_SCORE", "EPT_SCORE", "HBI_SCORE",
                                           "SHANNON_SCORE")], 1, FUN = mean)
  # Round to the hundredths place
  metrics$FINAL_SCORE <- round(metrics$FINAL_SCORE, digits = 2)

  return(metrics)
}
