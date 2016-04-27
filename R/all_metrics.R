#==============================================================================
#'Calculate All Metrics
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param isd.df = A data frame containing all of the Impact Source
#' Determination (ISD) models.
#'@return Calculates all of the metrics in the 2014 NYSDEC SOP.
#'@export

all_metrics <- function(Long, isd.df){

  final_id.df <- wide(Long, "SAMPLE_GENUS_SPECIES")
  # Create a new data frame to store metrics
  metrics <- data.frame(final_id.df[, 1:5])
  #==============================================================================
  # Species Richness
  metrics$RICHNESS <- vegan::specnumber(final_id.df[, 6:ncol(final_id.df)])
  #==============================================================================
  # EPT Richness
  metrics$EPT_RICH <- ept_rich(Long, "SAMPLE_GENUS_SPECIES")
  #==============================================================================
  # Hilsenhoff's Biotic Index (HBI)
  metrics$HBI <- tol_index(Long, Index = "TOLERANCE", Level = "SAMPLE_GENUS_SPECIES")
  #==============================================================================
  # Percent Model Affinity (PMA)

  metrics$PMA <- pma(Long)
  #==============================================================================
  # Shannon-Wiener Diversity
  metrics$SHANNON <- vegan::diversity(final_id.df[, 6:ncol(final_id.df)])

  #==============================================================================
  # Dominance-3
  metrics$PCT_DOM3 <- pct_dom3(final_id.df)
  #==============================================================================
  # Non-Chironomidae and Oligochaeta (NCO) Richness
  metrics$NCO_RICH <- rich_nco(Long, Level = "GENUS")
  #==============================================================================
  # Nutrient Biotic Index (NBI)
  metrics$NBI_P <- tol_index(Long, Index = "NBI_P_TOLERANCE",
                             Level = "SAMPLE_GENUS_SPECIES")
  metrics$NBI_N <- tol_index(Long,  Index = "NBI_N_TOLERANCE",
                             Level = "SAMPLE_GENUS_SPECIES")
  #==============================================================================
  # Percent Mayfly Richness (PMR)
  genus.df <- wide(Long, "GENUS")
  metrics$PCT_EPHEM_RICH <- pct_rich_ephem_epeorus(Long, genus.df)
  #==============================================================================
  # Acid Tolerance Index (ATI)
  metrics$PCT_ATI <- pct_acid_tol(genus.df)
  #==============================================================================
  # Impact Source Determination (ISD)
  ISD <- calc.isd(Long, isd.df)
  #==============================================================================
  # Merge the ISD data frame with the metrics data frame
  final.df <- merge(metrics, ISD, by = "EVENT_ID", all = TRUE)

  return(final.df)
}
