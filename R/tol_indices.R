#==============================================================================
#Tolerance Metrics
#==============================================================================
#'Tolerance Indices (Hilsenhoff, NBI_P, NBI_N)
#'
#'@param Long = Taxonomic count data in a Long data format.
#'@param Index = The column with the tolerance values of interest.
#'@param Level = Taxonomic rank used for calculations.
#'@return The average tolerance score per taxon for each unique sampling event.
#'If taxon does not have an assigned tolerance value it is excluded from the analysis.
#'@export

# Long <- long.df
# Index <- "NBI_P_TOLERANCE"
# Level <- "FINAL_ID"

tol_index <- function(Long, Index = "TOLERANCE", Level = "SAMPLES_GENUS_SPECIES") {

  tol_am <- aggregate(Long[, Index] ~ EVENT_ID +
                        Long[, colnames(Long) == Level] +
                        REPORTING_VALUE,
                      FUN = mean,
                      # na.action = "na.pass",
                      na.rm = TRUE,
                      data = Long)
  colnames(tol_am) <- c("EVENT_ID", Level, "REPORTING_VALUE", Index)
  tol_am$MULT <- tol_am$REPORTING_VALUE * tol_am[, Index]

  new <- aggregate(MULT ~ EVENT_ID,
                   FUN = sum, na.rm = TRUE, data = tol_am)
  new2 <- aggregate(REPORTING_VALUE ~ EVENT_ID,
                    FUN = sum, na.rm = TRUE, data = tol_am)

  merged <- merge(new, new2, by ="EVENT_ID")

  merged$FINAL <- merged$MULT / merged$REPORTING_VALUE

  final.df <- merge(unique(Long["EVENT_ID"]), merged,
                    by = "EVENT_ID",
                    all.x = TRUE)

  return(round(final.df$FINAL, digits = 2))

}



