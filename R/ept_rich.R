#============================================================================
#'EPT Richness Excluding Tolerant Taxa
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param Level = Must sepecify either 'FAMILY' or "GENUS.'
#'@return The number of Ephemeroptera, Plecoptera, and Trichoptera (EPT)
#' taxa excluding tolerant taxa.
#'@export
#'
ept_rich <- function(Long, Level) {
  wide.df <- wide(Long, Level)
  Order <- split(Long[, Level], Long$ORDER)
  ephem <- unique(Order$EPHEMEROPTERA)
  plecop <- unique(Order$PLECOPTERA)
  trichop <- unique(Order$TRICHOPTERA)
  ept.list <- list(ephem, plecop, trichop)
  taxa.list <- unlist(ept.list)
  #taxa.list[which(c(1, diff(taxa.list)) != 0)]
  idx <- match(taxa.list, names(wide.df))
  idx <- idx[! is.na(idx)]
  data <- data.frame(wide.df[, idx])
  data$names <- rownames(data)
  fam.copy <- data.frame(wide.df)
  fam.copy$names <- rownames(fam.copy)
  fam.copy <- fam.copy[, c("EVENT_ID", "names")]
  taxa.x <- merge(fam.copy, data, by.x = "names", by.y = "names",
                  all.x = TRUE)
  taxa.x[is.na(taxa.x)] <- 0 #NA = zero
  taxa.x <- data.frame(with(taxa.x, taxa.x[order(EVENT_ID), ]))
  final.df <- vegan::specnumber(taxa.x[, 3:ncol(taxa.x)])
  return(final.df)
}
