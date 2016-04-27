#==============================================================================
#'Prepare the Data
#'
#'@param Taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

data_prep <- function(Taxa.df){
  Taxa.df$SAMPLE_NUMBER[is.na(Taxa.df$SAMPLE_NUMBER)] <- 1
  Taxa.df$STATION_ID <- as.numeric(Taxa.df$STATION_ID)
  Taxa.df$EVENT_ID <- with(Taxa.df, paste0(STATION_ID, LOCATION, BASIN, DATE))
  final.df <- Taxa.df[, c(length(Taxa.df), 1:length(Taxa.df) - 1)]
  taxa.levels <- c("PHYLUM", "CLASS", "ORDER", "FAMILY", "SUBFAMILY",
                   "GENUS_SPECIES", "SAMPLE_GENUS_SPECIES")
  final.df[, taxa.levels] <- apply(final.df[, taxa.levels], 2, as.character)
  final.df[, taxa.levels] <- apply(final.df[, taxa.levels], 2, function(x){
    x <- sub("^$", "UNDETERMINED", x)
  })
  final.df$GENUS <- final.df$GENUS_SPECIES
  # Remove any UNDETERMINED
  final.df$GENUS <- gsub("UNDETERMINED ","", final.df$GENUS)
  # Remove any text contained within parentheses
  final.df$GENUS <- gsub("\\([^\\)]+\\)","", final.df$GENUS)
  # Remove NR.
  final.df$GENUS <- gsub("\ NR\\.","", final.df$GENUS)
  # Remove GR.
  final.df$GENUS <- gsub("\ GR\\.","", final.df$GENUS)
  # Remove ?
  final.df$GENUS <- gsub("\\?","", final.df$GENUS)
  final.df$GENUS <- ifelse(grepl(" ",  final.df$GENUS),
                           gsub( " .*$", "", final.df$GENUS_SPECIES),
                           final.df$GENUS)

  # Remove all genera, groups, undetermineds, complexes, and uncertainties
  final.df$SPECIES <- sapply(final.df$GENUS_SPECIES, function(x){
    remove <- c("SP\\.", "SPP\\.", "CF\\.", "UNDET\\.", "UNDETERMINED", "/")
    ifelse(grepl(paste(remove, collapse="|"), x), "", paste(x))
  })
  # Remove any text contained within parentheses
  final.df$SPECIES <- gsub("\\([^\\)]+\\)","", final.df$SPECIES)
  # Remove NR.
  final.df$SPECIES <- gsub("\ NR\\.","", final.df$SPECIES)
  # Remove GR.
  final.df$SPECIES <- gsub("\ GR\\.","", final.df$SPECIES)
  # Remove ?
  final.df$SPECIES <- gsub("\\?","", final.df$SPECIES)
  # Replace the space between genus and species with "_"
  final.df$SPECIES <- gsub(" ","_", final.df$SPECIES)
  # Replace the blanks with "UNDETERMINED"
  final.df$SPECIES <- sub("^$", "UNDETERMINED", final.df$SPECIES)
  return(final.df)
}

#==============================================================================
#'Create an Event_ID
#'
#'@param Check.df =
#'@return Create a unique event identification (EVENT_ID).
#'@export

event_prep <- function(Check.df){
  Check.df$SAMPLE_NUMBER[is.na(Check.df$SAMPLE_NUMBER)] <- 1
  Check.df$STATION_ID <- as.numeric(Check.df$STATION_ID)
  Check.df$EVENT_ID <- with(Check.df, paste0(STATION_ID, LOCATION, BASIN, DATE))
  final.df <- Check.df[, c(length(Check.df), 1:length(Check.df) - 1)]
  return(final.df)
}

#==============================================================================
#'Vector of taxa richness for a specific list of taxa
#'
#'@param NameList = uninque list of taxa.
#'@param Taxa.df = Wide data frame format of taxonomic counts.
#'@return A vector of taxa richness for a specific list of taxa representing
#'each sampling event. NameList from the wide data frame of taxa
#' (i.e. Family level or Genus level)
#'@export

group_rich <- function(NameList, Taxa.df){
  taxa.list <- unlist(NameList)
  #taxa.list[which(c(1, diff(taxa.list)) != 0)]
  idx <- match(taxa.list, names(Taxa.df))
  idx <- idx[! is.na(idx)]
  taxa_list.df <- data.frame(Taxa.df[, idx])
  taxa_list.df[is.na(taxa_list.df)] <- 0 #NA = zero
  final_taxa.df <- vegan::specnumber(taxa_list.df)
  return(final_taxa.df)
}

#==============================================================================
#'Wide Data Frame
#'Transform taxa count data from long to wide format
#'@param Long = Taxonomic count data in a long data format.
#'@param Level = Taxonomic Level (PHYLUM, CLASS, ORDER, FAMILY, GENUS).
#'@return Taxa counts in wide data format.
#'@export
#'
wide <- function (Long, Level) {
  agg <- aggregate(REPORTING_VALUE ~ EVENT_ID + LOCATION + STATION_ID +
                     BASIN + DATE + Long[, colnames(Long) == Level],
                   data = Long, FUN = sum, na.rm = TRUE)
  colnames(agg) <- c("EVENT_ID", "LOCATION", "STATION_ID", "BASIN",
                     "DATE", Level, "REPORTING_VALUE")

  wide.df <- reshape2::dcast(agg, EVENT_ID + LOCATION + STATION_ID +
                               BASIN + DATE ~ agg[, Level],
                             value.var = "REPORTING_VALUE")

  #number of taxa not identified to the specified taxonomic level
  #names(wide.df)[names(wide.df) == 'Var.6'] <- "UNIDENTIFIED"
  #Fill all NA's with zeros
  wide.df[is.na(wide.df)] <- 0 #NA = zero
  #Exclude any rows without taxonomic counts
  wide.df <- wide.df[rowSums(wide.df[, 6:ncol(wide.df)]) != 0, ]
  wide.df <- with(wide.df,  wide.df[order(EVENT_ID), ])
  names(wide.df) <- toupper(colnames(wide.df))
  return(wide.df)
}




