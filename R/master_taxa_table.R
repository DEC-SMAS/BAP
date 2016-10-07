#==============================================================================
#'Prepare Master Taxa List
#'
#'@param taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

prep_master_taxa <- function(master){
  master <- data.frame(apply(master, 2, toupper))
  master$GENUS <- master$GENUS_SPECIES
  # Remove any UNDETERMINED
  master$GENUS <- gsub("UNDETERMINED ","", master$GENUS)
  # Remove any text contained within parentheses
  master$GENUS <- gsub("\\([^\\)]+\\)","", master$GENUS)
  # Remove NR.
  master$GENUS <- gsub("\ NR\\.","", master$GENUS)
  # Remove GR.
  master$GENUS <- gsub("\ GR\\.","", master$GENUS)
  # Remove ?
  master$GENUS <- gsub("\\?","", master$GENUS)
  master$GENUS <- ifelse(grepl(" ",  master$GENUS),
                         gsub( " .*$", "", master$GENUS_SPECIES),
                         master$GENUS)
  
  # Remove all genera, groups, undetermineds, complexes, and uncertainties
  master$SPECIES <- sapply(master$GENUS_SPECIES, function(x){
    remove <- c("SP\\.", "SPP\\.", "CF\\.", "UNDET\\.", NA, "/")
    ifelse(grepl(paste(remove, collapse="|"), x), "", paste(x))
  })
  # Remove any text contained within parentheses
  master$SPECIES <- gsub("\\([^\\)]+\\)","", master$SPECIES)
  # Remove NR.
  master$SPECIES <- gsub("\ NR\\.","", master$SPECIES)
  # Remove GR.
  master$SPECIES <- gsub("\ GR\\.","", master$SPECIES)
  # Remove ?
  master$SPECIES <- gsub("\\?","", master$SPECIES)
  # Replace the space between genus and species with "_"
  master$SPECIES <- gsub(" ","_", master$SPECIES)
  # Replace the blanks with NA
  master$SPECIES <- sub("^$", NA, master$SPECIES)
  
  taxa <- c("PHYLUM", "CLASS", "ORDER", 
            "FAMILY", "SUBFAMILY",
            "GENUS", "SPECIES")
  
  master[master == ""] <- NA
  master[, taxa] <- data.frame(t(apply(master[, taxa], 1, zoo::na.locf)))
  master$FINAL_ID <- toupper(gsub(" ","_", master$GENUS_SPECIES))
  
  final.df <- master[, c("FINAL_ID", "PHYLUM", "CLASS", "ORDER", "FAMILY",
                         "SUBFAMILY", "GENUS", "SPECIES", "GENUS_SPECIES",
                         "FEEDINGHAB", "TOLERANCE", "NBI.P_TOLERANCE",
                         "NBI.N_TOLERANCE", "TOLNAME", "CLASSIFIER", 
                         "PREVNAMES", "COMMONNAME", "REFERENCE")]
  names(final.df)[names(final.df) %in% "NBI.P_TOLERANCE"] <- "NBI_P_TOLERANCE"
  names(final.df)[names(final.df) %in% "NBI.N_TOLERANCE"] <- "NBI_N_TOLERANCE"
  return(final.df)
}


