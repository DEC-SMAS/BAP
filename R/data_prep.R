#==============================================================================
#'Prepare the Taxonomic Count Data
#'
#'@param taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

taxa_prep2 <- function(taxa.df){
  taxa.df$SAMPLE_NUMBER[is.na(taxa.df$SAMPLE_NUMBER)] <- 1
  #taxa.df$RIVMILE <- as.numeric(taxa.df$RIVMILE)
  taxa.df$EVENT_ID <- with(taxa.df, paste0(RIVMILE, LOCATION, BASIN,
                                           DATE, SAMPLE_NUMBER, sep = "_"))
  #taxa.df$FINAL_ID
  final.df <- taxa.df[, c(length(taxa.df), 1:length(taxa.df) - 1)]
  taxa.levels <- c("PHYLUM", "CLASS", "ORDER", "FAMILY", "SUBFAMILY",
                   "GENUS_SPECIES", "FINAL_ID")
  final.df[, taxa.levels] <- apply(final.df[, taxa.levels], 2, as.character)
  final.df[, taxa.levels] <- apply(final.df[, taxa.levels], 2, function(x){
    x <- sub("^$", "UNDETERMINED", x)
  })

}

#==============================================================================
#'Create an Event_ID
#'
#'@param Check.df =
#'@return Create a unique event identification (EVENT_ID).
#'@export

event_prep <- function(data.df){
  names(data.df) <- toupper(names(data.df))
  if("REPLICATE" %in% names(data.df)){
    names(data.df)[names(data.df) %in% "REPLICATE"] <- "SAMPLE_NUMBER"
  }
  #============================================================================
  if("INDIV" %in% names(data.df)){
    names(data.df)[names(data.df) %in% "INDIV"] <- "REPORTING_VALUE"
  }
  #============================================================================
  if("MACRO_GENSPECIES" %in% names(data.df)){
    names(data.df)[names(data.df) %in% "MACRO_GENSPECIES"] <- "FINAL_ID"
  }
  #============================================================================
  if("COLL_DATE" %in% names(data.df)){
    names(data.df)[names(data.df) %in% "COLL_DATE"] <- "DATE"
  }
  #============================================================================

  data.df$SAMPLE_NUMBER <- ifelse(is.na(data.df$SAMPLE_NUMBER), 1,
                                   as.numeric(data.df$SAMPLE_NUMBER))
  data.df$RIVMILE <- as.character(data.df$RIVMILE)
  event.cols <- c("RIVMILE", "LOCATION", "BASIN", "DATE", "SAMPLE_NUMBER")
  data.df$EVENT_ID <- apply(data.df[, event.cols], 1, function(x){
    paste0(x, collapse = "_")
  })

  final.df <- data.df[, c(length(data.df), 1:length(data.df) - 1)]
  final.df <- final.df[!final.df$EVENT_ID %in% "NA__NA__1", ]
  return(final.df)
}

#==============================================================================
#'Vector of taxa richness for a specific list of taxa
#'
#'@param NameList = uninque list of taxa.
#'@param taxa.df = Wide data frame format of taxonomic counts.
#'@return A vector of taxa richness for a specific list of taxa representing
#'each sampling event. NameList from the wide data frame of taxa
#' (i.e. Family level or Genus level)
#'@export

group_rich <- function(NameList, taxa.df){
  taxa.list <- unlist(NameList)
  #taxa.list[which(c(1, diff(taxa.list)) != 0)]
  idx <- match(taxa.list, names(taxa.df))
  idx <- idx[! is.na(idx)]
  taxa_list.df <- data.frame(taxa.df[, idx])
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
  agg <- aggregate(REPORTING_VALUE ~ EVENT_ID + LOCATION + RIVMILE +
                     BASIN + DATE + Long[, colnames(Long) == Level],
                   data = Long, FUN = sum, na.rm = TRUE)
  colnames(agg) <- c("EVENT_ID", "LOCATION", "RIVMILE", "BASIN",
                     "DATE", Level, "REPORTING_VALUE")

  wide.df <- reshape2::dcast(agg, EVENT_ID + LOCATION + RIVMILE +
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

#==============================================================================
#==============================================================================
#'Prepare the Taxonomic Count Data
#'
#'@param taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

taxa_prep <- function(taxa.df){

  taxa.df$FINAL_ID <- toupper(gsub(" ","_", taxa.df$FINAL_ID))

  return(taxa.df)
}
#==============================================================================
#'Prepare the Taxonomic Count Data
#'
#'@param taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

taxa_prep3 <- function(taxa.df){
  taxa.levels <- c("PHYLUM", "CLASS", "ORDER", "FAMILY", "SUBFAMILY",
                   "GENUS_SPECIES", "SAMPLE_GENUS_SPECIES")
  taxa.df[, taxa.levels] <- apply(taxa.df[, taxa.levels], 2, as.character)
  taxa.df[, taxa.levels] <- apply(taxa.df[, taxa.levels], 2, function(x){
    x <- sub("^$", "UNDETERMINED", x)
  })
  taxa.df$GENUS <- taxa.df$GENUS_SPECIES
  # Remove any UNDETERMINED
  taxa.df$GENUS <- gsub("UNDETERMINED ","", taxa.df$GENUS)
  # Remove any text contained within parentheses
  taxa.df$GENUS <- gsub("\\([^\\)]+\\)","", taxa.df$GENUS)
  # Remove NR.
  taxa.df$GENUS <- gsub("\ NR\\.","", taxa.df$GENUS)
  # Remove GR.
  taxa.df$GENUS <- gsub("\ GR\\.","", taxa.df$GENUS)
  # Remove ?
  taxa.df$GENUS <- gsub("\\?","", taxa.df$GENUS)
  taxa.df$GENUS <- ifelse(grepl(" ",  taxa.df$GENUS),
                           gsub( " .*$", "", taxa.df$GENUS_SPECIES),
                           taxa.df$GENUS)

  # Remove all genera, groups, undetermineds, complexes, and uncertainties
  taxa.df$SPECIES <- sapply(taxa.df$GENUS_SPECIES, function(x){
    remove <- c("SP\\.", "SPP\\.", "CF\\.", "UNDET\\.", "UNDETERMINED", "/")
    ifelse(grepl(paste(remove, collapse="|"), x), "", paste(x))
  })
  # Remove any text contained within parentheses
  taxa.df$SPECIES <- gsub("\\([^\\)]+\\)","", taxa.df$SPECIES)
  # Remove NR.
  taxa.df$SPECIES <- gsub("\ NR\\.","", taxa.df$SPECIES)
  # Remove GR.
  taxa.df$SPECIES <- gsub("\ GR\\.","", taxa.df$SPECIES)
  # Remove ?
  taxa.df$SPECIES <- gsub("\\?","", taxa.df$SPECIES)
  # Replace the space between genus and species with "_"
  taxa.df$SPECIES <- gsub(" ","_", taxa.df$SPECIES)
  # Replace the blanks with "UNDETERMINED"
  taxa.df$SPECIES <- sub("^$", "UNDETERMINED", taxa.df$SPECIES)

  taxa <- c("PHYLUM", "CLASS", "ORDER",
            "FAMILY", "SUBFAMILY",
            "GENUS", "SPECIES")

  taxa.df[taxa.df == "" | taxa.df == "UNDETERMINED"]  <- NA
  taxa.df[, taxa] <- data.frame(t(apply(taxa.df[, taxa], 1, zoo::na.locf)))
  taxa.df$FINAL_ID <- taxa.df$SPECIES
  return(taxa.df)
}
#==============================================================================
#'Prepare the Data
#'
#'@param taxa.df = A data frame containing raw taxonomic counts.
#'@return Prepare taxonomic data for metric calculations.
#'@export

data_prep <- function(taxa.df){
  prep.df <- event_prep(taxa.df)
  taxa.df <- taxa_prep(prep.df)
  data("master")
  final.df <- merge(taxa.df, master, by = "FINAL_ID", all.x = T)
  final.df <- final.df[order(final.df$EVENT_ID), ]
  return(final.df)
}



