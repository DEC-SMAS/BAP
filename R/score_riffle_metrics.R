#==============================================================================
#'Score Richness (Riffle)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw richness calculations with the ranges assigned to
#'riffle samples.
#'@export

score_rich_riffle <- function(metrics.df){
  m.value <- metrics.df$RICHNESS
  final.df <- ifelse(m.value > 35, 10,
                     ifelse(m.value > 26, ((((m.value - 26) / 9) * 2.5) + 7.5),
                            ifelse(m.value > 18, ((((m.value - 18) / 8.5) * 2.5) + 5),
                                   ifelse(m.value > 10, ((((m.value - 10) / 8.5) * 2.5) + 2.5),
                                          ifelse(m.value > 5, (((m.value - 4) / 6.5) * 2.5),
                                                 ifelse(m.value <= 5, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score EPT Richness (Riffle)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw EPT richness calculations with the ranges assigned to
#'riffle samples.
#'@export

score_ept_riffle <- function(metrics.df){
  m.value <- metrics.df$EPT_RICH
  final.df <- ifelse(m.value > 15, 10,
                     ifelse(m.value > 10, ((((m.value - 10) / 5) * 2.5) + 7.5),
                            ifelse(m.value > 5, ((((m.value - 5) / 5.5) * 2.5) + 5),
                                   ifelse(m.value > 1, ((((m.value - 1) / 4.5) * 2.5) + 2.5),
                                          ifelse(m.value == 1, 1.25,
                                                 ifelse(m.value == 0, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score HBI (Riffle)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw HBI calculations with the ranges assigned to
#'riffle samples.
#'@export

score_hbi_riffle <- function(metrics.df){
  m.value <- metrics.df$HBI
  final.df <- ifelse(m.value > 8.5, 2.5 - (((m.value - 8.5) / 1.5) * 2.5),
                     ifelse(m.value > 6.51, 5 - (((m.value - 6.5) / 2) * 2.5),
                            ifelse(m.value > 4.51, 7.5 - (((m.value - 4.5) / 2) * 2.5),
                                   ifelse(m.value >= 2, 10 - (m.value - 2),
                                          ifelse(m.value < 2, 10, 100000)))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score PMA (Riffle)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw PMA calculations with the ranges assigned to
#'riffle samples.
#'@export

score_pma_riffle <- function(metrics.df){
  m.value <- metrics.df$PMA
  final.df <- ifelse(m.value > 90, 10,
                     ifelse(m.value > 64, ((((m.value - 64) / 26) * 2.5) + 7.5),
                            ifelse(m.value > 49, ((((m.value - 49) / 15.5) * 2.5) + 5),
                                   ifelse(m.value > 34, ((((m.value - 34) / 15.5) * 2.5) + 2.5),
                                          ifelse(m.value > 20, (((m.value - 20) / 14.5) * 2.5),
                                                 ifelse(m.value <= 20, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score NBI-P (Riffle)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw NBI-P calculations with the ranges assigned to
#'riffle samples.
#'@export

score_nbip_riffle <- function(metrics.df){
  m.value <- metrics.df$NBI_P
  final.df <- ifelse(m.value > 8, 0,
                     ifelse(m.value > 7, 2.5 - (m.value - 7) * 2.5,
                            ifelse(m.value > 6, 5 - (m.value - 6) * 2.5,
                                   ifelse(m.value > 5, 7.5 - (m.value - 5) * 2.5,
                                          ifelse(m.value > 3, 10 - (m.value - 2.5),
                                                 ifelse(m.value <= 3, 10, 100000))))))
  return(round(final.df, digits = 2))
}

