#==============================================================================
#'Score Richness (Ponar)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw richness calculations with the ranges assigned to
#'ponar samples.
#'@export

score_rich_ponar <- function(metrics.df){
  m.value <- metrics.df$RICHNESS
  final.df <- ifelse(m.value > 25, 10,
                     ifelse(m.value > 19, ((((m.value - 19) / 6.5) * 2.5) + 7.5),
                            ifelse(m.value > 14, ((((m.value - 14) / 5.5) * 2.5) + 5),
                                   ifelse(m.value > 10, ((((m.value - 10) / 4.5) * 2.5) + 2.5),
                                          ifelse(m.value >= 4, (((m.value - 5) / 5.5) * 2.5),
                                                 ifelse(m.value < 4, 0, 100000))))))
  final.df <- ifelse(final.df < 0, 0, as.numeric(final.df))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score HBI (Ponar)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw HBI calculations with the ranges assigned to
#'ponar samples.
#'@export

score_hbi_ponar <- function(metrics.df){
  m.value <- metrics.df$HBI
  final.df <- ifelse(m.value >= 9, 2.5 - ((m.value - 9) * 2.5),
                     ifelse(m.value >= 8, 5 - ((m.value - 8) * 2.5),
                            ifelse(m.value >= 7, 7.5 - ((m.value - 7) * 2.5),
                                   ifelse(m.value >= 6, 10 - ((m.value - 6) * 2.5),
                                          ifelse(m.value < 6, 10, 100000)))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score PMA (Ponar)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw PMA calculations with the ranges assigned to
#'ponar samples.
#'@export

score_pma_ponar <- function(metrics.df){
  m.value <- metrics.df$PMA
  final.df <- ifelse(m.value > 80, 10,
                     ifelse(m.value > 67.5, ((m.value - 67.5) / 5) + 7.5,
                            ifelse(m.value > 55, ((m.value - 55) / 5) + 5,
                                   ifelse(m.value > 42.5, ((m.value - 42.5) / 5) + 2.5,
                                          ifelse(m.value > 30, (m.value - 30) / 5,
                                                 ifelse(m.value <= 30, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score Percent DOM-3 (Ponar)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw percent DOM-3 calculations with the ranges assigned to
#'ponar samples.
#'@export

score_pct_dom3_ponar <- function(metrics.df){
  m.value <- metrics.df$PCT_DOM3
  final.df <- ifelse(m.value == 100, 0,
                     ifelse(m.value >= 90, 2.5 - (((m.value - 90) / 10) *2.5),
                            ifelse(m.value >= 75,  5 - (((m.value - 75) / 15) *2.5),
                                   ifelse(m.value >= 60,  7.5 - (((m.value - 60) / 15) *2.5),
                                          ifelse(m.value > 45, 10 - (((m.value - 45) / 15) *2.5),
                                                 ifelse(m.value <= 45, 10, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score Shannon-Wiener Diversity (Ponar)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw Shannon-Wiener Diversity calculations
#' with the ranges assigned to ponar samples.
#'@export

score_shannon_ponar<- function(metrics.df){
  m.value <- metrics.df$SHANNON
  final.df <- ifelse(m.value > 4, 10,
                     ifelse(m.value > 3, (((m.value - 3) / 0.5) * 2.5) + 7.5,
                            ifelse(m.value > 2.5, (((m.value - 2.5) / 0.5) * 2.5) + 5,
                                   ifelse(m.value > 2, (((m.value - 2) / 0.5) * 2.5) + 2.5,
                                          ifelse(m.value > 1.5, ((m.value - 1.5) / 0.5) * 2.5,
                                                 ifelse(m.value <= 1.5, 0, 100000))))))
  return(round(final.df, digits = 2))
}
