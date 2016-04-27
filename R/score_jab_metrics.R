#==============================================================================
#'Score Richness (Jab)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw richness calculations with the ranges assigned to
#'jab samples.
#'@export

score_rich_jab <- function(metrics.df){
  m.value <- metrics.df$RICHNESS
  final.df <- ifelse(m.value > 26, 10,
                     ifelse(m.value > 21, ((((m.value - 21) / 5) * 2.5) + 7.5),
                            ifelse(m.value > 16, ((((m.value - 16) / 5.5) * 2.5) + 5),
                                   ifelse(m.value > 11, ((((m.value - 11) / 5.5) * 2.5) + 2.5),
                                          ifelse(m.value >= 8, (((m.value - 8) / 3.5) * 2.5),
                                                 ifelse(m.value < 8, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score EPT Richness (Jab)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw EPT richness calculations with the ranges assigned to
#'jab samples.
#'@export

score_ept_jab <- function(metrics.df){
  m.value <- metrics.df$EPT_RICH
  final.df <- ifelse(m.value > 10, 10,
                     ifelse(m.value > 5, (((m.value - 5) / 5) * 2.5) + 7.5,
                            ifelse(m.value > 3, (m.value - 3) + 5,
                                   ifelse(m.value > 2, (m.value - 1) + 2.5,
                                          ifelse(m.value > 0, 1.5,
                                                 ifelse(m.value == 0, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score HBI (Jab)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw HBI calculations with the ranges assigned to
#'jab samples.
#'@export

score_hbi_jab <- function(metrics.df){
  m.value <- metrics.df$HBI
  final.df <- ifelse(m.value > 8.5, 2.5 - (((m.value - 8.5) / 1.5) * 2.5),
                     ifelse(m.value > 7, 5 - (((m.value - 7) / 1.5) * 2.5),
                            ifelse(m.value > 5.5, 7.5 - (((m.value - 5.5) / 1.5) * 2.5),
                                   ifelse(m.value >= 4, 10 - ((m.value - 4) / 1.5) *2.5,
                                          ifelse(m.value < 4, 10, 100000)))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score NCO Richness (Jab)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw NCO Richness calculations with the ranges assigned to
#'jab samples.
#'@export

score_nco_jab <- function(metrics.df){
  m.value <- metrics.df$NCO_RICH
  final.df <- ifelse(m.value > 15, 10,
                     ifelse(m.value > 10, ((((m.value - 10) / 5) * 2.5) + 7.5),
                            ifelse(m.value > 5, ((((m.value - 5) / 5.5) * 2.5) + 5),
                                   ifelse(m.value > 1, ((((m.value - 1) / 4.5) * 2.5) + 2.5),
                                          ifelse(m.value > 0, 1.25,
                                                 ifelse(m.value == 0, 0, 100000))))))
  return(round(final.df, digits = 2))
}



