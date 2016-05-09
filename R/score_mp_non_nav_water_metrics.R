#==============================================================================
#'Score Richness (Multiple-Plate Non-Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw richness calculations with the ranges assigned to
#'multiple-plate samples collected in non-navigable waters.
#'@export

score_rich_mp_non_nav_water <- function(metrics.df){
  m.value <- metrics.df$RICHNESS
  final.df <- ifelse(m.value > 35, 10,
                     ifelse(m.value > 26, ((((m.value - 26) / 9) * 2.5) + 7.5),
                            ifelse(m.value > 18, ((((m.value - 18) / 8.5) * 2.5) + 5),
                                   ifelse(m.value > 10, ((((m.value - 10) / 8.5) * 2.5) + 2.5),
                                          ifelse(m.value >= 5, (((m.value - 5) / 5.5) * 2.5),
                                                 ifelse(m.value < 5, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score EPT Richness (Multiple-Plate Non-Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw EPT richness calculations with the ranges assigned to
#'multiple-plate samples collected in non-navigable waters.
#'@export

score_ept_mp_non_nav_water <- function(metrics.df){
  m.value <- metrics.df$EPT_RICH
  final.df <- ifelse(m.value > 15, 10,
                     ifelse(m.value > 10, ((((m.value - 10) / 5) * 2.5) + 7.5),
                            ifelse(m.value > 5, (((m.value - 5) / 5.5) * 2.5) + 5,
                                   ifelse(m.value > 1, (((m.value - 1)/ 4.5) * 2.5) + 2.5,
                                          ifelse(m.value == 1, 1.25,
                                                 ifelse(m.value == 0, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score HBI (Multiple-Plate Non-Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw HBI calculations with the ranges assigned to
#'multiple-plate samples collected in non-navigable waters.
#'@export

score_hbi_mp_non_nav_water <- function(metrics.df){
  m.value <- metrics.df$HBI
  final.df <- ifelse(m.value > 8.5, 2.5 - (((m.value - 8.5) / 1.5)  * 2.5),
                     ifelse(m.value > 6.51, 5 - (((m.value - 6.5) / 2) * 2.5),
                            ifelse(m.value > 4.51, 7.5 - (((m.value - 4.5) / 2) * 2.5),
                                   ifelse(m.value > 2, 10 - (m.value - 2),
                                          ifelse(m.value < 2, 10, 100000)))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score Shannon-Wiener Diversity (Multiple-Plate Non-Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw Shannon-Wiener Diversity calculations with the ranges assigned to
#'multiple-plate samples collected in non-navigable waters.
#'@export

score_shannon_mp_non_nav_water <- function(metrics.df){
  m.value <- metrics.df$SHANNON
  final.df <- ifelse(m.value > 5, 10,
                     ifelse(m.value > 4, (((m.value - 4) * 2.5) + 7.5),
                            ifelse(m.value > 3, (((m.value - 3) * 2.5) + 5),
                                   ifelse(m.value > 2, ((m.value - 2) * 2.5) + 2.5,
                                          ifelse(m.value > 1, ((m.value - 1) * 2.5),
                                                 ifelse(m.value <= 1, 0, 100000))))))
  return(round(final.df, digits = 2))
}


