#==============================================================================
#'Score Richness (Multiple-Plate Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw richness calculations with the ranges assigned to
#'multiple-plate samples collected in navigable waters.
#'@export

score_rich_mp_nav_water <- function(metrics.df){
  m.value <- metrics.df$RICHNESS
  final.df <- ifelse(m.value > 26, 10,
                     ifelse(m.value > 21, ((((m.value - 21) / 5) * 2.5) + 7.5),
                            ifelse(m.value > 16, ((((m.value - 16) / 5.5) * 2.5) + 5),
                                   ifelse(m.value > 11, ((((m.value - 11) / 5.5) * 2.5) + 2.5),
                                          ifelse(m.value > 8, (((m.value - 8) / 3.5) * 2.5),
                                                 ifelse(m.value <= 8, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score EPT Richness (Multiple-Plate Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw EPT richness calculations with the ranges assigned to
#'multiple-plate samples collected in navigable waters.
#'@export

score_ept_mp_nav_water <- function(metrics.df){
  m.value <- metrics.df$EPT_RICH
  final.df <- ifelse(m.value > 10, 10,
                     ifelse(m.value > 5, ((((m.value - 5) / 5) * 2.5) + 7.5),
                            ifelse(m.value >3, (m.value - 3) + 5,
                                   ifelse(m.value > 1, (m.value - 1) + 2.5,
                                          ifelse(m.value == 1, 1.5,
                                                 ifelse(m.value == 0, 0, 100000))))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score HBI (Multiple-Plate Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw HBI calculations with the ranges assigned to
#'multiple-plate samples collected in navigable waters.
#'@export

score_hbi_mp_nav_water <- function(metrics.df){
  m.value <- metrics.df$HBI
  final.df <- ifelse(m.value > 9, 2.5 - ((m.value - 9)  * 2.5),
                     ifelse(m.value > 8, 5 - ((m.value - 8) * 2.5),
                            ifelse(m.value > 7, 7.5 - ((m.value - 7) * 2.5),
                                   ifelse(m.value >= 6, 10 - ((m.value - 6) * 2.5),
                                          ifelse(m.value < 6, 10, 100000)))))
  return(round(final.df, digits = 2))
}

#==============================================================================
#'Score Shannon-Wiener Diversity (Multiple-Plate Navigable Waters)
#'
#'@param metrics.df = a data frame of calculated metrics values for each
#'sampling event.
#'@return Scores the raw Shannon-Wiener Diversity calculations with the ranges assigned to
#'multiple-plate samples collected in navigable waters.
#'@export

score_shannon_mp_nav_water <- function(metrics.df){
  m.value <- metrics.df$SHANNON
  final.df <- ifelse(m.value > 3.5, 10,
                     ifelse(m.value > 3, ((((m.value - 3) / 0.5) * 2.5) + 7.5),
                            ifelse(m.value > 2.5, ((((m.value - 2.5) / 0.5) * 2.5) + 5),
                                   ifelse(m.value > 2, ((((m.value - 2) / 0.5) * 2.5) + 2.5),
                                          ifelse(m.value > 1.5, (((m.value - 1.5) / 0.5) * 2.5),
                                                 ifelse(m.value <= 1.5, 0, 100000))))))
  return(round(final.df, digits = 2))
}


