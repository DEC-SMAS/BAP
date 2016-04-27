#==============================================================================
#'ISD Preparation
#'
#'@param Long = Taxonomic count data in a long data format.
#'@return A data frame of the percentage each taxon, necessary for ISD
#'calculations, in each sampling event.
#'@export

isd_prep <- function(Long){
  # function for calculating the percentage each taxon represents in the sample
  #pct_df <- function(x) (x / rowSums(x)) * 100

  phylum.df <- wide(Long, "PHYLUM")
  #phylum.df[,6:ncol(phylum.df)]  <- pct_df(phylum.df[,6:ncol(phylum.df)])

  class.df <- wide(Long, "CLASS")
  #class.df[,6:ncol(class.df)]  <- pct_df(class.df[,6:ncol(class.df)])

  order.df <- wide(Long, "ORDER")
  #order.df[,6:ncol(order.df)]  <- pct_df(order.df[,6:ncol(order.df)])

  family.df <- wide(Long, "FAMILY")
  #family.df[,6:ncol(family.df)]  <- pct_df(family.df[,6:ncol(family.df)])

  subfamily.df <- wide(Long, "SUBFAMILY")
  #subfamily.df[,6:ncol(subfamily.df)]  <- pct_df(subfamily.df[,6:ncol(subfamily.df)])

  genus.df <- wide(Long, "GENUS")
  #genus.df[,6:ncol(genus.df)]  <- pct_df(genus.df[,6:ncol(genus.df)])

  species.df <- wide(Long, "SPECIES")
  #species.df[,6:ncol(species.df)]  <- pct_df(species.df[,6:ncol(species.df)])


  new <- data.frame(unique(Long$EVENT_ID))
  new$PLATYHELMINTHES <- pct_taxon("PLATYHELMINTHES", "PHYLUM", Long)
  new$OLIGOCHAETA <- pct_taxon("OLIGOCHAETA", "CLASS", Long)
  new$HIRUDINEA<- pct_taxon("HIRUDINEA", "CLASS", Long)
  new$GASTROPDA <- pct_taxon("GASTROPODA", "CLASS", Long)
  new$SPHAERIIDAE <- pct_taxon("SPHAERIIDAE", "FAMILY", Long)
  new$ASELLIDAE <- pct_taxon("ASELLIDAE", "FAMILY", Long)
  new$GAMMARIDAE <- pct_taxon("GAMMARIDAE", "FAMILY", Long)
  new$ISONYCHIA <- pct_taxon("ISONYCHIA", "GENUS", Long)
  new$BAETIDAE <- pct_taxon("BAETIDAE", "FAMILY", Long)
  new$HEPTAGENIIDAE <- pct_taxon("HEPTAGENIIDAE", "FAMILY", Long)
  new$LEPTOPHLEBIIDAE <- pct_taxon("LEPTOPHLEBIIDAE", "FAMILY", Long)
  new$EPHEMERELLIDAE <- pct_taxon("EPHEMERELLIDAE", "FAMILY", Long)
  new$CAENIS_TRICORYTHODES <- pct_taxon("CAENIS", "GENUS", Long) +
    pct_taxon("TRICORYTHODES", "GENUS", Long)
  new$PLECOPTERA <- pct_taxon("PLECOPTERA", "ORDER", Long)
  new$PSEPHENUS <- pct_taxon("PSEPHENUS", "GENUS", Long)
  new$OPTIOSERVUS <- pct_taxon("OPTIOSERVUS", "GENUS", Long)
  new$PROMORESIA <- pct_taxon("PROMORESIA", "GENUS", Long)
  new$STENELMIS <- pct_taxon("STENELMIS", "GENUS", Long)
  new$PHILOPOTAMIDAE <- pct_taxon("PHILOPOTAMIDAE", "FAMILY", Long)
  new$HYDROPSYCHIDAE <- pct_taxon("HYDROPSYCHIDAE", "FAMILY", Long)
  new$HELICOPSYCHIDAE_BRACHYCENTRIDAE_EPHEMERELLIDAE <- pct_taxon("HELICOPSYCHIDAE", "FAMILY", Long) +
    pct_taxon("BRACHYCENTRIDAE", "FAMILY", Long) + pct_taxon("EPHEMERELLIDAE", "FAMILY", Long)
  new$RHYACOPHILDAE <- pct_taxon("RHYACOPHILIDAE", "FAMILY", Long)
  new$SIMULIIDAE<- pct_taxon("SIMULIIDAE", "FAMILY", Long) -
    pct_taxon("SIMULIUM_VITTATUM", "SPECIES", Long)
  new$SIMULIUM_VITTATUM <- pct_taxon("SIMULIUM_VITTATUM", "SPECIES", Long)
  new$EMPIDIDAE <- pct_taxon("EMPIDIDAE", "FAMILY", Long)
  new$TIPULIDAE <- pct_taxon("TIPULIDAE", "FAMILY", Long)
  new$TANYPODINAE <- pct_taxon("TANYPODINAE", "SUBFAMILY", Long)
  new$DIAMESINAE <- pct_taxon("DIAMESINAE", "SUBFAMILY", Long)
  new$CARDIOCLADIUS <- pct_taxon("CARDIOCLADIUS", "GENUS", Long)
  new$CRICOTOPUS_ORTHOCLADIUS  <- pct_taxon("CRICOTOPUS", "GENUS", Long) +
    pct_taxon("ORTHOCLADIUS", "GENUS", Long)
  new$EUKIEFFERIELLA_TVETENIA <- pct_taxon("EUKIEFFERIELLA", "GENUS", Long) +
    pct_taxon("TVETENIA", "GENUS", Long)
  new$PARAMETRIOCNEMUS <- pct_taxon("PARAMETRIOCNEMUS", "GENUS", Long)
  new$CHIRONOMUS <- pct_taxon("CHIRONOMUS", "GENUS", Long)
  new$MICROTENDIPES<- pct_taxon("MICROTENDIPES", "GENUS", Long)
  new$POLYPEDILUM_AVICEPS <- pct_taxon("POLYPEDILUM_AVICEPS", "SPECIES", Long)
  new$POLYPEDILUM <- pct_taxon("POLYPEDILUM", "GENUS", Long) -
    pct_taxon("POLYPEDILUM_AVICEPS", "SPECIES", Long)
  new$TANYTARSINI <- pct_taxon("TANYTARSINI", "GENUS", Long)

  names(new)[1] <- "EVENT_ID"
  melted <- reshape2::melt(new, id.vars = "EVENT_ID")
  names(melted) <- c("EVENT_ID", "TAXON", "PCT_OBS")


  comm.df <- melted
  return(melted)

}

#==============================================================================
#'ISD Models
#'
#'@param isd.df = a data frame with the ISD models.
#'@param comm.df = a data frame of observed community percentages.
#'@return The percentage of similarity the observed community has
#'to each model community.
#'@export

isd.models <- function(isd.df, comm.df){

  impoundment.df <- isd.func(isd.df, comm.df, isd.model = "IMPOUNDMENT")
  municipal.df <- isd.func(isd.df, comm.df, isd.model = "MUNICIPAL")
  natural.df <- isd.func(isd.df, comm.df, isd.model = "NATURAL")
  nonpoint.df <- isd.func(isd.df, comm.df, isd.model = "NONPOINT")
  sewage.df <- isd.func(isd.df, comm.df, isd.model = "SEWAGE")
  siltation.df <- isd.func(isd.df, comm.df, isd.model = "SILTATION")
  toxic.df <- isd.func(isd.df, comm.df, isd.model = "TOXIC")

  final.df <- cbind(impoundment.df[, c(1, length(impoundment.df))],
                    municipal.df[, length(municipal.df)],
                    natural.df[, length(natural.df)],
                    nonpoint.df[, length(nonpoint.df)],
                    sewage.df[, length(sewage.df)],
                    siltation.df[, length(siltation.df)],
                    toxic.df[, length(toxic.df)])
  names(final.df) <- c("EVENT_ID", "IMPOUNDMENT", "MUNICIPAL", "NATURAL",
                       "NONPOINT", "SEWAGE", "SILTATION", "TOXIC")
  return(final.df)
}

#==============================================================================
#'ISD Base Function
#'
#'@param isd.df = a data frame with the ISD models.
#'@param comm.df = a data frame of observed community percentages.
#'@param isd.model = the particular ISD model to compare with the
#'community percentages (e.g. "TOXIC").
#'@return The percentage of similarity the observed community has
#'to specific model community.
#'@export

isd.func <- function(isd.df, comm.df, isd.model){
  split.df <- split(isd.df, isd.df$ISD)
  if(isd.model == "IMPOUNDMENT"){
    isd.df2 <- split.df$IMPOUNDMENT
  }else{
    if(isd.model == "MUNICIPAL"){
      isd.df2 <- split.df$MUNICIPAL
    }else{
        if(isd.model == "NATURAL"){
          isd.df2 <- split.df$NATURAL
        }else{
          if(isd.model == "NONPOINT"){
            isd.df2 <- split.df$NONPOINT
          }else{
            if(isd.model == "SEWAGE"){
              isd.df2 <- split.df$SEWAGE
            }else{
              if(isd.model == "SILTATION"){
                isd.df2 <- split.df$SILTATION
              }else{
                if(isd.model == "TOXIC"){
                  isd.df2 <- split.df$TOXIC
                }
              }
            }
          }
        }
      }
  }

  isd.df3 <- data.frame(t(isd.df2[,3:ncol(isd.df2)]))
  names(isd.df3) <- isd.df2$GROUP
  isd.df3$TAXON <- row.names(isd.df3)
  merged<- merge(isd.df3, comm.df, by = "TAXON", all = T)

  merged$min_A <- pmin(merged$A, merged$PCT_OBS)
  merged$min_B <- pmin(merged$B, merged$PCT_OBS)
  merged$min_C <- pmin(merged$C, merged$PCT_OBS)
  merged$min_D <- pmin(merged$D, merged$PCT_OBS)
  merged$min_E <- pmin(merged$E, merged$PCT_OBS)
  merged$min_F <- if(!is.null(merged$F)) pmin(merged$F, merged$PCT_OBS)
  merged$min_G <- if(!is.null(merged$G))pmin(merged$G, merged$PCT_OBS)
  merged$min_H <- if(!is.null(merged$H))pmin(merged$H, merged$PCT_OBS)
  merged$min_I <- if(!is.null(merged$I))pmin(merged$I, merged$PCT_OBS)
  merged$min_J <- if(!is.null(merged$J))pmin(merged$J, merged$PCT_OBS)


  m.a <- aggregate(min_A ~ EVENT_ID, data = merged, FUN = sum)
  m.b <- aggregate(min_B ~ EVENT_ID, data = merged, FUN = sum)
  m.c <- aggregate(min_C ~ EVENT_ID, data = merged, FUN = sum)
  m.d <- aggregate(min_D ~ EVENT_ID, data = merged, FUN = sum)
  m.e <- aggregate(min_E ~ EVENT_ID, data = merged, FUN = sum)
  m.f <- if(!is.null(merged$F)) aggregate(min_F ~ EVENT_ID, data = merged, FUN = sum)
  m.g <- if(!is.null(merged$G)) aggregate(min_G ~ EVENT_ID, data = merged, FUN = sum)
  m.h <- if(!is.null(merged$H)) aggregate(min_H ~ EVENT_ID, data = merged, FUN = sum)
  m.i <- if(!is.null(merged$I)) aggregate(min_I ~ EVENT_ID, data = merged, FUN = sum)
  m.j <- if(!is.null(merged$J)) aggregate(min_J ~ EVENT_ID, data = merged, FUN = sum)

  final.df <- cbind(m.a, m.b[, 2], m.c[, 2], m.d[, 2], m.e[, 2],
                    if(!is.null(m.f)){m.f[, 2]}else{0},
                    if(!is.null(m.g)){m.g[, 2]}else{0},
                    if(!is.null(m.h)){m.h[, 2]}else{0},
                    if(!is.null(m.i)){m.i[, 2]}else{0},
                    if(!is.null(m.j)){m.j[, 2]}else{0})

  names(final.df) <- c("EVENT_ID", "A", "B", "C", "D", "E",
                       "F", "G", "H", "I", "J")

  #final.df[, isd.model] <- ifelse(apply(final.df[, 2:ncol(final.df)], 1, FUN = max) >= 50,  1, 0)
  final.df[, isd.model] <- apply(final.df[, 2:ncol(final.df)], 1, FUN = max)
  return(final.df)
}

#==============================================================================
#'Calculate All of the ISD Models
#'
#'@param Long = Taxonomic count data in a long data format.
#'@param isd.df = a data frame with the ISD models.
#'@return The percentage of similarity the observed community has
#'to each of the model communities.
#'@export

calc.isd <- function(Long, isd.df){
  comm.df <- isd_prep(Long)
  final.df <- isd.models(isd.df, comm.df)
  final.df[, 2:ncol(final.df)] <- round(final.df[, 2:ncol(final.df)], digits = 2)
  return(final.df)
}



