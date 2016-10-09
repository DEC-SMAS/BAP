# Import PMA Table
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
#pma.model <- read.csv("PMA_MODEL.csv")
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
#devtools::use_data(pma.model, overwrite = TRUE)

# Import PMA Table
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
#pma.ponar <- read.csv("PMA_PONAR.csv")
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
#devtools::use_data(pma.ponar, overwrite = TRUE)

# Import ISD Table
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
#isd.df <- read.csv("ISD.csv")
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
#devtools::use_data(isd.df, overwrite = TRUE)

# Requires Packages
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
#devtools::use_package("reshape2", "imports")
#devtools::use_package( "vegan", "imports")
#devtools::use_package("zoo", "imports")

# Import Master Taxa List
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
#master <- read.csv("BPA_MASTER.csv")
#master <- read.csv("specieslist2016.csv")
#(master)[names(master) %in% "GENSPECIES"] <- "GENUS_SPECIES"
#names(master)[names(master) %in% "CLAS"] <- "CLASS"
#names(master)[names(master) %in% "ORDR"] <- "ORDER"
#master <- BAP::prep_master_taxa(master)
#setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
#devtools::use_data(master, overwrite = TRUE)
