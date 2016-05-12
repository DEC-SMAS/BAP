setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
pma <- read.csv("PMA_MODEL.csv")
setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
devtools::use_data(pma, overwrite = TRUE)


setwd("C:\\Users\\Owner\\Desktop\\NYSDEC")
isd <- read.csv("ISD.csv")
setwd("C:\\Users\\Owner\\Desktop\\NYSDEC.BAP\\nysdec.bap")
devtools::use_data(isd, overwrite = TRUE)



