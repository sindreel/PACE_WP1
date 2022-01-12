#Create metadata table for samples
library(dplyr)
library(plyr)

NALO_2021 <- read.csv("./data/raw_data/NALO_2021.csv", sep = ";")
str(NALO_2021)
colnames(NALO_2021)
NALO_2021 <- NALO_2021[c(1:15)]
names(NALO_2021) <- c("ID","Dato","PO","Stasjon","Breddegrad","Lengdegrad","Art","Redskap","Maskestr","Skjellprove","Vekt","Lengde_gaf","Lengde_tot","Gjelleprove","Lagret_sep_2021")




NALO_2021_keka <- read.csv("./data/raw_data/NALO_2021_keka.csv", sep = ";")
str(NALO_2021_keka)
colnames(NALO_2021_keka)[1] <- "ID"
colnames(NALO_2021_keka)[13] <- "Skjellprove"

meta <- plyr::rbind.fill(NALO_2021, NALO_2021_keka)

str(meta)

meta <- meta[meta$ID!='',]
summary(as.factor(meta$Stasjon))
