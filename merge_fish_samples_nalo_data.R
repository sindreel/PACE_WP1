#All sampled fish WP1 and NALO data combination
library(lubridate)
library(stringr)
#Read data
Sys.setlocale(locale='no_NB.utf8')

nalo <- read.csv("./data/raw_data/NALO_2020_2022.csv", sep=";")

fishdata <- read.csv("./data/raw_data/all_analysed_WP1_samples.csv", sep = ";")
fishdata$Dato[fishdata$nalo_id=='VL107'] <- '02.06.2020'
fishdata$year <- floor_date(dmy(fishdata$Dato), unit = "years")
summary(as.factor(fishdata$year))
summary(as.factor(fishdata$Stasjon))

fishdata$Stasjon[fishdata$Stasjon=='Handelsbukt'] <- "Handelsbukt_Jarfjord_2022"

summary(as.factor(nalo$station))
nalo$station[nalo$station=='HerOyosen'] <- "Heroyosen"
nalo$station[nalo$station=='LOksebotn'] <- "Loksebotn"
nalo$station[nalo$station=='MAloy'] <- "Maloy"
nalo$station[nalo$station=='MAlOy'] <- "Maloy"
nalo$station[nalo$station=='Ytre Ardalsfjord'] <- "Ytre_Ardalsfjord"
nalo$station[nalo$station=='Reisa'] <- "Oksfjord"
nalo$station[nalo$station=='Oksfjordhamn'] <- "Oksfjord"
nalo$station[nalo$station=='Handelsbukt'] <- "Handelsbukt_Jarfjord_2022"
nalo$station[nalo$station=='Jarfjord'&nalo$year=='2022-01-01'] <- "Handelsbukt_Jarfjord_2022"


nalo$year <- floor_date(dmy(nalo$date), unit = "years")


fishdata$nalo_id[fishdata$Stasjon=='Handelsbukt'] <- fishdata$gill_sample_id[fishdata$Stasjon=='Handelsbukt']
fishdata$nalo_id <- str_replace(fishdata$nalo_id, "Foe", "FO")

str(nalo)

str(fishdata)
fishdata$key <- paste(fishdata$Stasjon, fishdata$nalo_id, fishdata$year)
nalo$key <- paste(nalo$station, nalo$nalo_id, nalo$year)
data <- merge(fishdata[c("key", "gill_sample_id")], nalo, by="key", all.x=TRUE)

summary(as.factor(data$station))
data[is.na(data$station),]

data <- data[!is.na(data$station),]
summary(as.factor(data$station))
str(data)

saveRDS(data, "./data/modified_data/NALO_data_PACE_WP1_fish.RDS")

summary(as.factor(fishdata$Stasjon))
saveRDS(fishdata, "./data/modified_data/all_analysed_samples_PACE_WP1.RDS")
saveRDS(nalo, "./data/modified_data/all_nalo_data_2020_2021_2022.RDS")
