#Create figures over samples collected
###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################


#Create metadata table for samples
library(dplyr)
library(plyr)
library(ggplot2)

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

#select the correct Hitra-samples
meta <- meta[which (!(meta$Stasjon=='Hitra'&meta$Luseteller=='Rolf Sivertsgård')), ]
meta <- meta[!((meta$ID=='HIT-02'
                         |meta$ID=='HIT-07'
                         |meta$ID=='HIT-12'
                         |meta$ID=='HIT-16'
                         |meta$ID=='HIT-17'
                         |meta$ID=='HIT-24'
                         |meta$ID=='HIT-27')
                        &meta$Stasjon=='Hitra'), ]

str(meta)

meta <- meta[meta$ID!='',]
summary(as.factor(meta$Stasjon))
meta$Stasjon[meta$Stasjon=='Ã˜rsta'] <- 'Ørsta'
meta$Stasjon[meta$Stasjon=='MÃ¥lÃ¸y'] <- 'Måløy'
meta$Stasjon[meta$Stasjon=='Ytre Ã…rdalsfjord'] <- 'Ytre_Årdalsfjord'
meta$Stasjon[meta$Stasjon=='LÃ¸ksebotn'] <- 'Løksebotn'

str(meta)

#meta$tmp <- meta$Dato
#meta$Dato <- NA
#meta$Dato <- as.POSIXct(meta$tmp, format = "%d.%m.%Y", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%dd-%mm-%yyyy", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%d-%m-%Y", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%D-%M-%Y", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%d-%m-%y", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%d-%m-%yy", optional = TRUE)
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%dd-%mm-%yyyy", optional = TRUE)


#meta$tmp[is.na(meta$Dato)]
#meta$Dato[is.na(meta$Dato)] <- as.POSIXct(meta$tmp, format = "%d-%m-%d", optional = TRUE)

summary(as.factor(meta$Stasjon))

tmp1 <- meta[meta$Stasjon=='Flekkefjord'|meta$Stasjon=='Sandnesfjord'|meta$Stasjon=='Hitra', ]
tmp1$Dato <- as.POSIXct(tmp1$Dato, format = "%d.%m.%Y")
tmp2 <- meta[meta$Stasjon=='Ytre_Årdalsfjord'|meta$Stasjon=='Etne'|meta$Stasjon=='Måløy'|meta$Stasjon=='Ørsta'|meta$Stasjon=='Vikna'|meta$Stasjon=='Leirfjord', ]
tmp2$Dato[tmp2$ID=='231'] <- '2021-05-22'
tmp2$Dato[tmp2$ID=='NF 45'] <- '2021-05-22'
tmp2$Dato[tmp2$ID=='NF 105'] <- '2021-05-24'
tmp2$Dato[tmp2$ID=='NF 144'] <- '2021-05-24'
tmp2$Dato[tmp2$ID=='NF 364'] <- '2021-06-04'
tmp2$Dato <- as.POSIXct(tmp2$Dato, format = "%Y-%m-%d")
tmp2$Dato

tmp3 <- meta[meta$Stasjon=='Steigen'|meta$Stasjon=='Løksebotn'|meta$Stasjon=='Oksfjord'|meta$Stasjon=='Handelsbukt'|meta$Stasjon=='Jarfjord'|meta$Stasjon=='Balestrand', ]
tmp3$Dato <- as.POSIXct(tmp3$Dato, format = "%d-%m-%Y")
str(tmp3)

meta <- rbind(tmp1, tmp2, tmp3)
str(meta)


###############################################################
###############################################################
#Check the location of samples 13.01.2021
###############################################################
meta$location_jan22 <- NA

#Flekkefjord
meta$ID[meta$Stasjon=='Flekkefjord' & meta$Dato>'2021-06-03' & meta$Dato<'2021-06-07']
meta$location_jan22[meta$Stasjon=='Flekkefjord' & meta$Dato>'2021-06-03' & meta$Dato<'2021-06-07'] <- 'NTNU_MUSEUM'
summary(as.factor(meta$location_jan22))

#Sandnesfjord
meta$location_jan22[meta$Stasjon=='Sandnesfjord'] <- 'MISSING'

#Årdal
meta$ID[meta$Stasjon=='Ytre_Årdalsfjord']
meta$location_jan22[meta$Stasjon=='Ytre_Årdalsfjord'] <- 'NTNU_MUSEUM'
meta$location_jan22[meta$ID=='SO-211'] <- 'NTNU_MUSEUM_UNSURE_ID'
meta$location_jan22[meta$ID=='SO-224'] <- 'NTNU_MUSEUM_UNSURE_ID'

#Etne
meta$ID[meta$Stasjon=='Etne']
meta$location_jan22[meta$Stasjon=='Etne'] <- 'NTNU_MUSEUM'

#Balestrand
meta$ID[meta$Stasjon=='Balestrand']
meta$location_jan22[meta$Stasjon=='Balestrand'] <- 'NTNU_MUSEUM'

#Måløy
meta$ID[meta$Stasjon=='Måløy']
meta$location_jan22[meta$Stasjon=='Måløy'] <- 'NTNU_MUSEUM'
meta$location_jan22[meta$ID=='NF 30'|
                    meta$ID=='NF 31'|
                    meta$ID=='NF 34'] <- NA
meta$location_jan22[meta$ID=='NF 346'|
                      meta$ID=='NF 364'] <- 'NTNU_MUSEUM?'
meta$ID[meta$ID=='NF 37A'] <- 'NF 37'

#Ørsta
meta$ID[meta$Stasjon=='Ørsta']
meta$location_jan22[meta$Stasjon=='Ørsta'] <- 'NTNU_MUSEUM'

#Vikna
meta$ID[meta$Stasjon=='Vikna']
meta$location_jan22[meta$Stasjon=='Vikna'] <- 'NTNU_MUSEUM'

#Leirfjord
meta$ID[meta$Stasjon=='Leirfjord']
meta$location_jan22[meta$Stasjon=='Leirfjord'] <- 'NTNU_MUSEUM'

#Steigen
meta$ID[meta$Stasjon=='Steigen']
meta$location_jan22[meta$Stasjon=='Steigen'] <- 'NTNU_MUSEUM'

#Løksebotn
meta$ID[meta$Stasjon=='Løksebotn']
meta$location_jan22[meta$Stasjon=='Løksebotn'] <- 'NTNU_MUSEUM'

#Oksfjord
meta$ID[meta$Stasjon=='Oksfjord']
meta$location_jan22[meta$Stasjon=='Oksfjord'] <- 'NTNU_MUSEUM'

#Handelsbukt
meta$ID[meta$Stasjon=='Handelsbukt']
meta$location_jan22[meta$Stasjon=='Handelsbukt'] <- 'NTNU_MUSEUM'

#Jarfjorden
meta$ID[meta$Stasjon=='Jarfjord']
meta$location_jan22[meta$Stasjon=='Jarfjord'] <- 'NTNU_MUSEUM'

summary(as.factor(meta$location_jan22))

#Hitra
meta$ID[meta$Stasjon=='Hitra']
tmp <- meta[meta$Stasjon=='Hitra',]
meta$location_jan22[meta$Stasjon=='Hitra' & meta$Dato<'2021-06-20'] <- '?'

#NTNU_freezer_location
meta <- meta[which (!is.na(meta$ID)), ]
meta$ntnu_freezer_location <- ''
meta$ntnu_freezer_location[meta$Stasjon=='Flekkefjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box2_white'
summary(as.factor(meta$ntnu_freezer_location))

meta$ntnu_freezer_location[meta$Stasjon=='Ytre_Årdalsfjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')
                           & (meta$ID=='SO-204'
                              |meta$ID=='SO-205'
                              |meta$ID=='SO-206'
                              |meta$ID=='SO-207'
                              |meta$ID=='SO-211'
                              |meta$ID=='SO-219'
                              |meta$ID=='SO-224'
                              |meta$ID=='SO-254'
                              |meta$ID=='SO-255'
                              |meta$ID=='SO-260')] <- 'box2_green'

meta$ntnu_freezer_location[meta$Stasjon=='Ytre_Årdalsfjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')
                           & (meta$ID=='SO-376'
                              |meta$ID=='SO-377'
                              |meta$ID=='SO-384'
                              |meta$ID=='SO-385'
                              |meta$ID=='SO-387'
                              |meta$ID=='SO-388'
                              |meta$ID=='SO-389'
                              |meta$ID=='SO-390'
                              |meta$ID=='SO-392'
                              |meta$ID=='SO-406'
                              |meta$ID=='SO-407'
                              |meta$ID=='SO-431'
                              |meta$ID=='SO-435')] <- 'box1_green'

meta$ntnu_freezer_location[meta$Stasjon=='Etne' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box2_yellow'

meta$ntnu_freezer_location[meta$Stasjon=='Balestrand' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box4_white'

meta$ntnu_freezer_location[meta$Stasjon=='Måløy' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box3_blue'

meta$ntnu_freezer_location[meta$Stasjon=='Ørsta' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box3_blue'

meta$ntnu_freezer_location[meta$Stasjon=='Vikna' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box1_grey'

meta$ntnu_freezer_location[meta$Stasjon=='Leirfjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box1_purple'

meta$ntnu_freezer_location[meta$Stasjon=='Steigen' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box3_pink'

meta$ntnu_freezer_location[meta$Stasjon=='Løksebotn' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box2_orange'

meta$ntnu_freezer_location[meta$Stasjon=='Oksfjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box1_red'

meta$ntnu_freezer_location[meta$Stasjon=='Handelsbukt' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box2_blue'

meta$ntnu_freezer_location[meta$Stasjon=='Jarfjord' 
                           & (meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID')] <- 'box1_brown'

summary(as.factor(meta$ntnu_freezer_location))
###############################################################
###############################################################

meta$location_jan22[meta$Stasjon=='Hitra'] <- 'Sindre_freezer'
meta <- meta[meta$location_jan22=='NTNU_MUSEUM'|meta$location_jan22=='NTNU_MUSEUM_UNSURE_ID'|meta$location_jan22=='?'|meta$location_jan22=='Sindre_freezer'|, ]
meta <- meta[which (!is.na(meta$ID)), ]


str(meta)
meta <- meta[c("ID","Dato","Stasjon","Breddegrad","Lengdegrad","Art","Redskap","Maskestr","Skjellprove","Vekt","Lengde_gaf","Lengde_tot","Lagret_sep_2021","location_jan22", "ntnu_freezer_location")]
meta$nalo_id <- meta$ID
meta$gill_sample_id <- meta$ID
colnames(meta)
samples_2020 <- read.csv("./data/modified_data/WP1_samples_2020.csv")
colnames(samples_2020)
samples_2020$ID <- samples_2020$ID..
samples_2020$nalo_id <- samples_2020$ID..
samples_2020$Lagret_sep_2021 <- ''
samples_2020$ntnu_freezer_location <- ''
samples_2020$Skjellprove <- samples_2020$Skjellprøve
samples_2020$Lengde_gaf <- samples_2020$Lengde.gaf
samples_2020$Lengde_tot <- samples_2020$Lengde.tot
samples_2020$location_jan22 <- 'NTNU_MUSEUM'
samples_2020 <- samples_2020[c("ID","Dato","Stasjon","Breddegrad","Lengdegrad","Art","Redskap","Maskestr","Skjellprove","Vekt","Lengde_gaf","Lengde_tot","Lagret_sep_2021", "location_jan22", "ntnu_freezer_location", "nalo_id", "gill_sample_id")]

meta <- rbind(meta, samples_2020)


tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise(sample_size = n())

str(tmp)

#Sea trout
p1 <- ggplot(tmp, aes(x = sample_size, y = Stasjon))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("NALO SAMPLES 2020-2021") +xlab("Number of samples")+ ylab("Sampling location")
p1

str(meta)

meta2020 <- meta[meta$Dato<'2021-01-01',]
tmp2 <- meta2020 %>%
  group_by(Stasjon) %>%
  dplyr::summarise(sample_size = n(), min_date = min(Dato), max_date = max(Dato), mean_size = mean(Lengde_tot), sd_size = sd(Lengde_tot), min_size = min(Lengde_tot), max_size = max(Lengde_tot), latitude = first(Breddegrad))
tmp2$size <- paste(round(tmp2$mean_size, digits = 0), "±", round(tmp2$sd_size, digits = 0), " (", tmp2$min_size, "-", tmp2$max_size, ")", sep = '')
tmp2$sampling_period <- paste(format(tmp2$min_date, format = "%d.%m"), " - ", format(tmp2$max_date, format = "%d.%m"), sep = '')
tmp2$sampling_year <- '2020'
tmp2$sampling_site <- tmp2$Stasjon
tmp2 <- tmp2[c("sampling_year", "sampling_site", "sample_size", "sampling_period", "size", "latitude")]
names(tmp2) <- c("sampling_year", "sampling_site", "sample_size (n)", "sampling_period (dd.mm)", "tot_body_length (mm)", "latitude")
summary_2020 <- tmp2


library(dplyr)

#select and filter data table
meta2021 <- meta[meta$Dato>'2021-01-01',]
tmp2 <- meta2021 %>% #making a new sumary table called tmp2 with the summarized data
  group_by(Stasjon) %>%
  dplyr::summarise(sample_size = n(), min_date = min(Dato), max_date = max(Dato), mean_size = mean(Lengde_tot), sd_size = sd(Lengde_tot), min_size = min(Lengde_tot), max_size = max(Lengde_tot), latitude = first(Breddegrad))
#the following lines are to use paste function to combine and format colums made by the previous summarize function
tmp2$size <- paste(round(tmp2$mean_size, digits = 0), "±", round(tmp2$sd_size, digits = 0), " (", tmp2$min_size, "-", tmp2$max_size, ")", sep = '')
tmp2$sampling_period <- paste(format(tmp2$min_date, format = "%d.%m"), " - ", format(tmp2$max_date, format = "%d.%m"), sep = '')
tmp2$sampling_year <- '2021' # column with constand value
tmp2$sampling_site <- tmp2$Stasjon
tmp2 <- tmp2[c("sampling_year", "sampling_site", "sample_size", "sampling_period", "size", "latitude")] #select the columns you want to keep
names(tmp2) <- c("sampling_year", "sampling_site", "sample_size (n)", "sampling_period (dd.mm)", "tot_body_length (mm)", "latitude") #rename column names
summary_2021 <- tmp2

summary_table <- rbind(summary_2020, summary_2021)
write.csv(summary_table, "./data/modified_data/samples_summary.csv", row.names = FALSE)



#Assign location of samples
meta$location_jan22[meta$Stasjon=='Hitra'] <- 'NTNU_MUSEUM'
meta$ntnu_freezer_location[meta$Stasjon=='Hitra' 
                           &meta$Dato<'2021-01-01'] <- 'box6_red'
meta$ntnu_freezer_location[meta$Stasjon=='Hitra' 
                           &meta$Dato>'2021-01-01'] <- '?'

meta$ntnu_freezer_location[meta$Stasjon=='Balestrand' 
                           &meta$Dato<'2021-01-01'] <- 'box5_grey'

meta$ntnu_freezer_location[meta$Stasjon=='Nordfjord' 
                           &meta$Dato<'2021-01-01'] <- 'box5_blue'

meta$ntnu_freezer_location[meta$Stasjon=='Etne' 
                           &meta$Dato<'2021-01-01'] <- 'box6_clear_slim_tubes'

meta$ntnu_freezer_location[meta$Stasjon=='Etne' 
                           &meta$Dato<'2021-01-01'] <- 'box6_clear_slim_tubes'


meta$ntnu_freezer_location[meta$Stasjon=='Oksfjordhamn' 
                           &meta$Dato<'2021-01-01'] <- 'box6_purple'


meta$ntnu_freezer_location[meta$Stasjon=='Løksebotn' 
                           &meta$Dato<'2021-01-01'] <- 'box6_clear_purple'

meta$ntnu_freezer_location[meta$Stasjon=='Løksebotn' 
                           &meta$Dato<'2021-01-01'] <- 'box6_clear_purple'

meta$ntnu_freezer_location[meta$Stasjon=='Herøyosen' 
                           &meta$Dato<'2021-01-01'] <- 'box5_clear_slim_tubes'

meta$ntnu_freezer_location[meta$Stasjon=='Herdlafjord' 
                           &meta$Dato<'2021-01-01'] <- 'box5_white'

meta$ntnu_freezer_location[meta$Stasjon=='Herdlafjord' 
                           &meta$Dato<'2021-01-01'
                           &(meta$ID=='HO-13'|meta$ID=='HO-25'|meta$ID=='HO-32'|meta$ID=='HO-34')] <- 'box5_clear_slim_tubes'


ggsave("./data/modified_data/NALO_2020_2021_SAMPLES.tiff", p1, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

write.csv(meta, "./data/modified_data/meta.csv", row.names = FALSE)


