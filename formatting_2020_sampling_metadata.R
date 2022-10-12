#Organize 2020-metadata

NALO_2020 <- read.csv("./data/raw_data/NALO_2020.csv", sep = ";")
str(NALO_2020)
summary(as.factor(NALO_2020$Stasjon))
NALO_2020$Dato
NALO_2020$Dato <- as.POSIXct(NALO_2020$Dato, format = "%d.%m.%Y", optional = TRUE)



sampling_sheets_2020 <- read.csv("./data/raw_data/PACE_WP1_Sampling_sheets_2020.csv", sep = ";")
str(sampling_sheets_2020)
summary(as.factor(sampling_sheets_2020$location))


sampling_sheets_2020$ID <- ''
str(sampling_sheets_2020)
sampling_sheets_2020$Stasjon[sampling_sheets_2020$location=='Herdla'] <- 'Herdlafjord'
sampling_sheets_2020$Stasjon[sampling_sheets_2020$location=='Austfjorden_heroyosen_aureholen'] <- 'Herøyosen'
sampling_sheets_2020$Stasjon[sampling_sheets_2020$location=='Austfjorden_heroyosen_baldersvagen'] <- 'Herøyosen'
sampling_sheets_2020$Stasjon[sampling_sheets_2020$location=='Balestrand'] <- 'Balestrand'
#sampling_sheets_2020$Stasjon[sampling_sheets_2020$location=='Straumfjorden, Hitra'] <- 'Hitra'

summary(as.factor(sampling_sheets_2020$location))

sampling_sheets_2020$key <- interaction(sampling_sheets_2020$Stasjon, sampling_sheets_2020$total_lenght_mm, sampling_sheets_2020$weight_g)
tmp <- sampling_sheets_2020[duplicated(sampling_sheets_2020$key), ]

str(NALO_2020)
tmp <- NALO_2020[NALO_2020$Stasjon=='Herdlafjord'
                       |NALO_2020$Stasjon=='Herøyosen'
                       |NALO_2020$Stasjon=='Balestrand', ]
tmp$ID <- tmp$ID..
str(tmp)
tmp$key <- interaction(tmp$Stasjon, tmp$Lengde.tot, tmp$Vekt)
tmp <- merge(tmp, sampling_sheets_2020[c("key", "gill_sample_id", "nr_of_sea_lice")], by = "key")
str(tmp)
tmp2 <- tmp[c("ID", "Stasjon", "Dato", "Lengde.tot", "Vekt", "key", "gill_sample_id", "nr_of_sea_lice")]
tmp2 <- tmp2[!is.na(tmp2$key), ]
summary(as.factor(tmp2$Stasjon))
summary(as.factor(sampling_sheets_2020$Stasjon))
tmp3 <- sampling_sheets_2020[sampling_sheets_2020$location=='see_nalo_spreadsheet_ask_rune_nilsen_varies_between_fish', ]
str(NALO_2020)
str(tmp3)
NALO_2020$ID <- NALO_2020$ID..
tmp3 <- merge(NALO_2020, tmp3[c("gill_sample_id")], by.x ="ID..", by.y = "gill_sample_id")
str(tmp3)
tmp3 <- tmp3[c("ID", "Stasjon", "Dato", "Lengde.tot", "Vekt")]
tmp3$key <- ''
tmp3$gill_sample_id <- tmp3$ID
tmp3$nr_of_sea_lice <- ''

tmp4 <- rbind(tmp2, tmp3)
summary(as.factor(tmp4$Stasjon))
summary(as.factor(sampling_sheets_2020$location))
str(sampling_sheets_2020)
tmp4$nalo_id <- tmp4$ID
sampling_sheets_2020 <- merge(sampling_sheets_2020, tmp4[c("gill_sample_id", "nalo_id")], by = "gill_sample_id", all.x = TRUE)
sampling_sheets_2020$nalo_id[sampling_sheets_2020$nr_of_sea_lice=='nalo_22_85lice'] <- 'HO-22'
sampling_sheets_2020$nalo_id[sampling_sheets_2020$nr_of_sea_lice=='nalo_24'] <- 'HO-24'
sampling_sheets_2020$nalo_id[sampling_sheets_2020$nr_of_sea_lice=='nalo_26'] <- 'HO-26'
sampling_sheets_2020$nalo_id[sampling_sheets_2020$nr_of_sea_lice=='nalo_17'] <- 'HO-17'
sampling_sheets_2020$nalo_id[sampling_sheets_2020$gill_sample_id=='VL107'] <- 'VL107'

str(tmp4)
str(NALO_2020)
tmp5 <- merge(NALO_2020, tmp4[c("nalo_id","gill_sample_id")], by.x ="ID", by.y = "nalo_id")
str(tmp5)
tmp5 <- tmp5[c("ID", "Stasjon", "Dato", "Lengde.tot", "Vekt", "gill_sample_id")]


#Find all Hitra samples 2020 and append to tmp5 to make control table

summary(as.factor(NALO_2020$Stasjon))
nalo_hitra <- NALO_2020[(NALO_2020$ID=='HIT - 021'
                         |NALO_2020$ID=='HIT - 022'
                         |NALO_2020$ID=='HIT - 023'
                         |NALO_2020$ID=='HIT - 024'
                         |NALO_2020$ID=='HIT - 025'
                         |NALO_2020$ID=='HIT - 026'
                         |NALO_2020$ID=='HIT - 027'
                         |NALO_2020$ID=='HIT - 028'
                         |NALO_2020$ID=='HIT - 029'
                         |NALO_2020$ID=='HIT - 030'
                         |NALO_2020$ID=='HIT - 031'
                         |NALO_2020$ID=='HIT - 032'
                         |NALO_2020$ID=='HIT - 033'
                         |NALO_2020$ID=='HIT - 034'
                         |NALO_2020$ID=='HIT - 035'
                         |NALO_2020$ID=='HIT - 036'
                         |NALO_2020$ID=='HIT - 037'
                         |NALO_2020$ID=='HIT - 038'
                         |NALO_2020$ID=='HIT - 039'
                         |NALO_2020$ID=='HIT - 040')
                        &NALO_2020$Stasjon=='Hitra', ]
str(nalo_hitra)
nalo_hitra <- nalo_hitra[c("ID", "Stasjon", "Dato", "Lengde.tot", "Vekt")]
nalo_hitra$gill_sample_id <- nalo_hitra$ID

tmp6 <- rbind(tmp5, nalo_hitra)

write.csv(tmp6, "./data/modified_data/coltrol_sheet_NALO2020.csv", row.names = FALSE)


norce_troms <- NALO_2020[(NALO_2020$ID=='186'
                         |NALO_2020$ID=='206'
                         |NALO_2020$ID=='193'
                         |NALO_2020$ID=='235'
                         |NALO_2020$ID=='194'
                         |NALO_2020$ID=='187')
                         &NALO_2020$Område..Overvåkingsområde.=='Troms',]

etne_2020 <- NALO_2020[(NALO_2020$ID=='511'
                        |NALO_2020$ID=='510'
                        |NALO_2020$ID=='526'
                        |NALO_2020$ID=='512'
                        |NALO_2020$ID=='528'
                        |NALO_2020$ID=='513'
                        |NALO_2020$ID=='527'
                        |NALO_2020$ID=='583'
                        |NALO_2020$ID=='585'
                        |NALO_2020$ID=='584'
                        |NALO_2020$ID=='581'
                        |NALO_2020$ID=='514'
                        |NALO_2020$ID=='451'
                        |NALO_2020$ID=='455'
                        |NALO_2020$ID=='454'
                        |NALO_2020$ID=='453'
                        |NALO_2020$ID=='457'
                        |NALO_2020$ID=='461'
                        |NALO_2020$ID=='437'
                        |NALO_2020$ID=='436')
                       &NALO_2020$Stasjon=='Etne', ]

nordfjord_2020 <- NALO_2020[NALO_2020$Stasjon=='Nordfjord', ]

nordfjord_2020 <- NALO_2020[(NALO_2020$ID=='NF 950'
                        |NALO_2020$ID=='NF 949'
                        |NALO_2020$ID=='NF 948'
                        |NALO_2020$ID=='NF 700'
                        |NALO_2020$ID=='NF 775'
                        |NALO_2020$ID=='NF 551'
                        |NALO_2020$ID=='NF 513'
                        |NALO_2020$ID=='NF 512'
                        |NALO_2020$ID=='NF 778'
                        |NALO_2020$ID=='NF 511'
                        |NALO_2020$ID=='NF 510'
                        |NALO_2020$ID=='NF 776'
                        |NALO_2020$ID=='NF 777')
                       &NALO_2020$Stasjon=='Nordfjord', ]



tmp_samples <- rbind(etne_2020, nordfjord_2020, norce_troms)
str(tmp_samples)
tmp_samples <- tmp_samples[c("ID", "Stasjon", "Dato", "Lengde.tot", "Vekt")]
tmp_samples$gill_sample_id <- tmp_samples$ID

tmp7 <- rbind(tmp6, tmp_samples)

tmp7$key <- interaction(tmp7$ID, tmp7$Stasjon, tmp7$Lengde.tot)

NALO_2020$key <- interaction(NALO_2020$ID, NALO_2020$Stasjon, NALO_2020$Lengde.tot)

WP1_samples_2020 <- merge(NALO_2020, tmp7[c("ID", "gill_sample_id", "key")],  by = 'key')


write.csv(WP1_samples_2020, "./data/modified_data/WP1_samples_2020.csv", row.names = FALSE)
