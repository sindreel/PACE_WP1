#Sorting pathogen data that was received September 2023

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################
options(scipen=999)
#test

#########################################################################
#download Pathogen data
#########################################################################

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------

# This script downloads and merges the data needed for further analyses for the CHASES WP2 (Marine migration and physiology) paper.
# End product of this script is a .csv file containing fish info and key migratory variables (timing of marine entry, timing of marine exit, marine residence duration and categorized migratory distance)


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)


#Pahtogen list 11-2021:
pathogen_list <- "https://ntnu.box.com/shared/static/0gcqxbw2662n4tcd2lf8e44m697pxjxg.csv" #Uploaded new version 18.12.20
download.file(url=pathogen_list,destfile="./data/raw_data/pathogen_list_2023.csv")  

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)

#Read pathogen list
pathogen_list <- read.csv("./data/raw_data/pathogen_list_2023.csv", sep = ";")
#pathogen_list$agent_name[pathogen_list$agent_name=='Piscine orthoreovirus'] <- 'Piscine orthoreovirus -3'
pathogen_list <- pathogen_list[pathogen_list$assay_id!="81", ]
str(pathogen_list)
summary(as.factor(pathogen_list$agent_name))
str(pathogen_list)
pathogen_list$ran_in_2020 <- pathogen_list$Ran.in.Norway.2020_SL..outmigrating.
summary(droplevels(as.factor(pathogen_list$ran_in_2020)))
pathogen_list$ran_in_2020 <- str_replace(pathogen_list$ran_in_2020, "ran isav7", "yes")
pathogen_list$ran_in_2020 <- str_replace(pathogen_list$ran_in_2020, "46", "")

pathogen_list$ran_in_2021 <- paste(pathogen_list$Ran.in.Norway_2021_Microbe_runs_AS..retun.migrating..4089.4090..48x2., pathogen_list$Ran.in.Norway_2021_Fitchip_runs_AS..retun.migrating..4186.4187..1x2..1x9., sep="_")
summary(droplevels(as.factor(pathogen_list$ran_in_2021)))
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes_", "yes")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "_yessingleton", "yes_singleton")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-singleton-4187 only", "singleton_only_4187")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-4090 onlyyes in duplicate-4186 only", "yes")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "_", "")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "NANA", "")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-4090 onlyyes in duplicate-4186 only", "yes")

pathogen_list$ran_in_2023 <- paste(pathogen_list$Run.in.Norway_2023_Microbe.chip..48x2., pathogen_list$Run.on.Norway_2003_.Fit.chip..4x2..3x1., sep="_")
summary(droplevels(as.factor(pathogen_list$ran_in_2023)))
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "NA_NA", "")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_", "")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes-singleton", "yes_singleton")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "yes-isav8", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "yes_", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "NA", "")
pathogen_list$ran_in_2023[pathogen_list$Run.on.Norway_203_.Fit.chip..4x2..3x1.=='yes'] <- 'yes'
pathogen_list$ran_in_2023[pathogen_list$Run.on.Norway_203_.Fit.chip..4x2..3x1.=='yes-singleton'] <- 'yes_singleton'


#Read assays
full_list <- read.csv("./data/raw_data/WP1_individual_assays.csv", sep = ";")
str(full_list)

duplicated(full_list$alternate_num)


full_list_long_copy <- full_list %>%
  select(c(69:116, "fish_num", "alternate_num", "stock_origin", "scientific_name"))%>%
  select("fish_num", "alternate_num", "stock_origin", "scientific_name", contains('copy')) %>%
  group_by(fish_num,alternate_num, stock_origin) %>%
  gather(key = "assay", value = value, -fish_num, -alternate_num, -stock_origin, -scientific_name) %>%
  filter(!is.na(value))%>%
  mutate_at(c(1:5), as.factor)

str(full_list_long_copy)
full_list_long_copy$vial <- paste(full_list_long_copy$stock_origin, "_", full_list_long_copy$alternate_num, sep='')

#Inspect the number of assays for each fish
tmp <- full_list_long_copy %>%
  group_by(stock_origin, assay) %>%
  dplyr::summarize(n())
#Comment - duplicates are not a problem


str(full_list_long_copy)

#Using this as our data, we can map value as our x variable, and use facet_wrap to separate by the key column:

p1 <- ggplot(full_list_long_copy, aes(value)) + 
  geom_histogram() +#bins = 10 
  facet_wrap(~assay, scales = 'free')

#ggsave("./data/modified_data/histogram_copy_numbers.tiff", p1, units="cm", width=80, height=50, dpi=600, compression = 'lzw', limitsize = FALSE)

saveRDS(full_list_long_copy,"./data/modified_data/full_assay_list_long_copy_101023.RDS")

summary(as.factor(full_list_long_copy$assay))
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv.1", "prv-1")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv.3", "prv-3")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_1", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_18", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_20", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_21", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_27", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_29", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_37", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_38", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_39", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_41", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_47", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_50", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_51", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "psy8", "psy")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_Gill_copy", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "isav8", "isav")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "c_b_cys3", "c_b_cys")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Lesa_COI", "le_sa")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Ichy_Costia", "ic_spp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ihnv_22", "ihnv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "nu_sal_33", "nu_sal")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_min_36", "pa_min")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_min_36", "pa_min")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ascv_a", "ascv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ascv41", "ascv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "myco_sp", "my_sp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "rlo_45", "rlo")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_copy", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_58", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_23", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_25", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_26", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_28", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_32", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_454", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_35", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_40", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_42", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_44", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_46", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_48", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_49", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_52", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_53", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_54", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_55", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_56", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_61", "")

full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Ic_spp.2", "Ic_spp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ae_sal2", "ae_sal")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "cr_sal5", "cr_sal")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "p.totiv64", "p.totiv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_atl9", "pa_atl")

full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_sky7", "pa_sky")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "sgpv61", "sgpv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "te_dic5", "te_dic")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "te_fin3", "te_fin")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv-34", "prv-3")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Ic_spp", "ic_spp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "p.rotav", "p-rotav")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "p.totiv", "p-totiv")



#Remove house keeping gene
full_list_long_copy <- full_list_long_copy[full_list_long_copy$assay!='HK_S1000', ]

pathogen_list$December.2021.Onward.Names <- str_replace(pathogen_list$December.2021.Onward.Names, "Ic_spp ", "ic_spp")


pathogen_list$December.2021.Onward.Names

unique(full_list_long_copy$vial)
summary(as.factor(full_list_long_copy$scientific_name))

str(full_list_long_copy)

pathogens_long_trout_salmon <- full_list_long_copy
str(pathogens_long_trout_salmon)
#merge LOD copy columns and evauate number of pathogens that get cut off:

tmp <- pathogens_long_trout_salmon[unique(pathogens_long_trout_salmon$assay), ] 
pathogen_list$December.2021.Onward.Names
pathogen_list$assay <- pathogen_list$December.2021.Onward.Names
str(pathogen_list)
temp <- merge(pathogen_list[c("December.2021.Onward.Names", "assay")], pathogens_long_trout_salmon[c("assay", "value")], by = "assay",)

pathogen_list$LOD_Copy_95. <- as.numeric(pathogen_list$LOD_Copy_95.)
pathogen_list$LOD_Copy_75. <- as.numeric(pathogen_list$LOD_Copy_75.)
pathogens_long_trout_salmon <- merge(pathogens_long_trout_salmon, pathogen_list[c("assay", "LOD_Copy_95.", "LOD_Copy_75.")], by = "assay")
#find which assays have not been merged properly
summary(as.factor(pathogens_long_trout_salmon$assay))
summary(as.factor(full_list_long_copy$assay))

str(pathogens_long_trout_salmon)
str(pathogen_list)
pathogens_long_trout_salmon$value[is.na(pathogens_long_trout_salmon$LOD_Copy_75.)] <- 0
pathogens_long_trout_salmon$passed_75 <- '' 
pathogens_long_trout_salmon$passed_75[pathogens_long_trout_salmon$value>=pathogens_long_trout_salmon$LOD_Copy_75.] <- 'yes'
pathogens_long_trout_salmon$passed_95 <- '' 
pathogens_long_trout_salmon$passed_95[pathogens_long_trout_salmon$value>=pathogens_long_trout_salmon$LOD_Copy_95.] <- 'yes'
summary(as.factor(pathogens_long_trout_salmon$passed_75))
summary(as.factor(pathogens_long_trout_salmon$passed_95))
tmp <- pathogens_long_trout_salmon$value[pathogens_long_trout_salmon$value>0]

#According to angela we should use the copy numbers for the pathogens.
#Lets bring in the fishdata to try to replicate the RIB from Robs paper.
pathogens_long_trout_salmon$log_value <- log(pathogens_long_trout_salmon$value) # Make log value
pathogens_long_trout_salmon$log_value[pathogens_long_trout_salmon$log_value<0] <- 0
pathogens_long_trout_salmon$log_value[is.na(pathogens_long_trout_salmon$log_value)] <- 0
pathogens_long_trout_salmon$log_value[pathogens_long_trout_salmon$log_value==-Inf] <- 0
summary(pathogens_long_trout_salmon$log_value)

summary(as.factor(pathogens_long_trout_salmon$vial), maxsum = 999)


saveRDS(pathogens_long_trout_salmon, "./data/modified_data/all_pathogens_long.RDS")
write.csv(pathogens_long_trout_salmon, "./data/modified_data/all_pathogens_long.csv", row.names = FALSE)
head(pathogens_long_trout_salmon)



#################################################################################
#All_trout
#################################################################################
str(pathogens_long_trout_salmon)
summary(as.factor(pathogens_long_trout_salmon$Spp))
trout_pathogens <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$scientific_name=='Salmo trutta', ]
head(trout_pathogens)
library(dplyr)
#Test with 75% LOD
str(trout_pathogens)
trout_75 <- trout_pathogens[trout_pathogens$passed_75=='yes', ]
max_path <- trout_75 %>%
  group_by(assay) %>%
  dplyr::summarize(max_path = max(log_value))
summary(max_path)
head(trout_75)
head(max_path)
trout_75 <- merge(trout_75, max_path, by = "assay")
trout_75$rib <- trout_75$log_value/trout_75$max_path
summary(trout_75$rib)
trout_75$rib[is.nan(trout_75$rib)] <- 0

tot_rib <- trout_75 %>%
  group_by(vial)%>%
  dplyr::summarize(tot_rib = sum(rib))
trout_75 <- merge(trout_75, tot_rib, by="vial")
levels = unique(trout_75$vial[order(trout_75$tot_rib)])

trout_75$vial <- factor(trout_75$vial,                                    # Change ordering manually
                        levels = levels)
head(trout_75)
str(trout_75)
ggplot(trout_75, aes(y=value, x=assay, col=vial)) + geom_point()
summary(trout_75$rib)

head((trout_75))


# Grouped
p0 <- ggplot(trout_75, aes(fill=assay, x=rib, y=vial, drop=FALSE)) + 
  geom_bar(stat="identity")+
  facet_wrap(~stock_origin, scales = "free_y", ncol=8) + theme_classic(base_size = 14) +  scale_fill_discrete(breaks = c('c_b_cys', 'fl_psy', 'ic_hof', 'ic_mul', 'ic_spp', 'lo_sal', 'my_arc', 'pa_pse', 'pa_ther', 'pch_sal', 'prv-3', 'sch', 'te_bry', 'te_dic', 'te_mar')
  )

p0

ggsave("./data/modified_data/PaceWP1_individuals_LOD75.tiff", p0, units="cm", width=80, height=25, dpi=200, compression = 'lzw')

str(trout_pathogens)

head(trout_75)
trout_75 <- subset(trout_75, select=-c(tot_rib))
saveRDS(trout_75, "./data/modified_data/PACE_WP2_WP3_trout_pathogens_LOD75.RDS")
write.csv(trout_75, "./data/modified_data/PACE_WP2_WP3_trout_pathogens_LOD75.csv", row.names = FALSE)
















#Test with 95% LOD
str(trout_pathogens)
trout_95 <- trout_pathogens[trout_pathogens$passed_95=='yes', ]
max_path <- trout_95%>%
  group_by(assay) %>%
  summarize(max_path = max(log_value))
summary(max_path)
trout_95 <- merge(trout_95, max_path, by = "assay")
trout_95$rib <- trout_95$log_value/trout_95$max_path
summary(trout_95$rib)
trout_95$rib[is.nan(trout_95$rib)] <- 0

tot_rib <- trout_95 %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
trout_95 <- merge(trout_95, tot_rib, by="vial")
levels = unique(trout_95$vial[order(trout_95$tot_rib)])

trout_95$vial <- factor(trout_95$vial,                                    # Change ordering manually
                        levels = levels)
head(trout_95)
str(trout_95)
ggplot(trout_95, aes(y=value, x=assay, col=vial)) + geom_point()
summary(trout_95$rib)

# Grouped
p0 <- ggplot(trout_95, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free", ncol=4)

p0

ggsave("./data/modified_data/Pace_all_trout_log_transformed_LOD95.tiff", p0, units="cm", width=35, height=30, dpi=600, compression = 'lzw')

str(trout_pathogens)

head(trout_95)
summary(as.factor(meta_trout$Transmitter))
trout_95 <- subset(trout_95, select=-c(tot_rib))
saveRDS(trout_95, "./data/modified_data/PACE_WP2_WP3_trout_pathogens_LOD95.RDS")


#################################################################################
#################################################################################
# 
# 
# 
# accel_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13A-1x-BL',]
# summary(as.factor(accel_trout$System))
# 
# accel_pathogens <- merge(trout_pathogens, accel_trout[c("vial")], by = "vial")
# str(accel_pathogens)
# accel_pathogens <- subset(accel_pathogens, select=-c(max_path, rib, tot_rib))
# 
# 
# max_path <- accel_pathogens%>%
#   group_by(assay) %>%
#   summarize(max_path = max(log_value))
# accel_pathogens <- merge(accel_pathogens, max_path, by = "assay")
# accel_pathogens$rib <- accel_pathogens$log_value/accel_pathogens$max_path
# summary(accel_pathogens$rib)
# 
# 
# tot_rib <- accel_pathogens %>%
#   group_by(vial)%>%
#   summarize(tot_rib = sum(rib))
# accel_pathogens <- merge(accel_pathogens, tot_rib, by="vial")
# levels = unique(accel_pathogens$vial[order(accel_pathogens$tot_rib)])
# 
# accel_pathogens$vial <- factor(accel_pathogens$vial,                                    # Change ordering manually
#                   levels = levels)
# 
# str(accel_pathogens)
# 
# #remove ca_cl (sea lice ran in signleton)
# accel_pathogens <- accel_pathogens[accel_pathogens$assay!='ca_cl', ]
# 
# ggplot(accel_pathogens, aes(y=value, x=assay, col=vial)) + geom_point()
# 
# str(pathogen_list)
# accel_pathogen_list <- pathogen_list[pathogen_list$ran_in_2023=='yes'|pathogen_list$ran_in_2023=='yes_singleton', ]
# accel_pathogen_list$December.2021.Onward.Names
# accel_pathogen_list$agent_name
# 
# # Grouped
# p0 <- ggplot(accel_pathogens, aes(fill=assay, x=rib, y=vial)) + 
#   geom_bar(stat="identity")+
#   facet_wrap(~System, scales = "free", ncol=3)
# 
# p0
# 
# ggsave("./data/modified_data/Pace_accleleration_log_transformed.tiff", p0, units="cm", width=35, height=15, dpi=600, compression = 'lzw')
# 
# 
# summary(as.factor(meta_trout$Transmitter))
###########################################################
#Fish included in temperature studies
###########################################################
summary(as.factor(meta_trout$Transmitter))
temp_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13-T-1x-BLU-1'|meta_trout$Transmitter=='V9T-2x' |meta_trout$Transmitter=='LP13-T'|meta_trout$Transmitter=='LP13-TAD',]
summary(as.factor(temp_trout$System))

temp_pathogen_list <- pathogen_list[pathogen_list$ran_in_2021=='yes'|pathogen_list$ran_in_2023=='yes', ]
summary(as.factor(temp_pathogen_list$ran_in_2021))
summary(as.factor(temp_pathogen_list$ran_in_2023))

temp_pathogen_list$December.2021.Onward.Names
temp_pathogen_list$agent_name
str(temp_pathogen_list)


str(temp_pathogens)
str(temp_pathogen_list)
#max pathogen values
temp_pathogens <- merge(trout_95, temp_trout[c("vial")], by = "vial")
temp_pathogens <- merge(temp_pathogens, temp_pathogen_list[c("assay", "ran_in_2021", "ran_in_2023")], by= "assay")

str(temp_pathogens)
detected_2020 <- unique(temp_pathogens$assay[temp_pathogens$Year<=2020])
detected_2021_2022 <- unique(temp_pathogens$assay[temp_pathogens$Year>=2021])

temp_pathogens <- temp_pathogens[temp_pathogens$ran_in_2021=='yes' & temp_pathogens$ran_in_2023=='yes', ]

#Find individual detectoin counts for shared pathogens
pathogen_count <- temp_pathogens %>%
  group_by(vial, System, Year)%>%
  summarize(n = n())

#Find detectoin counts for shared pathogens
pathogen_count <- temp_pathogens %>%
  group_by(assay)%>%
  summarize(n = n())
pathogen_count <- merge(pathogen_count, temp_pathogen_list[c("assay", "agent_name")], by="assay")

tot_rib <- temp_pathogens %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
temp_pathogens <- merge(temp_pathogens, tot_rib, by="vial")
levels = unique(temp_pathogens$vial[order(temp_pathogens$tot_rib)])

temp_pathogens$vial <- factor(temp_pathogens$vial,                                    # Change ordering manually
                              levels = levels)


str(temp_pathogens)
ggplot(temp_pathogens, aes(y=value, x=assay, col=vial)) + geom_point()


# Grouped
p1 <-ggplot(temp_pathogens, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free", ncol=4)

p1
str(temp_pathogens)





ggsave("./data/modified_data/Pace_temperature_RIB_LOD75.tiff", p1, units="cm", width=35, height=15, dpi=600, compression = 'lzw')


#add pathogen count
pathogen_count <- temp_pathogens %>%
  group_by(vial)%>%
  summarize(pathogen_count = n())
temp_pathogens <- merge(temp_pathogens, pathogen_count, by="vial")


saveRDS(temp_pathogens, "./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
saveRDS(pathogen_list, "./data/modified_data/pathogen_list.RDS")
write.csv(pathogen_list, "./data/modified_data/pathogen_list.csv", row.names = FALSE)
pathogen_list$December.2021.Onward.Names

