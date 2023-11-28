#Download data

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

#########################################################################
#download data files for PACE WP1
#########################################################################

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)


#NALO 2021 data from Rune 01.12.2021:
URL_NALO_2021 <- "https://ntnu.box.com/shared/static/xgy66pt4eux3jolqt3avmskaby7o3jrk.csv"
download.file(url=URL_NALO_2021,destfile="./data/raw_data/NALO_2021.csv")  

#All samples 2020-2022, uploaded 12.10.22:
URL_meta_complete <- "https://ntnu.box.com/shared/static/31ag1rxhnwusysqtxejpfzfthy2eez00.csv"
download.file(url=URL_meta_complete,destfile="./data/raw_data/meta_complete.csv")  


#MHC locations from Sten, uploaded 12.10.22:
URL_MHC_locations_steen <- "https://ntnu.box.com/shared/static/iw4vtets4rhd5uexinach6hpu8hbn2zs.csv"
download.file(url=URL_MHC_locations_steen,destfile="./data/raw_data/MHC_locations_from_sten.csv") 



#Additional NALO data from missing individuals from Keka 10.01.2021:
URL_NALO_2021_keka <- "https://ntnu.box.com/shared/static/8ynblzadzj6rv23a6gfgdq13gy2ylnc6.csv"
download.file(url=URL_NALO_2021_keka,destfile="./data/raw_data/NALO_2021_keka.csv")  


#NALO2020-data
URL_NALO_2020 <- "https://ntnu.box.com/shared/static/lv7vzc1823ngfagdg9gp75qbvwfw2spw.csv"
download.file(url=URL_NALO_2020,destfile="./data/raw_data/NALO_2020.csv")  


#NALO2022-data
URL_NALO_2022 <- "https://ntnu.box.com/shared/static/n1vbl9lh1uvl9xxcsokj85k8ypcw2ig6.csv"
download.file(url=URL_NALO_2022,destfile="./data/raw_data/NALO_2022.csv")  


#NALO2021-data alle data
URL_NALO_2021_all <- "https://ntnu.box.com/shared/static/w8ydu27xrkpv1a5nkf5jfpnzo5l5qovs.csv"
download.file(url=URL_NALO_2021_all,destfile="./data/raw_data/NALO_2021_all.csv")  


#NALO2020_2022-data alle data
URL_NALO_2020_2022 <- "https://ntnu.box.com/shared/static/4qnt14gn48er6tak1lz0ra2ge4hpi3fc.csv"
download.file(url=URL_NALO_2020_2022,destfile="./data/raw_data/NALO_2020_2022.csv")  



#all_analysed_wp1_samples
URL_WP1_samples <- "https://ntnu.box.com/shared/static/ud5a1hdeh03q84tdpkx8k4e2z81v6y70.csv"
download.file(url=URL_WP1_samples,destfile="./data/raw_data/all_analysed_WP1_samples.csv")  




#PACE_sampling_sheets_2020
URL_PACE_sampling_sheets_2020 <- "https://ntnu.box.com/shared/static/b7uonhrnjopblutuq6d5dewigv6un534.csv"
download.file(url=URL_PACE_sampling_sheets_2020,destfile="./data/raw_data/PACE_WP1_Sampling_sheets_2020.csv")  


#Pace WP1 individual assays
WP1_individual_assays <- "https://ntnu.box.com/shared/static/y53xx4smjnrswvk2nz1lt2jmeqbde5nq.csv"
download.file(url=WP1_individual_assays,destfile="./data/raw_data/WP1_individual_assays.csv")  



