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


#Addirional NALO data from missing individuals from Keka 10.01.2021:
URL_NALO_2021_keka <- "https://ntnu.box.com/shared/static/8ynblzadzj6rv23a6gfgdq13gy2ylnc6.csv"
download.file(url=URL_NALO_2021_keka,destfile="./data/raw_data/NALO_2021_keka.csv")  


