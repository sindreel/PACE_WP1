####################################
#Selecting samples for WP1

#Create figures over samples collected
###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

library(ggplot2)
library(gridExtra)
library(data.table)
meta <- read.csv("./data/raw_data/meta_complete.csv", sep = ";")
str(meta)

mhc_rivers <- read.csv("./data/raw_data/MHC_locations_from_sten.csv", sep = ";")
str(mhc_rivers)
mhc_rivers <- mhc_rivers[c(1,5,6)]
new_row    <- data.table("Elv" = "Agde_21", "longitude" = 9.64085940, "latitude" = 63.62240561)                         
new_row1    <- data.table("Elv" = "Moen_21", "longitude" = 11.43807395, "latitude" = 64.69273930)
new_row2    <- data.table("Elv" = "Oksf_14_15", "longitude" = 21.32555246, "latitude" = 69.90711389) 
mhc_rivers <- rbind(mhc_rivers, new_row, new_row1, new_row2)


#######################################################################
#Make study area map
#This is based on tutorial from: https://www.benjaminbell.co.uk/2019/08/creating-simple-location-maps-in-r.html
#######################################################################

library(raster)
# Set working folder (to store data files)
# Download map data - Country shapefile
norway <- getData("GADM", country="NO", level=0)



#Make NALO 2020-2022-plot
str(meta)
library(dplyr)
summary(as.factor(meta$Stasjon))
meta$Stasjon[meta$Stasjon=='Aalvik'] <- "Alvik"

meta$Stasjon[meta$Stasjon=='Ytre Årdalsfjord'] <- "Ytre_Årdalsfjord"
meta$Stasjon[meta$Stasjon=='Indre Etne'] <- "Etne"

meta_0 <- meta



tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")


tmp$label[tmp$Stasjon=='Nordfjord'] <- ''
tmp$label[tmp$Stasjon=='Måløy'] <- 'Måløy and Nordfjord, n = 79'
tmp$label[tmp$Stasjon=='Oksfjordhamn'] <- ''
tmp$label[tmp$Stasjon=='Straumfjord'] <- ''
tmp$label[tmp$Stasjon=='Oksfjord'] <- 'Oksfjord og Straumfjord, n = 24'
tmp$label[tmp$Stasjon=='Sandnesfjord'] <- 'Sandnesfjord, n = 18'
sindre_locations <- tmp[c(1,2,3)]
names(sindre_locations) <- c("station_name", "lat", "long")
write.csv(sindre_locations, ("./data/modified_data/sindre_locations.csv"), row.names = FALSE)

# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 37, 71.5))


tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- tmp$Stasjon



plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2020-2022")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="green", cex=1)
tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="green", bg="green", cex=1)

points(mhc_rivers$longitude, mhc_rivers$latitude, pch=20, col="red", bg="red", cex=1)
text(mhc_rivers$longitude, mhc_rivers$latitude, labels=mhc_rivers$Elv, cex=0.75, pos=2)


# Add place names to map
text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=0.75, pos=3)

# axis
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")

ggsave("./data/modified_data/all_samples_2020-2022.tiff", plot2021, units="cm", width=30, height=30, dpi=600, compression = 'lzw')


#################################################################################################################################################################
#Fish over 20 cm
meta <- meta_0
meta <- meta[meta$Lengde_tot>199, ]


tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")



tmp$label[tmp$Stasjon=='Nordfjord'] <- ''
tmp$label[tmp$Stasjon=='Måløy'] <- 'Måløy and Nordfjord, n = 77'
tmp$label[tmp$Stasjon=='Oksfjordhamn'] <- ''
tmp$label[tmp$Stasjon=='Straumfjord'] <- ''
tmp$label[tmp$Stasjon=='Oksfjord'] <- 'Oksfjord og Straumfjord, n = 24'
tmp$label[tmp$Stasjon=='Sandnesfjord'] <- 'Sandnesfjord, n = 28'


# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
#norway <- crop(norway, extent(0, 57.5, 37, 71.5))




plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2020-2022")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="green", cex=1)
tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="green", bg="green", cex=1)

points(mhc_rivers$longitude, mhc_rivers$latitude, pch=20, col="red", bg="red", cex=1)


# Add place names to map
text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=0.75, pos=2)

# axis
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")

ggsave("./data/modified_data/samples_over_200mmTL_2020-2022.tiff", plot2021, units="cm", width=30, height=30, dpi=600, compression = 'lzw')


