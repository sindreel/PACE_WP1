#Create figures over samples collected
###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

library(ggplot2)
library(gridExtra)

meta <- read.csv("./data/modified_data/meta_complete_141122.csv", sep = ";")
mhc_rivers <- read.csv("./data/modified_data/MCH_locations_from_sten.csv", sep = ";")


#First, let's load the first shapefile of Norway into R
#shape_zip_link <- "https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/norway-shapefile/at_download/file"
#dir.create("data/shapefiles", showWarnings = FALSE, recursive = TRUE)
#download.file(url=shape_zip_link,destfile="./data/shapefiles/norway.zip", mode = "wb")  
#Unzip downloaded file
library(utils)
#unzip("./data/shapefiles/norway.zip", files = NULL, list = FALSE, overwrite = FALSE,
#      junkpaths = FALSE, exdir = "./data/shapefiles", unzip = "internal",
#      setTimes = FALSE)

#norway <- readOGR(dsn="./data/shapefiles/no_100km.shp")

#str(norway)

#norway <- readOGR(dsn="./data/shapefiles" ,
#                  layer="NO_Arealdekke_pol")


#######################################################################
#Make study area map
#This is based on tutorial from: https://www.benjaminbell.co.uk/2019/08/creating-simple-location-maps-in-r.html
#######################################################################

library(raster)
# Set working folder (to store data files)
#setwd("./data/shapefiles")
#getwd()
# Download map data - Country shapefile
norway <- getData("GADM", country="NO", level=0)

meta_0 <- meta


#Make NALO 2021-plot
meta <- meta_0
meta <- meta[meta$Dato>'2021-01-01', ]
str(meta)
library(dplyr)
tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")
# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 36, 71.5))

plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2021")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="red", cex=1)
tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="red", bg="red", cex=1)

# Add place names to map
text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=1.2, pos=3)

# Overview map
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
?scalebar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")
# North Arrow
#arrows(x0=-5.4, y0=33.55, x1=-5.4, y1=33.675, length=0.3, lwd=5)
# Add map label
#legend("topleft", legend="B", cex=2, bty="n")





#Make NALO 2020 plot
meta <- meta_0
meta <- meta[meta$Dato<'2021-01-01', ]
str(meta)
library(dplyr)
tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")
# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 36, 71.5))

plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2020")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="red", cex=1)

# Add place names to map
text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=1.2, pos=3)


#Make NALO 2021 location plot
meta <- meta_0
str(meta)
library(dplyr)
tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
#tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")
tmp$label <- paste(tmp$Stasjon)

# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 36, 71.5))

plot_locations <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill sampling locations NALO 2020-2021")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="red", bg="red", cex=1)

# Add place names to map
#text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=1.2, pos=3)



# Overview map
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
?scalebar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")





#Make NALO 2020-2022-plot
meta <- meta_0
str(meta)
library(dplyr)
summary(as.factor(meta$Stasjon))
meta$Stasjon[meta$Stasjon=='Ytre Årdalsfjord'] <- "Ytre_Årdalsfjord"
meta$Stasjon[meta$Stasjon=='Indre Etne'] <- "Etne"
str(meta)
tmp <- meta[meta$Lengde_tot>199,] %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n(), min_size = min(Lengde_tot))
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp <- tmp[tmp$sample_size>19,]
tmp <- tmp[!tmp$Stasjon=='Sandnesfjord',]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")
#tmp$label[tmp$Stasjon=='Nordfjord'] <- ''
#tmp$label[tmp$Stasjon=='Måløy'] <- 'Måløy and Nordfjord, n = 79'
#tmp$label[tmp$Stasjon=='Oksfjordhamn'] <- ''
#tmp$label[tmp$Stasjon=='Straumfjord'] <- ''
#tmp$label[tmp$Stasjon=='Oksfjord'] <- 'Oksfjord og Straumfjord, n = 24'
#tmp$label[tmp$Stasjon=='Sandnesfjord'] <- 'Sandnesfjord, n = 18'


# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 37, 71.5))

plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2020-2022")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
plot2021 <-plot2021+points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="green", cex=1)
#tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
#plot2021 <-plot2021+points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="green", bg="green", cex=1)


plot2021 <-plot2021+points(mhc_rivers$longitude, mhc_rivers$latitude, pch=20, col="red", bg="red", cex=1)

str(tmp)
# Add place names to map
plot2021 <-plot2021+text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=0.75, pos=2)

# axis
plot2021 <-plot2021+axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
plot2021 <-plot2021+axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
plot2021 <-plot2021+scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")

ggsave(plot2021, file="./data/modified_data/nalo_locations_to_kristi_141122.tiff", units="cm", width=35, height=50, dpi=600, compression = 'lzw', limitsize = FALSE)


#MHC sampling rivers suggested by sten

plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "MHC sampling rivers PACE WP1")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="red", bg="red", cex=1)
tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="red", bg="red", cex=1)

# Add place names to map
#text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=1.2, pos=3)


#Add rivers from Sten
str(mhc_rivers)
points(mhc_rivers$long, mhc_rivers$lat, pch=20, col="blue", bg="blue", cex=1)
text(mhc_rivers$long, mhc_rivers$lat, labels=mhc_rivers$Vdr.nr, cex=0.8, pos=2)


# Overview map
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")
# North Arrow
#arrows(x0=-5.4, y0=33.55, x1=-5.4, y1=33.675, length=0.3, lwd=5)
# Add map label
#legend("topleft", legend="B", cex=2, bty="n")


# Bounding box
rect(-5.5, 32.75, -4.25, 33.75, border="red", lwd=2)
# Points of interest
points(p.lon, p.lat, pch=22, col="black", bg="blue", cex=3)
text(p.lon, p.lat, labels=place, cex=1.2, pos=3)
# Scale bar
scalebar(200, below="km",type="bar", divs=4, xy=c(-2.5,30.5), lonlat=TRUE, cex=1)
# North Arrow
arrows(x0=-7.5, y0=36, x1=-7.5, y1=37, length=0.4, lwd=5)
# Add map label
legend("topleft", legend="A", cex=2, bty="n")

