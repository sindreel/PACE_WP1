#Create figures over samples collected
###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

library(ggplot2)
library(gridExtra)

meta <- read.csv("./data/modified_data/meta.csv", sep = ",")
mhc_rivers <- read.csv("./data/raw_data/MHC_rivers_from_sten.csv", sep = ";")


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
setwd("./data/shapefiles")
getwd()
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

# Overview map
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
?scalebar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")





#Make NALO 2020-2021-plot
meta <- meta_0
str(meta)
library(dplyr)
tmp <- meta %>%
  group_by(Stasjon) %>%
  dplyr::summarise (Lengdegrad, Breddegrad, sample_size = n())
tmp <- tmp[!duplicated(tmp$Stasjon), ]
tmp$label <- paste(tmp$Stasjon, ", n = ", tmp$sample_size, sep = "")
tmp$label[tmp$Stasjon=='Nordfjord'] <- ''
tmp$label[tmp$Stasjon=='Måløy'] <- 'Måløy and Nordfjord, n = 53'
tmp$label[tmp$Stasjon=='Oksfjordhamn'] <- ''
tmp$label[tmp$Stasjon=='Oksfjord'] <- 'Oksfjord, n = 22'

# Create points of interest
#place <- c("Midelt", "Khenifra", "Fes", "RABAT", "Beni-Mellal", "Tangier")
#p.lon <- c(-4.73, -5.66,  -5.00, -6.83, -6.37, -5.82)
#p.lat <- c(32.68, 32.93, 34.03, 33.96, 32.34, 35.756)

# Crop DEM
norway <- crop(norway, extent(0, 57.5, 36, 71.5))

plot2021 <- plot(norway, border = "lightgrey", xlab = "Longetude", ylab = "Latitude", col.lab = "lightgrey", main = "Gill samples NALO 2020-2021")
# Bounding box
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
#rect(0, 57.5, 36, 71.5, border="red", lwd=2)#Denne brukes til å se området du har tenkt å croppe

# Add points to map
#?points
points(tmp$Lengdegrad, tmp$Breddegrad, pch=20, col="green", bg="green", cex=1)
tmp2 <- tmp[tmp$Stasjon=='Sandnesfjord', ]
points(tmp2$Lengdegrad, tmp2$Breddegrad, pch=20, col="red", bg="red", cex=1)

# Add place names to map
text(tmp$Lengdegrad, tmp$Breddegrad, labels=tmp$label, cex=1.2, pos=3)

# axis
axis(1, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(4,13,22,31), col.axis = "lightgrey")
axis(2, tcl=0.5, cex.axis=1, col = "lightgrey", at = c(58,62,66,70), col.axis = "lightgrey")

# Scale bar
scalebar(400, below="km",type="bar", divs=4, xy=c(22.2,58.5), lonlat=TRUE, adj=c(0, -2), cex=1, col = "lightgrey")



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
