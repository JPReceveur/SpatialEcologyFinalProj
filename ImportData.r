###################
#Data Import
##################

library(maptools)
library(rgdal)
library(dismo)
library(ggplot2)
library(GISTools)
library(ggpubr) #For arranging ggplots
library(plyr)
library(dplyr)

##############
#Create file paths and download basemap
############
output_path<-("output")
# if this folder doesn't exist, create it
if(!dir.exists(output_path)){
  dir.create(output_path)
}


# Create the folders (directories) "data" and "Final project" - If they exist already, this 
# command won't over-write them.
data_path<-(file.path("data","FinalProject"))
if(!dir.exists(data_path)){
  dir.create(data_path,recursive = TRUE)
}

#Download shapefile outline of Ghana
if(! file.exists(file.path(data_path,'countries.zip'))){
  download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", dest=file.path(data_path,"countries.zip"), mode="wb") 
}
#unzip (file.path(data_path,"countries.zip"), exdir = data_path)
# Read in the shapefile with rgdal package
world <- readOGR(file.path(data_path,"ne_10m_admin_0_countries.shp"))
GhanaOutline<-(world[world$ADMIN=="Ghana",1]) #1=overall country outline




GhanaMeterGrid<-CRS("+proj=tmerc +lat_0=4.666666666666667 +lon_0=-1 +k=0.99975 +x_0=274319.51 +y_0=0 +ellps=clrk80 +towgs84=-130,29,364,0,0,0,0 +units=m +no_defs") #GhanaMeter Grid EPSG:25000
GhanaOutline



###########3
#Import Point Data (Benbow et.al 2014)
###########
#Data available at https://github.com/JPReceveur/SpatialEcologyFinalProj
MUSurvey<-read.csv("GhanaMUSurvey.csv",header=T)
MUGeo <- subset(MUSurvey, !is.na(lon) & !is.na(lat)) #Remove NAs


#plot(GhanaOutline)
# restore the box around the map
#box()
# add the points
#points(MUGeo$lon, MUGeo$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
#points(MUGeo$lon, MUGeo$lat, col='red', cex=0.75)
#WSGeo

wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
MUGeo.proj <- SpatialPoints(coords=MUGeo[,c("lon","lat")], proj4string=wgs1984.proj)

plot(GhanaOutline)
points(MUGeo.proj,col="blue")


MUGeo.Meter<-spTransform(MUGeo.proj, GhanaMeterGrid) #project to Ghana meter grid
Presence<-subset(MUGeo,Total.MU.Present..VNTR.==1)#Total MU Presence coded as 0 for absence and 1 for presence
#Presence$Total.MU.Present..VNTR.
Absence<-subset(MUGeo,Total.MU.Present..VNTR.==0)
names(MUGeo)
PresenceMPM<-subset(MUGeo,Total.ER.Present==1)#Total MPM Presence coded as 0 for absence and 1 for presence
#Presence$Total.MU.Present..VNTR.
AbsenceMPM<-subset(MUGeo,Total.ER.Present==0)


#Project the subsets into WGS84 and Ghana Meter Grid

wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Presence.proj <- SpatialPoints(coords=Presence[,c("lon","lat")], proj4string=wgs1984.proj)
Absence.proj <- SpatialPoints(coords=Absence[,c("lon","lat")], proj4string=wgs1984.proj)
PresenceMPM.proj <- SpatialPoints(coords=PresenceMPM[,c("lon","lat")], proj4string=wgs1984.proj)
AbsenceMPM.proj <- SpatialPoints(coords=AbsenceMPM[,c("lon","lat")], proj4string=wgs1984.proj)


Presence.Meter<-spTransform(Presence.proj, GhanaMeterGrid)
Absence.Meter<-spTransform(Absence.proj, GhanaMeterGrid)

PresenceMPM.Meter<-spTransform(PresenceMPM.proj, GhanaMeterGrid)
AbsenceMPM.Meter<-spTransform(AbsenceMPM.proj, GhanaMeterGrid)


#MUGeo.Meter
GhanaOutline.Meter<-spTransform(GhanaOutline,GhanaMeterGrid)

# par(mfrow=c(1,2), mai=c(0.1,0.1,0.5,0.1))
# plot(GhanaOutline,main="WGS84")
# box()
# points(MUGeo.proj,col="blue")
# 
# 
# 
# plot(GhanaOutline.Meter,main="GhanaMeterGrid")
# box()
# points(MUGeo.Meter,col="blue")



#############
#Import Land cover and clip to Ghana (Data from Tapppen et al. 2016)
############

#Download land cover data from https://www.sciencebase.gov/catalog/item/5deffc05e4b02caea0f4f3fc

if(! file.exists(file.path(data_path,'LULC.zip'))){
  download.file("http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/wafrica/downloads/data/west_africa_land-use_land-cover_2013_2km.zip", dest=file.path(data_path,"LULC.zip"), mode="wb") 
}
#unzip (file.path(data_path,"LULC.zip"), exdir = data_path)

LandCover2k <- stack(file.path(data_path,"west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif"))
#plot(LandCover2k)
LandCover2k.Meter<-projectRaster(LandCover2k,crs=GhanaMeterGrid)
#summary(GhanaOutline.Meter)
par(mfrow=c(1,1))
r<- raster(GhanaOutline.Meter)
res(r)<-2000   #Not really sure what number to put here (units of outline shapefile are m, so just picked the resoultion for the LandCover2k.Meter file)
r[] <- rnorm(ncell(r))
#plot(r)
#plot(GhanaOutline.Meter,add=T)
r[] <- 0

GhanaRaster<-rasterize(GhanaOutline.Meter,r,getCover=T)
GhanaRaster[GhanaRaster>0]<-1
GhanaRaster[GhanaRaster==0]<-NA
#plot(GhanaRaster)
#summary(LandCover2k)

#res(LandCover2k.Meter)
LULCRaster<-LandCover2k.Meter*GhanaRaster #Subset land use by the Ghana outline (all other raster points =0)

plot(LULCRaster)
points(MUGeo.Meter,col="blue")





#############
#Import Climate Data
############
#Climate Data downloaded 
#if(! file.exists(file.path(data_path,'ClimateData.zip'))){
# download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip", dest=file.path(data_path,"ClimateData.zip"), mode="wb") 
#}
#unzip (file.path(data_path,"ClimateData.zip"), exdir = data_path)
#Folder Climate Data subset contains
#wc2.1_30s_bio_1.tif Annual Mean Temperature
#wc2.1_30s_bio_12.tif Annual Precipitation
#wc2.1_30s_bio_15.tif Precipitation seasonality
#wc2.1_30s_bio_7.tif Temperature Annual range
#wc2.1_30s_bio_14.tif Precipitation of driest month

ClimateTif<-list.files(file.path(data_path,"ClimateDataSubset"), pattern='tif$', full.names=TRUE )
ClimateSubset <- stack(ClimateTif)


ma.area <- extent(GhanaOutline)

ClimateSubsetGhana <- crop(ClimateSubset, ma.area)
GhanaClimateSubset.Meter<-projectRaster(ClimateSubsetGhana,crs=GhanaMeterGrid)
