library(landscapemetrics)
library(landscapetools)


#MUSurvey<-read.table("PointsOnly.txt",header=T)
MUSurvey<-read.csv("GhanaMUSurvey.csv",header=T)

MUGeo <- subset(MUSurvey, !is.na(lon) & !is.na(lat))


#Subset<-MUGeo[1:50,]

wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
MUGeo.proj <- SpatialPoints(coords=MUGeo[,c("lon","lat")], proj4string=wgs1984.proj)

LandCover<-LandCover2k
MUGeo.proj<-spTransform(MUGeo.proj,crs(LandCover))
GhanaOutline2<-spTransform(GhanaOutline,crs(LandCover))

#check_landscape(LandCoverCropped)



Buffer<-sample_lsm(
  LandCover,
  MUGeo.proj,
  plot_id = NULL,
  shape = "square",
  size=5000,
  all_classes = FALSE,
  return_raster = FALSE,
  verbose = TRUE,
  progress = FALSE,level="landscape",type="aggregation metric")

Buffer
unique(Buffer$level)

hist(Buffer$value)
head(Buffer)
Contag<-subset(Buffer,metric=="contag")
Contag

Contag$plot_id<-MUGeo$USITEID
Contag$PresAbs<-MUGeo$Total.MU.Present..VNTR.

Contag$PresAbs[Contag$PresAbs==1]<-"Present"
Contag$PresAbs[Contag$PresAbs==0]<-"Absent"

theme_set(theme_bw(base_size = 14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

ggplot(Contag,aes( x=PresAbs,y=value))+geom_point()

kruskal.test(data=Contag, PresAbs~value)
MU