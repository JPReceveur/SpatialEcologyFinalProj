#############
#Overview plots and Land Cover 
#############

################3
#Correlograms MU/MPM presence absence by distance (Figure 2b)
################

library(ncf)
#Not used Moran I assumption data is normal
# 
# qqnorm(MUGeo$Total.ER.Present)
# qqline(MUGeo$Total.ER.Present,col="blue")
# 
# 

cor.MUPos <- correlog(MUGeo.proj$lat, MUGeo.proj$lon, MUGeo$Total.MU.Present..VNTR., increment = 10000, resamp = 100)
plot(cor.MUPos,xlab= "Distance (m, mean of class)",ylab="Correlation (MU positivity)",main="")
abline(h = 0, col = "purple")

# 
# dev.off()
# tiff("output/CorrelogramMUPresence.tiff", width = 84, height = 84, units = 'mm', res = 1200)
# plot(cor.MUPos,xlab= "Distance (m, mean of class)",ylab="Correlation (MU positivity)",main="")
# abline(h = 0, col = "purple")
# 
# dev.off()


cor.MPMPos <- correlog(MUGeo.proj$lat, MUGeo.proj$lon, MUGeo$Total.ER.Present, increment = 10000, resamp = 100)

plot(cor.MPMPos,xlab= "Distance (m, mean of class)",ylab="Correlation (MPM positivity)",main="")
abline(h = 0, col = "purple")
# 
# dev.off()
# tiff("output/CorrelogramMPMPresence.tiff", width = 84, height = 84, units = 'mm', res = 1200)
# plot(cor.MPMPos,xlab= "Distance (m, mean of class)",ylab="Correlation (MPM positivity)",main="")
# abline(h = 0, col = "purple")
# 
# dev.off()



#############
#Land use graph MU
############

#pal_nlcd Function modified from https://space-lab-msu.github.io/MSUGradSpatialEcology/lab3_patch_design.html
#Table of values avialable at https://github.com/JPReceveur/SpatialEcologyFinalProj

pal_nlcd <- function() {
  data.frame(
    class = c("forest", "forest",
              "forest", "shrubland",
              "anthropogenic", "anthropogenic",
              "wetland","anthropogenic",
              "forest", "No Data",
              "Sand", "shrubland",
              "herbaceous", "herbaceous",
              "No Data", "anthropogenic",
              "shrubland", "forest",
              "anthropogenic", "anthropogenic",
              "No Data","shrubland",
              "shrubland","herbaceous",
              "Oasis","shrubland",
              "shrubland","water",
              "forest","anthropogenic",
              "No Data"),
    code = as.character(c(7, 15,
                          28, 2, 8, 78,
                          3,
                          24, 1, 99,
                          10,12, 31,
                          4, 0, 27, 32,
                          25, 13,
                          6, 98,11,29,23,5,16,22,9,21,14,255)),
    description = c("Mangrove", "Gallery/ Riparian Forest",
                    "Swamp Forest", "Savanna", 
                    "Agriculture", "Open Mine",
                    "Wetland","Agriculture/ flood recessional",
                    "Forest", "Cloud",
                    "Sandy","Bare soil", 
                    "Herbaceous savanna", "Steppe",
                    "No data","Cropland/ oil palms", 
                    "Shrubland",  "Woodland", 
                    "Settlements","Plantation",
                    "Cloud shadow", "Rocky land",
                    "Sahelian short grass savanna", "Thicket",
                    "Oasis","Shrub and savanna",
                    "Bowe", "Water bodies",
                    "Degraded forest","Irrigated agriculture",
                    "No Data"),
    color = c("#33cccc", "#aa5ce8",
              "#beffa6", "#8cb08c", 
              "#ffff96", "#505050",
              "#000081","#ebc961", 
              "#8400a8", "#ffffff",
              "#ff99cc", "#a87000",
              "#0a9696", "#ffd09b",
              "#ffffff", "#77AD93",
              "#749373", "#28734b",
              "#ff0000", "#808000",
              "#ffffff", "#969696",
              "#a7c38d", "#f8a37b",
              "#99C147", "#77AD93",
              "#DCD939", "#3366ff",
              "#d296e6", "#cdff66",
              "##ffffff"
    ),
    stringsAsFactors = FALSE)
}


NLCD <- as.matrix(table(raster::values(LULCRaster)))
cols <- dplyr::filter(pal_nlcd(), code %in% row.names(NLCD))
par(xpd = FALSE,mai = c(0.5, 0.5, 0.2, 0)) 

plot(LULCRaster, legend=FALSE,axes=TRUE,box=FALSE,col=cols$color,
     xlab="", ylab="") 
par(xpd = TRUE) # reset to limit plotting to figure region
plot(GhanaOutline.Meter,add=T)
legend(x = -2000, y = 690000, legend = cols$description, fill = cols$color, 
       ncol = 2,  cex = 0.85, inset = 0.9) 
legend(x=500183,y=615115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
plot(Presence.Meter,col="red",bg="red",add=T,pch=24)
plot(Absence.Meter,col="blue",bg="blue",add=T,pch=21)

# 
# dev.off()
# tiff("output/LUGhana.tiff", width = 174, height = 174, units = 'mm', res = 600)
# par(xpd = FALSE,mai = c(0.5, 0.5, 0.2, 0))
# 
# plot(LULCRaster, legend=FALSE,axes=TRUE,box=FALSE,col=cols$color,
#      xlab="", ylab="")
# par(xpd = TRUE) # reset to limit plotting to figure region
# plot(GhanaOutline.Meter,add=T)
# legend(x = -2000, y = 690000, legend = cols$description, fill = cols$color,
#        ncol = 2,  cex = 0.85, inset = 0.9)
# legend(x=450183,y=645115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
# scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
# north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
# plot(Presence.Meter,col="red",bg="red",add=T,pch=24)
# plot(Absence.Meter,col="blue",bg="blue",add=T,pch=21)
# 
# dev.off()

##############
#LandUseGraph MPM
###############

#Plotted as above but showing MPM pres/abs
par(xpd = FALSE,mai = c(0.5, 0.5, 0.2, 0))

plot(LULCRaster, legend=FALSE,axes=TRUE,box=FALSE,col=cols$color,
     xlab="", ylab="")
par(xpd = TRUE) # reset to limit plotting to figure region
plot(GhanaOutline.Meter,add=T)
legend(x = -2000, y = 690000, legend = cols$description, fill = cols$color,
       ncol = 2,  cex = 0.85, inset = 0.9)
legend(x=450183,y=645115,legend =c("MPM +","MPM -"),pch=c(17,16),col=c('red4','blue'))
scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
plot(PresenceMPM.Meter,col="red4",bg="red4",add=T,pch=24)
plot(AbsenceMPM.Meter,col="blue",bg="blue",add=T,pch=21)



############
#Landscape Metrics
#############
#Landscape Metrics

##Calculate 5k landscape metrics


#Load landscape pachages
library(landscapemetrics)
library(landscapetools)
#Data same as above
MUSurvey<-read.csv("GhanaMUSurvey.csv",header=T)

MUGeo <- subset(MUSurvey, !is.na(lon) & !is.na(lat))


wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
MUGeo.proj <- SpatialPoints(coords=MUGeo[,c("lon","lat")], proj4string=wgs1984.proj)

LandCover<-LandCover2k
MUGeo.proj<-spTransform(MUGeo.proj,crs(LandCover))
GhanaOutline2<-spTransform(GhanaOutline,crs(LandCover))



#Sample 5k buffer (5000 m) around each sample point using landscape metrics
Buffer5k<-sample_lsm(
  LandCover,
  MUGeo.proj,
  plot_id = NULL,
  shape = "square",
  size=5000,
  all_classes = FALSE,
  return_raster = FALSE,
  verbose = TRUE,
  progress = FALSE,level="landscape",type="aggregation metric")

Buffer5k
#unique(Buffer$metric)



##Landscape Metric MU 5k


#For loop testing differences between PresAbs values for the chosen metrics (MetricList)
MetricList<-c("contag","np","lsi","cohesion")

for(i in 1:length(MetricList)){
  Subset<-subset(Buffer5k,Buffer5k$metric==MetricList[i])
  print(unique(Subset$metric))
  
  
  
  Subset$plot_id<-MUGeo$USITEID
  Subset$PresAbs<-MUGeo$Total.MU.Present..VNTR.
  Subset$PresAbs[Subset$PresAbs==1]<-"Present"
  Subset$PresAbs[Subset$PresAbs==0]<-"Absent"
  Plot <- ggplot(Subset,aes( x=PresAbs,y=value,col=PresAbs))+ylab(MetricList[i])+xlab("MU VNTR Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")
  print(Plot)
  print(kruskal.test(data=Subset,value~PresAbs))
  Trtdata <- ddply(Subset, c("metric","PresAbs"), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd / sqrt(N)
  )
  print(Trtdata)
  
}

#LSI Landscape shape index #standardized measure of total edge LSI=1 =single patch increases from there pg 158 Fragstats

#NP <- number of patches pg 149
#contagion -sum of proportional abundance of each patch, inverse to edge density pg154




##Landscape Metric MPM 5k



MetricList<-c("contag","np","lsi","cohesion")
#Same as above but using Total.ER.Present to calculate for MPM abundance
for(i in 1:length(MetricList)){
  Subset<-subset(Buffer5k,Buffer5k$metric==MetricList[i])
  print(unique(Subset$metric))
  
  
  
  Subset$plot_id<-MUGeo$USITEID
  Subset$PresAbs<-MUGeo$Total.ER.Present #Total ER presence coded as 1= presence, 0 =absence
  Subset$PresAbs[Subset$PresAbs==1]<-"Present" 
  Subset$PresAbs[Subset$PresAbs==0]<-"Absent"
  Plot <- ggplot(Subset,aes( x=PresAbs,y=value,col=PresAbs))+ylab(MetricList[i])+xlab("MPM VNTR Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red4"))+theme(legend.position="none")
  print(Plot)
  print(kruskal.test(data=Subset,PresAbs~value))
  
}





#10k landscape metrics MU


#Data same as above
MUSurvey<-read.csv("GhanaMUSurvey.csv",header=T)

MUGeo <- subset(MUSurvey, !is.na(lon) & !is.na(lat))


#Project to WGS84
wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
MUGeo.proj <- SpatialPoints(coords=MUGeo[,c("lon","lat")], proj4string=wgs1984.proj)

LandCover<-LandCover2k
MUGeo.proj<-spTransform(MUGeo.proj,crs(LandCover))
GhanaOutline2<-spTransform(GhanaOutline,crs(LandCover))



#Calculate 10 k buffer (10000 m) around sample points for landscape metrics
Buffer10k<-sample_lsm(
  LandCover,
  MUGeo.proj,
  plot_id = NULL,
  shape = "square",
  size=10000,
  all_classes = FALSE,
  return_raster = FALSE,
  verbose = TRUE,
  progress = FALSE,level="landscape",type="aggregation metric")

#Buffer10k
#unique(Buffer$metric)



##Landscape Metric MU 10 k



MetricList<-c("contag","np","lsi","cohesion")

for(i in 1:length(MetricList)){
  Subset<-subset(Buffer10k,Buffer10k$metric==MetricList[i])
  print(unique(Subset$metric))
  
  
  
  Subset$plot_id<-MUGeo$USITEID
  Subset$PresAbs<-MUGeo$Total.MU.Present..VNTR.
  Subset$PresAbs[Subset$PresAbs==1]<-"Present"
  Subset$PresAbs[Subset$PresAbs==0]<-"Absent"
  Plot <- ggplot(Subset,aes( x=PresAbs,y=value,col=PresAbs))+ylab(MetricList[i])+xlab("MU VNTR Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")
  print(Plot)
  print(kruskal.test(data=Subset,value~PresAbs))
  Trtdata <- ddply(Subset, c("metric","PresAbs"), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd / sqrt(N)
  )
  print(Trtdata)
  
}

#LSI Landscape shape index #standardized measure of total edge LSI=1 =single patch increases from there pg 158 Fragstats

#NP <- number of patches pg 149
#contagion -sum of proportional abundance of each patch, inverse to edge density pg154

#Subset for Contag only plot
Contag<-subset(Buffer10k,Buffer10k$metric=="contag")
Contag$plot_id<-MUGeo$USITEID
Contag$PresAbs<-MUGeo$Total.MU.Present..VNTR.
Contag$PresAbs[Contag$PresAbs==1]<-"Present"
Contag$PresAbs[Contag$PresAbs==0]<-"Absent"

Plot <- ggplot(Contag,aes( x=PresAbs,y=value,col=PresAbs,shape=PresAbs))+ylab(MetricList[i])+xlab("MU VNTR Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")+ylab("Contagion Index")+geom_point()
Plot


# 
# dev.off()
# tiff("output/ContagPlot.tiff", width = 84, height = 84, units = 'mm', res = 600)
# Plot
# dev.off()






#Landscape Metric MPM Presence Absence
##5 kilometer


#List of metrics to run kruskal.wallis tests on
MetricList<-c("contag","np","lsi","cohesion")

for(i in 1:length(MetricList)){
  Subset<-subset(Buffer5k,Buffer5k$metric==MetricList[i])
  print(unique(Subset$metric))
  
  
  
  Subset$plot_id<-MUGeo$USITEID #unique site ID from file
  Subset$PresAbs<-MUGeo$Total.ER.Present
  Subset$PresAbs[Subset$PresAbs==1]<-"Present"
  Subset$PresAbs[Subset$PresAbs==0]<-"Absent"
  Plot <- ggplot(Subset,aes( x=PresAbs,y=value,col=PresAbs))+ylab(MetricList[i])+xlab("MPM  Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red4"))+theme(legend.position="none")
  #print(Plot)
  print(kruskal.test(data=Subset,value~PresAbs))
  Trtdata <- ddply(Subset, c("metric","PresAbs"), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd / sqrt(N)
  )
  print(Trtdata)
  
}





##10k MPM landscape metrics



MetricList<-c("contag","np","lsi","cohesion")

for(i in 1:length(MetricList)){
  Subset<-subset(Buffer10k,Buffer5k$metric==MetricList[i])
  print(unique(Subset$metric))
  
  
  
  Subset$plot_id<-MUGeo$USITEID
  Subset$PresAbs<-MUGeo$Total.ER.Present
  Subset$PresAbs[Subset$PresAbs==1]<-"Present"
  Subset$PresAbs[Subset$PresAbs==0]<-"Absent"
  Plot <- ggplot(Subset,aes( x=PresAbs,y=value,col=PresAbs))+ylab(MetricList[i])+xlab("MPM  Profiling")+geom_boxplot()+scale_color_manual(values=c("blue","red4"))+theme(legend.position="none")
  #print(Plot)
  print(kruskal.test(data=Subset,value~PresAbs))
  Trtdata <- ddply(Subset, c("metric","PresAbs"), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd / sqrt(N)
  )
  print(Trtdata)
  
}







