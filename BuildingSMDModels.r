###############
#Building SDM models
###############

library(maptools)
library(rgdal)
library(dismo)
library(ggplot2)
library(GISTools)
library(ggpubr)
library(plyr)
library(dplyr)
library(randomForest)

#SDM Models


#Import survey data and seperate into pres absence data
MUSurvey<-read.csv("GhanaMUSurvey.csv",header=T)
MUGeo <- subset(MUSurvey, !is.na(lon) & !is.na(lat))
Presence<-subset(MUGeo,Total.MU.Present..VNTR.==1)
Presence$Total.MU.Present..VNTR.
Absence<-subset(MUGeo,Total.MU.Present..VNTR.==0)

wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Presence.proj <- SpatialPoints(coords=Presence[,c("lon","lat")], proj4string=wgs1984.proj)
Absence.proj <- SpatialPoints(coords=Absence[,c("lon","lat")], proj4string=wgs1984.proj)


Presence.Meter<-spTransform(Presence.proj, GhanaMeterGrid)
Absence.Meter<-spTransform(Absence.proj, GhanaMeterGrid)


#MUGeo.Meter


presvals <- extract(GhanaClimateSubset.Meter, Presence.Meter)
absvals <- extract(GhanaClimateSubset.Meter, Absence.Meter)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
#head(sdmdata)

#pairs(sdmdata[,2:5], cex=0.1)



PresLat<-Presence.Meter$lat
PresLong<-Presence.Meter$lon

PresLongLat<-data.frame(PresLong,PresLat)


pred_nf<-GhanaClimateSubset.Meter

#See dismo vignette 
set.seed(0)
group <- kfold(PresLongLat, 5)
pres_train <- PresLongLat[group != 1, ]
pres_test <- PresLongLat[group == 1, ]

ext<-extent(GhanaOutline.Meter)

AbsLat<-Absence.Meter$lat
AbsLong<-Absence.Meter$lon

AbsLongLat<-data.frame(AbsLong,AbsLat)


pred_nf<-GhanaClimateSubset.Meter


set.seed(0)
group <- kfold(AbsLongLat, 5)
#group
abs_train <- AbsLongLat[group != 1, ]

abs_test <- AbsLongLat[group == 1, ]


r <- raster(pred_nf, 1)
# plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
# plot(ext, add=TRUE, col='red', lwd=2)
# points(abs_train, pch='-', cex=0.5, col='black')
# points(abs_test, pch='-',  cex=0.5, col='black')
# points(pres_train, pch= '+', col='green')
# points(pres_test, pch='+', col='blue')



##Bioclim model MU pres/abs


bc<-bioclim(pred_nf,pres_train) #pres_train

#plot(bc, a=1,b=2,p=0.85)



e<-evaluate(pres_test,abs_test,bc,pred_nf)
e

tr<-threshold(e,'spec_sens')
#tr

pb<-predict(pred_nf,bc,ext=ext,progress='')
pb

par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
points(pres_train, pch='+')



##Domain Model

dm <- domain(pred_nf, pres_train)
e <- evaluate(pres_test, abs_test, dm, pred_nf)
e

pd = predict(pred_nf, dm, ext=ext, progress='')
par(mfrow=c(1,2))
plot(pd, main='Domain, raw values')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(pd > tr, main='presence/absence')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
points(pres_train, pch='+')








predictors<-GhanaClimateSubset.Meter
colnames(pres_train)<-colnames(abs_train)
train <- rbind(pres_train, abs_train)


pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(abs_train)))
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
#head(envtrain)

testpres <- data.frame( extract(predictors, pres_test) )
testabs <- data.frame( extract(predictors, abs_test) )


##GLM Model MU pres/abs


# logistic regression:
#names(envtrain)
gm1 <- glm(pa ~ wc2.1_30s_bio_1+wc2.1_30s_bio_12+wc2.1_30s_bio_15+wc2.1_30s_bio_7+wc2.1_30s_bio_14, family = binomial(link = "logit"), data=envtrain)

summary(gm1)
#coef(gm1)
gm2 <- glm(pa ~ wc2.1_30s_bio_1+wc2.1_30s_bio_12+wc2.1_30s_bio_15+wc2.1_30s_bio_7+wc2.1_30s_bio_14,
           family = gaussian(link = "identity"), data=envtrain)
evaluate(testpres,testabs,gm1)

ge2 <- evaluate(testpres, testabs, gm1)
ge2

pg <- predict(predictors, gm1, ext=ext)

#plot(pg, main='GLM/binomial, raw values')
#plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
tr <- threshold(ge2, 'spec_sens')

plot(pg > tr, main='presence/absence MU (GLM)')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')

legend(x=500183,y=615115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
points(pres_test,col="red",bg="red",pch=24)
points(abs_test,col="blue",bg="blue",pch=21)

# 
# 
# dev.off()
# tiff("output/GLMSDMWTestSet.tiff", width = 174, height = 174, units = 'mm', res = 1200)
# plot(pg > tr, main='presence/absence MU (GLM)')
# plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
# legend(x=500183,y=615115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
# scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
# north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
# points(pres_test,col="red",bg="red",pch=24)
# points(abs_test,col="blue",bg="blue",pch=21)
# dev.off()



```


##Random Forest SDM Model MU

```{r}
library(randomForest)
model <- pa ~ wc2.1_30s_bio_1+wc2.1_30s_bio_12+wc2.1_30s_bio_15+wc2.1_30s_bio_7+wc2.1_30s_bio_14
rf1 <- randomForest(model, data=envtrain)
model <- factor(pa) ~ wc2.1_30s_bio_1+wc2.1_30s_bio_12+wc2.1_30s_bio_15+wc2.1_30s_bio_7+wc2.1_30s_bio_14
rf2 <- randomForest(model, data=envtrain)
erf <- evaluate(testpres, testabs, rf1)
erf

pr <- predict(predictors, rf1, ext=ext)
plot(pr, main='Random Forest, regression')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(pr > tr, main='presence/absence MU (RF)')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
legend(x=500183,y=615115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
points(pres_test,col="red",bg="red",pch=24)
points(abs_test,col="blue",bg="blue",pch=21)

importance(rf2)


# dev.off()
# tiff("output/rfSDMWTestSet.tiff", width = 174, height = 174, units = 'mm', res = 1200)
# plot(pr > tr, main='presence/absence MU (RF)')
# plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
# legend(x=500183,y=615115,legend =c("MU +","MU -"),pch=c(17,16),col=c('red','blue'))
# scalebar(100000,xy = c(439429.8,61762), type="bar", divs = 2, label = c(0,50,100),below="km")
# north.arrow(x=500183, y=345827, len = 10000, cex.lab = 0.75, col = "black", fg = "red")
# points(pres_test,col="red",bg="red",pch=24)
# points(abs_test,col="blue",bg="blue",pch=21)
# 
# dev.off()

```

##Support Vector machines

```{r}

library(kernlab)
svm <- ksvm(pa ~ wc2.1_30s_bio_1+wc2.1_30s_bio_12+wc2.1_30s_bio_15+wc2.1_30s_bio_7+wc2.1_30s_bio_14, data=envtrain)

esv <- evaluate(testpres, testabs, svm)
esv

ps <- predict(predictors, svm, ext=ext)
par(mfrow=c(1,2))
plot(ps, main='Support Vector Machine')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
tr <- threshold(esv, 'spec_sens')
plot(ps > tr, main='presence/absence')
plot(GhanaOutline.Meter, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(abs_train, pch='-', cex=0.25)

```

##Combining model predictions (Final two models (rf,glm) shown seperately and not combined)
```{r}
# Final 
# models <- stack(pg, pr )
# names(models) <- c("glm", "rf")
# plot(models)
# 
# m <- mean(models)
# plot(m, main='average score')
# 
# auc <- sapply(list(ge2, erf), function(x) x@auc)
# w <- (auc-0.5)^2
# m2 <- weighted.mean( models[[c("glm", "rf")]], w)
# plot(m2)
# plot(GhanaOutline.Meter,add=T)
# plot(Presence.Meter,col="red",bg="red",add=T,pch=24)
# plot(Absence.Meter,col="blue",bg="blue",add=T,pch=21)
```




