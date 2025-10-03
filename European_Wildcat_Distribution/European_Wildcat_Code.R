install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(predicts)
library(terra)
occdata <- geodata::sp_occurrence("Felis", "silvestris", geo=TRUE,removeZeros=TRUE,start=1,end=10000)
#importing data from GBIF on Felis silvestris
#geo=TRUE means only records with latitude and longitude are downloaded
#taking first 10,000 datapoints only for time reasons
dim(occdata)
occdata[1:10,]
wrld <- world(path=".")
#gives outline of world's political boundaries
plot(wrld, xlim=c(-180,180), ylim=c(-80,80), col="light yellow", border="light gray")
points(occdata$lon, occdata$lat, col='blue', pch=20)
#adds the occurrences as points 
occdata1<-subset(occdata,lon>-50)
#cleaning the data by removing points in the americas
plot(wrld, xlim=c(-180,180), ylim=c(-80,80), col="light yellow", border="light gray")
points(occdata1$lon, occdata1$lat, col='blue', pch=20)
#checking we removed the right points
dups <- duplicated(occdata1[, c('lon', 'lat')])
sum(dups)
occ <- occdata1[!dups, ]
#finding and removing duplicates
#downloading worldclim data
output_dir <- "C:/Users/kirby/Documents/European_Wildcat_Distribution/practical1_climdata"
bio_glob<-worldclim_global(var="bio", res=10,path=output_dir, version="2.1")
dim(bio_glob)
summary(occ$lon)
summary(occ$lat)
#creating a lon/lat crop to just the area of interest
e <- ext(-25, 90, -40, 60)
#shortening predictor names (11th-16th characters)
predictors <- crop(bio_glob, e)
names(predictors)<-substring(names(predictors),11,16)
#looking at climate data - first 9 predictors
plot(predictors,1:9)
#and now plotting species data on top for first variable
plot(predictors,1)
points(occ$lon,occ$lat, col='orange',pch=16,cex=0.2)
#sampling background data from region of interest (e)
bg<-spatSample(predictors,5000,"random", na.rm=TRUE, as.points=TRUE,ext=e)
#now plotting with worldclim data for bio_1
plot(predictors, 1)
points(bg, cex=0.1)
#now matching climate and occurrence data
occlatlon<-cbind(occ$lon,occ$lat)
presvals <- extract(predictors, occlatlon)
#presvals is the climate data for where the species is present
backvals <- values(bg)
#backvals is the climate data for the background data
bg_lonlat<-geom(bg)
lonlats<-rbind(occlatlon, bg_lonlat[,c("x","y")])
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(backvals)))
#The first column of pb  is a vector of 1s for presences and 0s for background data.
sdmdata <- data.frame(cbind(lonlats,pb, rbind(presvals, backvals)))
#here we combine the presence and background data into a single data frame
sdmdata
pairs(sdmdata[,4:7], cex=0.1)
#just checking for correlation between climate variables - in a real study we would look for highly correlated ones and remove as these can cause issues
#now fitting a species distribution model
sdmdata<-subset(sdmdata,is.na(bio_1)==F)
#here we're just removing a couple of rows where the climate data are NAs.
specdata<-as.data.frame(cbind(rep("Felis silvestris",length(sdmdata[,1])),
                              sdmdata))
names(specdata)[1:4]<-c("species","longitude","latitude","presabs")
specdata<-subset(specdata,presabs==1)
backdata<-as.data.frame(cbind(rep("background",length(sdmdata[,1])),
                              sdmdata))
names(backdata)[1:4]<-c("","longitude","latitude","presabs")
backdata<-subset(backdata,presabs==0)
write.table(specdata[,-4],paste(output_dir,"/Felissilvestris_swd.csv",sep=""),col.names=T,row.names=F,sep=",")
write.table(backdata[,-4],paste(output_dir,"/background.csv",sep=""),col.names=T,row.names=F,sep=",")
model<-MaxEnt(sdmdata[,-c(1:3)],sdmdata[,3],removeDuplicates=TRUE)
model
#partialResponse function - looks at likelihood of occurrence gradient with climatic variables
plot(model)
#biggest predictor by over 20% is BIO19 = Precipitation of Coldest Quarter
#now looking at predicted climate suitability and where it matches recording
predictedocc <- predict(model, predictors, args=c("outputformat=raw")) 
par(mfrow=c(2,1))
plot(predictedocc)
plot(predictedocc)
points(occlatlon,pch=".")
#to predict future distributions we need future climate data
bio_fut<-cmip6_world(model='ACCESS-ESM1-5', ssp='245', time='2041-2060', var='bioc', res=10, path=output_dir)
fut_predictors<-crop(bio_fut,e)

plot(predictors,2)
plot(fut_predictors,2)

names(fut_predictors)<-names(predictors)
fut_predictedocc <- predict(model, fut_predictors, args=c("outputformat=raw")) 

par(mfrow=c(2,1))
plot(predictedocc,main="current")

plot(fut_predictedocc,main="2050")