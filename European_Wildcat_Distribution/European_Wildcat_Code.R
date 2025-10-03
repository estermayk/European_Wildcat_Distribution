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
