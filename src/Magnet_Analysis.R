datdir <- "/Users/chadgevans/Research/Other/Data/Shape_Files/Illinois_Shapefiles/Illinois_Shape_File" # Set working directory
datdir2 <- "/Users/chadgevans/Research/Other/Data/Shape_Files/United_States_Shapefile" # Set working directory
Product_Directory<-"/Users/chadgevans/Research/Magnets"
libraries <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap","spdep","spatstat","raster","ggplot2","reshape2","scales","zoom","sp","dismo","gdata","gmodels","EBImage")
lapply(libraries, require, character.only = TRUE)

USA.shp <- readOGR(dsn = datdir2, layer = "cb_2014_us_nation_5m")
Il.shp <- readOGR(dsn = datdir, layer = "cb_2014_17_bg_500k")
Il.shp <- spTransform(Il.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
cook.shp=Il.shp[Il.shp$COUNTYFP=="031",] # Select only cook county
cook.shp <- spTransform(cook.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
chi.shp=readOGR(dsn = "/Users/chadgevans/Research/Other/Data/Shape_Files/Illinois_Shapefiles/Chicago_Shape_File/City_Boundary", layer = "City_Boundary")
chi.shp <- spTransform(chi.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
water.shp=readOGR(dsn = "/Users/chadgevans/Research/Other/Data/Shape_Files/Illinois_Shapefiles/Hydrography_Shape_File", layer = "ne_10m_lakes")
water.shp <- spTransform(water.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
d <- over(cook.shp, chi.shp[,"OBJECTID"]) 
cook.shp$bcode <- d$OBJECTID
chiblocks.shp<-cook.shp[!is.na(cook.shp$bcode)=="TRUE",]
pop=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B01003_with_ann.csv",sep=",", skip=1, header=T)
pop=pop[,c(1,2,4)]
names(pop)=c("AFFGEOID","GEOID","POP")
black=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B02009_with_ann.csv",sep=",", skip=1, header=T)
black=black[,c(1,2,4)]
names(black)=c("AFFGEOID","GEOID","BLACK")
d=merge(pop,black)
d$PCTBLACK=d$BLACK/d$POP
traveltime=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B08303_with_ann.csv",sep=",", skip=1, header=T)
traveltime$PCT30MIN=(traveltime$Estimate..Total....90.or.more.minutes+traveltime$Estimate..Total....60.to.89.minutes+traveltime$Estimate..Total....45.to.59.minutes+traveltime$Estimate..Total....40.to.44.minutes+traveltime$Estimate..Total....35.to.39.minutes+traveltime$Estimate..Total....30.to.34.minutes)/traveltime$Estimate..Total.
traveltime=traveltime[,c("Id","Id2","PCT30MIN")]
names(traveltime)=c("AFFGEOID","GEOID","PCT30MIN")
medrooms=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B25018_with_ann.csv",skip=1,sep=",",header=T,colClasses = c("factor","numeric","factor","numeric","numeric"),na.strings = c("-","**","9.0+","***")) # Shit this bug took time to figure out
medrooms=medrooms[,c(1,2,4)]
names(medrooms)=c("AFFGEOID","GEOID","MEDROOMS")
homeage=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B25034_with_ann.csv",skip=1,sep=",",header=T)
homeage$PCTHOMEAGE40=homeage$Estimate..Total....Built.1939.or.earlier/homeage$Estimate..Total.
homeage=homeage[,c("Id","Id2","PCTHOMEAGE40")]
names(homeage)=c("AFFGEOID","GEOID","PCTHOMEAGE40")
medincome=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B19013_with_ann.csv",skip=1,sep=",",header=T)
levels(medincome$Estimate..Median.household.income.in.the.past.12.months..in.2013.inflation.adjusted.dollars.)[1]<-NA
medincome=medincome[,c(1,2,4)]
names(medincome)=c("AFFGEOID","GEOID","MEDINCOME")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # needed a function to make the data read in w/o problems
medincome$MEDINCOME=as.numeric.factor(medincome$MEDINCOME)
medvalue=read.table("/Users/chadgevans/Dissertation/Projects/Build_Dataset/ACS/State_Level_Data/2013_5YR/ACS_13_5YR_B25077_with_ann.csv",skip=1,sep=",",header=T)
levels(medvalue$Estimate..Median.value..dollars.)[1]<-NA
levels(medvalue$Estimate..Median.value..dollars.)[1]<-NA # This is censoring the data at 1 mil.  it is [1] because the 1 replaces the previous
levels(medvalue$Estimate..Median.value..dollars.)[1]<-NA
medvalue=medvalue[,c(1,2,4)]
names(medvalue)=c("AFFGEOID","GEOID","MEDVALUE")
medvalue$MEDVALUE=as.numeric.factor(medvalue$MEDVALUE)
d=merge(merge(merge(merge(merge(d,traveltime),medrooms),homeage),medincome),medvalue)
d$MEDROOMS2=d$MEDROOMS*d$MEDROOMS
cookdata <- as(cook.shp, "data.frame")
cook.shp@data <- data.frame(as(cook.shp, "data.frame"), d[match(cook.shp@data[, "GEOID"], d[, "GEOID"]),])
crimedata=read.csv("/Users/chadgevans/Research/Magnets/Chicago_Data/Crime_Data/Crimedata.csv") # this is the 2013 data
# crimedata2=read.csv("/Users/chadgevans/Documents/Chads_documents/Work/Data/Chicago_Data/Crime_Data/Crimedata2.csv") # this is the most recent crime data from the last year
crimedata$COUNT=1
crimedata=crimedata[,c(20,21,23)]
crimedata=na.omit(crimedata) # 1926 or 0.006288015 or .01% missing data
# Point in Polygon Spatial Join
crime.shp <- SpatialPointsDataFrame(coords = crimedata[, c("Longitude","Latitude")], data = data.frame(values = crimedata$COUNT))
proj4string(crime.shp) <- CRS("+proj=longlat +datum=WGS84")

# Map of the City of Chicago, Situated within Cook County
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="City of Chicago, Situated within Cook County")
plot(cook.shp, add=T,col="lightgrey")
plot(chiblocks.shp, add=T, col="red3")
legend("bottomleft", inset=.05, c("Cook County","City of Chicago"), fill=c("lightgrey","red3"), horiz=FALSE, bg="white")   

# Census Tracts with Majorities of African Americans
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="African American Census Blocks in Cook County")
plot(cook.shp, add=T,col="lightgrey")
pctblack.shp=cook.shp[!is.na(cook.shp$PCTBLACK),]
sel <- pctblack.shp$PCTBLACK > 0.50
plot(pctblack.shp[sel, ], col = "turquoise", add = TRUE) # add selected zones to map
legend("bottomleft", inset=.05, c("Blocks with Majority \n African Americans"), fill=c("turquoise"), horiz=FALSE, bg="white")   

dsdat <- over(cook.shp, crime.shp, fn=sum) 
inds <- row.names(dsdat)
cook.shp@data[inds, "COUNTS"] <- dsdat
cook.shp$PERCAPCRIME=NA
cook.shp$PERCAPCRIME<-cook.shp$COUNTS/cook.shp$POP
cook.shp$PERCAPCRIME[cook.shp$PERCAPCRIME>2]<-NA
cookcrimedata=cook.shp@data[,c(5,6,23)]

# High Crime Census Blocks in Chicago
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="High Crime Census Blocks in Cook County")
box()
plot(cook.shp, add=T,col="lightgrey")
highcrime.shp=cook.shp[!is.na(cook.shp$PERCAPCRIME),]
sel <- highcrime.shp$PERCAPCRIME > .25
plot(highcrime.shp[sel, ], col = "orange", add = TRUE) # add selected zones to map
legend("bottomleft", inset=.05, c("Blocks with > .25 \n crimes per capita"), fill=c("orange"), horiz=FALSE, bg="white")   

# Elementary Schools in Chicago
#This lists the schools with report cards for 2013
#schooldata1=read.csv("/Users/chadgevans/Desktop/R_Tutorials/aff_download/Chicago_Public_Schools_-_Elementary_School_Progress_Report__2013-2014_2.csv")
#schooldata2=read.xls("/Users/chadgevans/Desktop/R_Tutorials/aff_download/CPS_Elementary_Schools3.xls")
#schooldata=merge(schooldata1,schooldata2, by=c("Longitude","Latitude"), all.x=T)
schooldata=read.csv("/Users/chadgevans/Research/Magnets/Chicago_Data/School_Data/schooldata.csv")
schools.shp <- SpatialPointsDataFrame(coords = schooldata[, c("Longitude","Latitude")],data = schooldata)
summary(schools.shp)
proj4string(schools.shp) <- CRS("+proj=longlat +datum=WGS84")
plot(USA.shp, ylim=c(41.64454, 42.02304), xlim=c(-87.94011, -87.52414),col = "wheat2",bg="azure2", main="Elementary Schools in Chicago")
box()
plot(chi.shp, add=T, col="lightgrey")
plot(schools.shp, pch = 17, add=T, col="red")
legend("bottomleft", inset=.05, pch = 17, c("Elementary School"), col="red", horiz=FALSE, bg="white")   

# This is for the color image
png(file.path(Product_Directory,"Chicago_Elem_Schools_2013.png"), width = 1500, height = 1500)
plot(USA.shp, ylim=c(41.63454, 42.02304), xlim=c(-87.94011, -87.52414),col = "wheat2",bg="azure2")
box()
plot(chiblocks.shp, add=T, col="lightgrey")
magnets.shp<-schools.shp[schools.shp$Type2=="Magnet",]
clusters.shp<-schools.shp[schools.shp$Type2=="Magnet Cluster",]
openenroll.shp<-schools.shp[schools.shp$Type2=="Open Enrollment", ]
plot(magnets.shp, pch = 17, add=T, col="red",cex=2.5)
plot(clusters.shp, pch = 17, add=T, col="blue",cex=2.5)
plot(openenroll.shp, pch = 17, add=T, col="darkgreen",cex=2.5)
legend("bottomleft", inset=.05, pch = c(17,17,17,15), c("Traditional Magnet","Magnet Cluster","Neighborhood School", "Chicago Census Block"), col=c("red","blue","darkgreen","lightgrey"), horiz=FALSE, cex=2.5)   
mtext("Lake Michigan             ",side=1,line=-75,adj=1,cex=2.5,col="darkblue")
mtext("Source: Chicago Public Schools Data Portal 2013-2014. Map excludes elementary charters, magnets that require admissions tests, \n small schools and special education schools.",side=1,line=3,adj=0,cex=2,col="black")
mtext("           Illinois",side=1,line=-45,adj=0,cex=3,col="black")
dev.off()


###### This is for a printed version in black and white
png(file.path(Product_Directory,"Chicago_Elem_Schools_2013_bw.png"), width = 1500, height = 1500)
plot(USA.shp, ylim=c(41.63454, 42.02304), xlim=c(-87.94011, -87.52414),col = "lightgrey",bg="azure2")
box()
plot(chiblocks.shp, add=T, col="white")
magnets.shp<-schools.shp[schools.shp$Type2=="Magnet",]
clusters.shp<-schools.shp[schools.shp$Type2=="Magnet Cluster",]
openenroll.shp<-schools.shp[schools.shp$Type2=="Open Enrollment", ]
plot(magnets.shp, pch = 17, add=T, col="black",cex=2.5)
plot(clusters.shp, pch = 17, add=T, col="gray44",cex=2.5)
plot(openenroll.shp, pch = 17, add=T, col="grey64",cex=2.5)
legend("bottomleft", inset=.05, pch = c(17,17,17,22), c("Traditional Magnet","Magnet Cluster","Neighborhood School", "Chicago Census Block"), col=c("black","gray44","gray64","black"), horiz=FALSE, cex=2.5)   
mtext("Lake Michigan             ",side=1,line=-75,adj=1,cex=2.5,col="darkblue")
mtext("Source: Chicago Public Schools Data Portal 2013-2014. Map excludes elementary charters, magnets that require admissions tests, \n small schools and special education schools.",side=1,line=3,adj=0,cex=2,col="black")
mtext("           Illinois",side=1,line=-45,adj=0,cex=3,col="black")
dev.off()
color.image<-readImage(file.path(Product_Directory,"Chicago_Elem_Schools_2013_bw.png"))
bw.image<-channel(color.image,"gray")
writeImage(bw.image,file="Figure_1.png")

ShapeFile.Dissolved<-gUnionCascaded(cook.shp)
x<-gRelate(chiblocks.shp, ShapeFile.Dissolved, byid= TRUE)
table(x)
poly.border<-which(x %in% c("2FF11F212"))
michblocks.shp <- crop(chiblocks.shp[poly.border, ], extent(-87.7, -87.525, 41.73180, 42.25))
michblocks.shp$MICH<-1
cook.shp@data$MICH<-0
indsd2 <- row.names(michblocks.shp@data)
cook.shp@data[indsd2, "MICH"] <- michblocks.shp@data$MICH

dsdat <- over(schools.shp,cook.shp)
data=data.frame(schools.shp@data,dsdat)

mean(data$MEDVALUE[data$Type=="Magnet"],na.rm=T)
mean(data$MEDVALUE[data$Type=="Magnet Cluster"],na.rm=T)
mean(data$MEDVALUE[data$Type=="Open Enrollment"],na.rm=T)

# Centering Quadratic Terms
data$Bk=data$PCTBLACK - mean(data$PCTBLACK, na.rm=T)
data$Bk2=data$Bk*data$Bk
data$CMEDROOMS=data$MEDROOMS-mean(data$MEDROOMS, na.rm=T)
data$CMEDROOMS2=data$CMEDROOMS*data$CMEDROOMS

data$Type=relevel(data$Type, ref="Open Enrollment")
data$Type2=relevel(data$Type2, ref="Open Enrollment")
#data$Type2=relevel(data$Type2, ref="Magnet Cluster")

data=data[!data$Type2=="MagTest",] # removes schools requiring admissions testing
data=data[!data$Type2=="Small",] # removes schools requiring admissions testing
data=data[!data$Type2=="Special Ed",] # removes schools requiring admissions testing
data=data[!data$Type2=="Charter",] # removes schools requiring admissions testing

data$CPERCAPCRIME=data$PERCAPCRIME-mean(data$PERCAPCRIME,na.rm=T)
data$CLOGPCT30MIN=log(data$PCT30MIN)-mean(log(data$PCT30MIN), na.rm=T)
data$CPCTHOMEAGE40=data$PCTHOMEAGE40-mean(data$PCTHOMEAGE40,na.rm=T)
data$CLOGMEDINCOME=log(data$MEDINCOME)-mean(log(data$MEDINCOME), na.rm=T)
data$CMICH=data$MICH-mean(data$MICH,na.rm=T)
data$CREAD=data$ISAT.Exceeding.Reading...-mean(data$ISAT.Exceeding.Reading...,na.rm=T)
data$CMATH=data$ISAT.Exceeding.Math..-mean(data$ISAT.Exceeding.Math..,na.rm=T)

mod=lm(log(MEDVALUE)~Magnet, data=data)
mod=lm(log(MEDVALUE)~Magnet+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+Bk+Bk2+CMICH, data=data)
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+Bk+Bk2+CMICH, data=data)
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+Bk+Bk2+CMICH+CREAD+CMATH, data=data)

#mod=lm(log(MEDVALUE)~Specialty+PERCAPCRIME+Bk+Bk2+log(PCT30MIN)+CMEDROOMS+CMEDROOMS2+PCTHOMEAGE40+log(MEDINCOME)+MICH, data=data)

# Correlation between Magnet and school Quality
data$CLUSTER[data$Type2=="Magnet Cluster"]<-1
data$CLUSTER[!data$Type2=="Magnet Cluster"]<-0
data$TRADITIONAL[data$Type2=="Magnet"]<-1
data$TRADITIONAL[!data$Type2=="Magnet"]<-0

d=data[,c("CLUSTER","TRADITIONAL","ISAT.Exceeding.Reading...")]
d=na.omit(d)
cor(d$CLUSTER,d$ISAT.Exceeding.Reading...) # correlation of .01949
cor(d$TRADITIONAL,d$ISAT.Exceeding.Reading...)  # correlation of 0.3234661

# Sensitivity Test
data2=data[data$Closed==0,]

# Descriptive Statistics
TYPE="Open Enrollment" # TYPE="Magnet Cluster" TYPE="Magnet"
a=mean(data$MEDVALUE[data$Type==TYPE], na.rm=T)
b=mean(data$PERCAPCRIME[data$Type==TYPE], na.rm=T)
c=mean(data$MEDROOMS[data$Type==TYPE], na.rm=T)
d=mean(data$MEDINCOME[data$Type==TYPE], na.rm=T)
e=mean(data$PCT30MIN[data$Type==TYPE], na.rm=T)
f=mean(data$PCTHOMEAGE40[data$Type==TYPE], na.rm=T)
g=mean(data$PCTBLACK[data$Type==TYPE], na.rm=T)
h=mean(data$MICH[data$Type==TYPE], na.rm=T)
i=mean(data$ISAT.Exceeding.Reading...[data$Type==TYPE], na.rm=T)
j=mean(data$ISAT.Exceeding.Math..[data$Type==TYPE], na.rm=T)
Variable=c("Average Median Home Value","Crime Rate","Median # of Rooms","Median Income","Proportion Traveling >30 Minutes to Work","Proportion of Homes Built Before 1940","Proportion Black","Lake Michigan","Proportion Exceeding ISAT Reading","Proportion Exceeding ISAT Math")
Open_Enrollment=round(c(a,b,c,d,e,f,g,h,i,j),2)
Magnet_Cluster=round(c(a,b,c,d,e,f,g,h,i,j),2)
Magnet=round(c(a,b,c,d,e,f,g,h,i,j),2)
Table=cbind(Variable,Open_Enrollment,Magnet_Cluster,Magnet)

# Exploratory Analysis
# When Interaction Term is included
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+Bk+Bk2+CMICH+CREAD+CREAD*Type2, data=data)
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+Bk+Bk2+CMICH+CMATH+CMATH*Type2, data=data)

#Reading
exp(12.146903+0.042634)-exp(12.146903) # but main term is insignificant
#Math
exp(12.135240+0.044257)-exp(12.135240) # but main term is insignficant

