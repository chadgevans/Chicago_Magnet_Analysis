Magnet Analysis
================
Chad Evans

Built with 3.3.2. Last run on 2017-11-10.

-   [Configure](#config)
    -   Libraries
    -   directories
    -   data
-   [Munge](#munge)
-   [Exploratory Analysis](#exploratory-analysis)
    -   Chicago Blocks Map
    -   Racial Blocks Map
    -   Crime Blocks Map
    -   [Elementary Schools in Chicago](#elementary-schools-in-chicago)
    -   General Schools Map
    -   School Typology Map Color
    -   School Typology Map Grayscale
    -   Chicago blocks on Lake Michigan
-   [Descriptive Statistics](#descriptive-statistics)
-   [Analysis](#analysis)
-   [Model Diagnostics](#model-diagnostics)
-   [Sensitivity Analysis](#sensitivity-analysis)
    -   [Propensity Score Analysis](#propensity-score-analysis)
        -   Propensity Scores
        -   Common Support
        -   Match It
        -   Visual Inspection
        -   Balance
        -   [Statistical test of treatment](#statistical-test-of-treatment)
    -   [Other Exploratory Analysis](#other-exploratory-analysis)

Configure
=========

First, set up the environment: read in the libraries, indicate the directories, read in and clean the data.

``` r
USA.shp <- readOGR(dsn = Shapefile_Directory, layer = "cb_2014_us_nation_5m")
Il.shp <- readOGR(dsn = Shapefile_Directory, layer = "cb_2014_17_bg_500k")
cook.shp=Il.shp[Il.shp$COUNTYFP=="031",]
chi.shp=readOGR(dsn = Shapefile_Directory, layer = "City_Boundary")
water.shp=readOGR(dsn = Shapefile_Directory, layer = "ne_10m_lakes")
pop=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B01003_with_ann.csv"), skip = 1)
black=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B02009_with_ann.csv"), skip = 1)
traveltime=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B08303_with_ann.csv"), skip=1)
medrooms=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B25018_with_ann.csv"), skip=1, na = c("**","-","***","9.0+"))
homeage=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B25034_with_ann.csv"), skip=1)
medincome=read_csv(file.path(ACS_Directory, "ACS_13_5YR_B19013_with_ann.csv"), skip=1, na = c("**","-","***","250,000+"))
medvalue=read_csv(file.path(ACS_Directory,"ACS_13_5YR_B25077_with_ann.csv"),skip=1, na = c("-","1,000,000+"))
crimedata=read_csv(file.path(Crime_Directory,"Crimedata.csv"))
schooldata=read_csv(file.path(Schools_Directory, "schooldata.csv")) %>% mutate_if(is.character, factor)
```

``` r
USA.shp@data$id = rownames(USA.shp@data)
USA.points = fortify(USA.shp, region="id")
USA.df = merge(USA.points, USA.shp@data, by="id")
```

Now project all the shapefiles to the same coordinate system.

``` r
Il.shp <- spTransform(Il.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
cook.shp <- spTransform(cook.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
chi.shp <- spTransform(chi.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
water.shp <- spTransform(water.shp, CRS=CRS("+proj=longlat +datum=WGS84")) 
```

Use spatial joins to create chicago city block (tracts) shapefile.

``` r
d <- over(cook.shp, chi.shp[,"OBJECTID"]) # need dependent projections correct
cook.shp$bcode <- d$OBJECTID
chiblocks.shp<-cook.shp[!is.na(cook.shp$bcode)=="TRUE",]
```

Munge
=====

Clean up files and reduce to necessary files.

``` r
medrooms$`Estimate; Median number of rooms`[c(1933,1934,1936:1941,1943,1944)]<-9.0 # hack to resolve 9.0+ character, imposes ceiling of 9 rooms.
medincome$`Estimate; Median household income in the past 12 months (in 2013 inflation-adjusted dollars)`[c(1933,1937,1944,1951,1952)]<-250000 # hack to resolve 250,000+, ceiling imposed on median income of 250k.
medvalue$`Estimate; Median value (dollars)`[c(321,398,1075,1933,1934,1944,1945,1948,1949,1952,1954,1972,3071)]<-1000000 # hack to resolve 1,000,000+, ceiling imposed on median value of 1m.

pop <- pop  %>% rename(POP = `Estimate; Total`) %>% select(Id,POP)
black <- black %>% rename(BLACK = `Estimate; Total:`) %>% select(Id,BLACK)
pctblack<-inner_join(pop,black) %>% mutate(PCTBLACK = BLACK/POP)
traveltime<- traveltime %>% mutate(PCT30MIN = (`Estimate; Total: - 30 to 34 minutes`+`Estimate; Total: - 35 to 39 minutes`+`Estimate; Total: - 40 to 44 minutes`+`Estimate; Total: - 45 to 59 minutes`+`Estimate; Total: - 60 to 89 minutes`+`Estimate; Total: - 90 or more minutes`)/`Estimate; Total:`) %>% select(Id,PCT30MIN)
medrooms<- medrooms %>% rename(MEDROOMS = `Estimate; Median number of rooms`) %>% select(Id,MEDROOMS)
homeage<-homeage %>% mutate(PCTHOMEAGE40 = `Estimate; Total: - Built 1939 or earlier`/`Estimate; Total:`) %>% select(Id,PCTHOMEAGE40)
medincome<- medincome %>% rename(MEDINCOME = `Estimate; Median household income in the past 12 months (in 2013 inflation-adjusted dollars)`) %>% select(Id,MEDINCOME)
medvalue<- medvalue %>% rename(MEDVALUE = `Estimate; Median value (dollars)`) %>% select(Id,MEDVALUE)
crimedata<-crimedata %>% select(Longitude,Latitude) %>% na.omit() # 1926 or 0.006288015 or .01% missing data
crimedata$COUNT=1 # One incident at each unique longitude and latitude combo
```

Merge all necessary columns into Tidy-formated data frame.

``` r
dfs_list <- list(pctblack,traveltime,medrooms,homeage, medincome, medvalue)
df<-Reduce(left_join, dfs_list)
rm(pctblack,traveltime,medrooms,homeage, medincome, medvalue)

chiblocks.shp@data <- chiblocks.shp@data %>% as_data_frame %>% rename(Id = `AFFGEOID`) 
chiblocks.shp@data <- left_join(chiblocks.shp@data, df)

crime.shp <- SpatialPointsDataFrame(coords = crimedata[, c("Longitude","Latitude")], data = data.frame(values = crimedata$COUNT))
proj4string(crime.shp) <- CRS("+proj=longlat +datum=WGS84")
crime.shp@data <- crime.shp@data %>% as_data_frame()
```

Exploratory Analysis
====================

Chicago Blocks Map
------------------

Let's make sure all the spatial joins were done properly. First, let's plot all the geographies: USA, Il, cook, and city of chicago.

``` r
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="City of Chicago, Situated within Cook County")
box()
plot(cook.shp, add=T,col="lightgrey")
plot(chiblocks.shp, add=T, col="red3")
legend("bottomleft", inset=.05, c("Cook County","City of Chicago"), fill=c("lightgrey","red3"), horiz=FALSE, bg="white") 
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
```

<img src="graphs/Chicago_Cook_County_Map-1.png" width="500" height="500">

Racial Block Map
----------------

Let's highlight the areas of Chicago that are majority African American. The southside and westside of Chicago should have majority black residents.

``` r
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="African American Census Blocks in Cook County")
#box()
plot(chiblocks.shp, add=T, col="lightgrey")
pctblack.shp=chiblocks.shp[!is.na(chiblocks.shp$PCTBLACK),]
sel <- pctblack.shp$PCTBLACK > 0.50
plot(pctblack.shp[sel, ], col = "turquoise", add = TRUE) # add selected zones to map
legend("bottomleft", inset=.05, c("Blocks with Majority \n African Americans"), fill=c("turquoise"), horiz=FALSE, bg="white") 
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
```

<img src="graphs/Majority_Black_Census_Tracts_Map-1.png" width="500" height="500">

This map is consistent with expectations.

Crime Blocks Map
----------------

Our analysis needs to factor in crime rates across Chicago city blocks. To calculate crime rates, we spatially join crime incidents to block level geographies. Then we sum up the number of crime incidents and divide by a measure of the population in each black. Using ACS data, I divided by the number of respondents in each tract. This is not a traditional crime rate as I did not scale up to an estimate of the population in each tract. However, this does produce an equally good measure of crime prevalence that differs only by the sample weight.

``` r
n_crimes <- over(chiblocks.shp, crime.shp, fn=sum) %>% as_data_frame()
n_crimes[is.na(n_crimes)]<-0 # 140 blocks (of 2329) have no crimes reported.  I substiute 0 for those NAs
chiblocks.shp@data[, "COUNTS"] <- n_crimes
chiblocks.shp@data <- chiblocks.shp@data %>% mutate(PERCAPCRIME = COUNTS/POP) # Incidents per population
chiblocks.shp$PERCAPCRIME[chiblocks.shp$PERCAPCRIME>10]<-0 # Because these blocks have 0 population or were not counted by ACS, resulting in -inf, set to zero
```

Now let's plot the high crime areas of Chicago. I expect there to be higher crime in the downtown and south and west side. The periphery, particularly the north, should have lower crime rates.

``` r
plot(USA.shp, ylim=c(41.6, 42), xlim=c(-88.4, -87.35),col = "wheat2",bg="azure2", main="High Crime Census Blocks in Cook County")
box()
plot(chiblocks.shp, add=T,col="lightgrey")
plot(chiblocks.shp[chiblocks.shp$PERCAPCRIME > .2, ], col = "orange", add = TRUE) # add selected zones to map
legend("bottomleft", inset=.05, c("Blocks with > .2 \n crimes per ACS respondent"), fill=c("orange"), horiz=FALSE, bg="white") 
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
```

<img src="graphs/High_Crime_Census_Blocks_in_Chicago-1.png" width="500" height="500">

Elementary Schools in Chicago
-----------------------------

### General Schools Map

First, create a shapefile based on the geographical location of each school. Project this file into the working coordinate system.

``` r
schools.shp <- SpatialPointsDataFrame(coords = schooldata[, c("Longitude","Latitude")],data = schooldata)
proj4string(schools.shp) <- CRS("+proj=longlat +datum=WGS84")
```

Now, let's plot those schools over the city of chicago base file.

``` r
plot(USA.shp, ylim=c(41.64454, 42.02304), xlim=c(-87.94011, -87.52414),col = "wheat2",bg="azure2", main="Elementary Schools in Chicago")
box()
plot(chiblocks.shp, add=T, col="lightgrey")
plot(schools.shp, pch = 17, add=T, col="red")
legend("bottomleft", inset=.05, pch = 17, c("Elementary School"), col="red", horiz=FALSE, bg="white") 
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
```

<img src="graphs/Elem_Schools_in_Chicago_Map-1.png" width="500" height="500">

### School Typology Map Color

``` r
plot(USA.shp, ylim=c(41.63454, 42.02304), xlim=c(-87.94011, -87.52414),col = "wheat2",bg="azure2",)
box()
plot(chiblocks.shp, add=T, col="lightgrey")
magnets.shp<-schools.shp[schools.shp$Type2=="Magnet",]
clusters.shp<-schools.shp[schools.shp$Type2=="Magnet Cluster",]
openenroll.shp<-schools.shp[schools.shp$Type2=="Open Enrollment", ]
plot(magnets.shp, pch = 17, add=T, col="red",cex=1)
plot(clusters.shp, pch = 17, add=T, col="blue",cex=1)
plot(openenroll.shp, pch = 17, add=T, col="darkgreen",cex=1)
legend("bottomleft", inset=.05, pch = c(17,17,17,15), c("Traditional Magnet","Magnet Cluster","Neighborhood School", "Chicago Census Block"), col=c("red","blue","darkgreen","lightgrey"), horiz=FALSE, cex=1)   
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
mtext("Source: Chicago Public Schools Data Portal 2013-2014. Map excludes elementary charters, magnets that require admissions tests, small schools and special education schools.",side=1,line=3,adj=0,cex=.6,col="black")
```

<img src="graphs/School_Types_Chicago_Color-1.png" width="500" height="500">

### School Typology Map Grayscale

``` r
plot(USA.shp, ylim=c(41.63454, 42.02304), xlim=c(-87.94011, -87.52414),col = "lightgrey",bg="gray96")
box()
plot(chiblocks.shp, add=T, col="white")
magnets.shp<-schools.shp[schools.shp$Type2=="Magnet",]
clusters.shp<-schools.shp[schools.shp$Type2=="Magnet Cluster",]
openenroll.shp<-schools.shp[schools.shp$Type2=="Open Enrollment", ]
plot(magnets.shp, pch = 17, add=T, col="black",cex=1)
plot(clusters.shp, pch = 17, add=T, col="gray44",cex=1)
plot(openenroll.shp, pch = 17, add=T, col="grey64",cex=1)
legend("bottomleft", inset=.05, pch = c(17,17,17,22), c("Traditional Magnet","Magnet Cluster","Neighborhood School", "Chicago Census Block"), col=c("black","gray44","gray64","black"), horiz=FALSE, cex=1)  
mtext("Illinois",side=1,line=-20,adj=.1,cex=2,col="black")
mtext("Lake Michigan",side=1,line=-30,adj=.95,cex=1.5,col="black")
mtext("Source: Chicago Public Schools Data Portal 2013-2014. Map excludes elementary charters, magnets that require admissions tests, small schools and special education schools.",side=1,line=1,adj=0,cex=.6,col="black")
```

<img src="graphs/School_Types_Chicago_BW-1.png" width="500" height="500">

Chicago blocks on Lake Michigan
-------------------------------

``` r
ShapeFile.Dissolved<-gUnionCascaded(chiblocks.shp)
x<-gRelate(chiblocks.shp, ShapeFile.Dissolved, byid= TRUE)
poly.border<-which(x %in% c("2FF11F212"))
#library(raster)  # crop from raster() requires select() from base
michblocks.shp <- raster::crop(chiblocks.shp[poly.border,], raster::extent(-87.673, -87.525, 41.70, 42.25))
michblocks.shp$MICH<-1
chiblocks.shp@data$MICH<-0
rows <- row.names(michblocks.shp@data)
chiblocks.shp@data[rows, "MICH"] <- michblocks.shp@data$MICH
spjoin <- over(schools.shp, chiblocks.shp) %>% as_data_frame() 
data=bind_cols(schools.shp@data, spjoin)
```

There are no schools located in neighborhoods that directly border Lake Michigan

Let's change the reference category of the schools to "Open Enrollment" and only examine the three school types of the study (i.e., Open Enrollment, Magnet Cluster and Tradititional Magnet)

``` r
data$Type=relevel(data$Type, ref="Open Enrollment")
data$Type2=relevel(data$Type2, ref="Open Enrollment")
data<- data %>% filter(Type2 == "Open Enrollment" | Type2 == "Magnet" | Type2 == "Magnet Cluster") # removes schools requiring testing, small, special ed and charter schools
```

Let's center variables and calculate the quadratic value, when required by the analysis.

``` r
data <- data %>% mutate(CPERCAPCRIME= PERCAPCRIME-mean(PERCAPCRIME,na.rm=T), CLOGPCT30MIN=log(PCT30MIN)-mean(log(PCT30MIN), na.rm=T), CPCTHOMEAGE40=PCTHOMEAGE40-mean(PCTHOMEAGE40,na.rm=T), CLOGMEDINCOME=log(MEDINCOME)-mean(log(MEDINCOME), na.rm=T), CREAD=ISAT.Exceeding.Reading...-mean(ISAT.Exceeding.Reading...,na.rm=T), CMATH=ISAT.Exceeding.Math..-mean(ISAT.Exceeding.Math..,na.rm=T), CBk=PCTBLACK - mean(PCTBLACK, na.rm=T), CMEDROOMS=MEDROOMS-mean(MEDROOMS, na.rm=T)) %>% mutate(CBk2=CBk*CBk, CMEDROOMS2=CMEDROOMS*CMEDROOMS)
```

Missing Data
------------

How much missingness?

``` r
miss_pct <- map_dbl(data, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
miss_pct <- miss_pct[miss_pct > 0]
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
ggplot(aes(x=reorder(var, -miss), y=miss)) +
geom_bar(stat='identity', fill='red') +
labs(x='', y='% missing', title='Percent missing data by feature') +
theme(axis.text.x=element_text(angle=90, hjust=1))
```

![](graphs/Missing%20data-1.png) Of 48 columns, 20 have missing values. The percentage of values missing ranges from 0.5% in PCTBLACK to 4.1% in MEDVALUE. Very little missing data. Listwise deletion will work fine.

Descriptive Statistics
======================

Correlations
------------

``` r
data %>% 
  select(MEDVALUE,CPERCAPCRIME,CMEDROOMS,CMEDROOMS2,CLOGMEDINCOME,CLOGPCT30MIN,CPCTHOMEAGE40,CBk,CBk2,CREAD,CMATH) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()
```

![](graphs/Descriptive%20Statistics-1.png) Variables seem to be correlated as expected.

Correlation between school types and educational quality.

``` r
data %>% mutate(Cluster=data$Type2=="Magnet Cluster", Magnet=data$Type2=="Magnet") %>% select(Read=ISAT.Exceeding.Reading...,Math=ISAT.Exceeding.Math.., Cluster, Magnet) %>% cor(use = "pairwise.complete.obs")
```

    ##               Read       Math     Cluster     Magnet
    ## Read    1.00000000 0.93181068  0.01262809  0.3541225
    ## Math    0.93181068 1.00000000  0.01710154  0.3341444
    ## Cluster 0.01262809 0.01710154  1.00000000 -0.1642822
    ## Magnet  0.35412255 0.33414443 -0.16428219  1.0000000

High correlation between magnet status and measures of educational quality. There is virtually no correlation of magnet cluster with reading or math scores.

Descriptive Statistics Table
----------------------------

``` r
data$Type2<-droplevels(data$Type2)
data$Type2<-factor(data$Type2, levels = c("Open Enrollment","Magnet Cluster","Magnet"))
Table<-data %>% 
  group_by(Type2) %>%
  select(MEDVALUE,PERCAPCRIME,MEDROOMS,MEDINCOME,PCT30MIN,PCTHOMEAGE40,PCTBLACK,ISAT.Exceeding.Reading...,ISAT.Exceeding.Math..) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  select(-Type2) %>% 
  t() %>%
  round(digits = 2)
Table<-rbind(table(data$Type2),Table)
colnames(Table) = c("Open Enrollment","Magnet Cluster","Magnet")
row.names(Table)<-c("n","Average Median Home Value","Crime Rate","Median # of Rooms","Median Income","Proportion Traveling >30 Minutes to Work","Proportion of Homes Built Before 1940","Proportion Black","Proportion Exceeding ISAT Reading","Proportion Exceeding ISAT Math")
kable(Table)
```

|                                             |  Open Enrollment|  Magnet Cluster|     Magnet|
|---------------------------------------------|----------------:|---------------:|----------:|
| n                                           |           274.00|           78.00|      38.00|
| Average Median Home Value                   |        209308.37|       252610.81|  321000.00|
| Crime Rate                                  |             0.15|            0.15|       0.13|
| Median \# of Rooms                          |             5.18|            4.97|       4.93|
| Median Income                               |         42673.44|        47789.10|   52777.42|
| Proportion Traveling &gt;30 Minutes to Work |             0.59|            0.61|       0.53|
| Proportion of Homes Built Before 1940       |             0.50|            0.52|       0.46|
| Proportion Black                            |             0.46|            0.45|       0.39|
| Proportion Exceeding ISAT Reading           |            12.81|           14.88|      27.29|
| Proportion Exceeding ISAT Math              |            17.11|           19.51|      32.77|

Analysis
--------

### Regression of Logged Median Neighborhood Home Value on Magnet, Covariates

``` r
mod0<-lm(log(MEDVALUE)~Magnet, data=data)
mod1<-lm(log(MEDVALUE)~Magnet+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2, data=data)
space<-rep(NA,8*4)
dim(space)<-c(8,4)
mod0<-rbind(summary(mod0)$coef,space)
Table2<-round(cbind(mod0,summary(mod1)$coef),3)
colnames(Table2)<-c("Mod1_Coef","Mod1_SE","Mod1_tstat","Mod1_pvalue","Mod2_Coef","Mod2_SE","Mod2_tstat","Mod2_pvalue")
rownames(Table2)<-c("Intercept", "Magnet (ref=Neighborhood Schools)", "Crime Rate","Median Number of Rooms","Median Number of Rooms^2", "log(Median Income)","log(Proportion Commuting > 30 min)","Proportion of Homes Build before 1940","Proportion Black","Proportion Black^2")
kable(Table2)
```

|                                       |  Mod1\_Coef|  Mod1\_SE|  Mod1\_tstat|  Mod1\_pvalue|  Mod2\_Coef|  Mod2\_SE|  Mod2\_tstat|  Mod2\_pvalue|
|---------------------------------------|-----------:|---------:|------------:|-------------:|-----------:|---------:|------------:|-------------:|
| Intercept                             |      12.153|     0.030|      411.283|             0|      12.093|     0.055|      218.646|         0.000|
| Magnet (ref=Neighborhood Schools)     |       0.213|     0.054|        3.935|             0|       0.102|     0.043|        2.386|         0.018|
| Crime Rate                            |          NA|        NA|           NA|            NA|      -0.477|     0.202|       -2.358|         0.019|
| Median Number of Rooms                |          NA|        NA|           NA|            NA|      -0.125|     0.027|       -4.584|         0.000|
| Median Number of Rooms^2              |          NA|        NA|           NA|            NA|       0.043|     0.015|        2.863|         0.004|
| log(Median Income)                    |          NA|        NA|           NA|            NA|       0.374|     0.052|        7.165|         0.000|
| log(Proportion Commuting &gt; 30 min) |          NA|        NA|           NA|            NA|      -0.386|     0.075|       -5.141|         0.000|
| Proportion of Homes Build before 1940 |          NA|        NA|           NA|            NA|       0.133|     0.087|        1.531|         0.127|
| Proportion Black                      |          NA|        NA|           NA|            NA|      -0.200|     0.070|       -2.842|         0.005|
| Proportion Black^2                    |          NA|        NA|           NA|            NA|       0.275|     0.264|        1.038|         0.300|

``` r
write.csv(Table2, file.path(Graph_Directory, "Models_1_2_Table.csv"))
```

### Regression of Logged Median Neighborhood Home Value on School Typologies and Covariates

``` r
mod2<-lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2, data=data)
mod3<-lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2+CREAD+CMATH, data=data)
space<-rep(NA,2*4)
dim(space)<-c(2,4)
mod2<-rbind(summary(mod2)$coef,space)
Table3<-round(cbind(mod2,summary(mod3)$coef),3)
colnames(Table3)<-c("Mod3_Coef","Mod3_SE","Mod3_tstat","Mod3_pvalue","Mod4_Coef","Mod4_SE","Mod4_tstat","Mod4_pvalue")
rownames(Table3)<-c("Intercept", "Magnet Cluster (ref=Neighborhood Schools)", "Magnet","Crime Rate","Median Number of Rooms","Median Number of Rooms^2", "log(Median Income)","log(Proportion Commuting > 30 min)","Proportion of Homes Build before 1940","Proportion Black","Proportion Black^2","ISAT Reading","ISAT Math")
kable(Table3)
```

|                                           |  Mod3\_Coef|  Mod3\_SE|  Mod3\_tstat|  Mod3\_pvalue|  Mod4\_Coef|  Mod4\_SE|  Mod4\_tstat|  Mod4\_pvalue|
|-------------------------------------------|-----------:|---------:|------------:|-------------:|-----------:|---------:|------------:|-------------:|
| Intercept                                 |      12.090|     0.055|      219.066|         0.000|      12.142|     0.056|      218.509|         0.000|
| Magnet Cluster (ref=Neighborhood Schools) |       0.059|     0.049|        1.207|         0.228|       0.049|     0.047|        1.033|         0.302|
| Magnet                                    |       0.188|     0.065|        2.876|         0.004|       0.022|     0.069|        0.317|         0.752|
| Crime Rate                                |      -0.480|     0.202|       -2.376|         0.018|      -0.430|     0.196|       -2.199|         0.029|
| Median Number of Rooms                    |      -0.124|     0.027|       -4.576|         0.000|      -0.115|     0.026|       -4.374|         0.000|
| Median Number of Rooms^2                  |       0.044|     0.015|        2.885|         0.004|       0.030|     0.015|        2.062|         0.040|
| log(Median Income)                        |       0.376|     0.052|        7.229|         0.000|       0.278|     0.053|        5.227|         0.000|
| log(Proportion Commuting &gt; 30 min)     |      -0.370|     0.075|       -4.903|         0.000|      -0.351|     0.073|       -4.783|         0.000|
| Proportion of Homes Build before 1940     |       0.145|     0.087|        1.679|         0.094|       0.212|     0.085|        2.499|         0.013|
| Proportion Black                          |      -0.199|     0.070|       -2.830|         0.005|      -0.130|     0.073|       -1.784|         0.075|
| Proportion Black^2                        |       0.291|     0.264|        1.101|         0.271|       0.144|     0.263|        0.547|         0.585|
| ISAT Reading                              |          NA|        NA|           NA|            NA|       0.016|     0.005|        3.538|         0.000|
| ISAT Math                                 |          NA|        NA|           NA|            NA|      -0.004|     0.004|       -1.100|         0.272|

``` r
write.csv(Table3, file.path(Graph_Directory, "Models_3_4_Table.csv"))
```

Model Diagnostics
-----------------

``` r
source(file.path(Diagnostics_Directory,"diagnostics.R"))
```

    ## [1] "Outlier and Influential Observations Diagnostics"

![](graphs/diagnostics-1.png)![](graphs/diagnostics-2.png)![](graphs/diagnostics-3.png)![](graphs/diagnostics-4.png)![](graphs/diagnostics-5.png)![](graphs/diagnostics-6.png)

    ## [1] "Non-normality Diagnostics"

![](graphs/diagnostics-7.png)

    ## [1] "Homoscedasticity Diagnostics"

![](graphs/diagnostics-8.png)

    ## [1] "Multicollinearity Diagnostics"
    ##                   GVIF Df GVIF^(1/(2*Df))
    ## Type2         1.264852  2        1.060498
    ## CPERCAPCRIME  1.856182  1        1.362418
    ## CMEDROOMS     1.392380  1        1.179992
    ## CMEDROOMS2    1.144730  1        1.069921
    ## CLOGMEDINCOME 2.137745  1        1.462103
    ## CLOGPCT30MIN  1.099742  1        1.048686
    ## CPCTHOMEAGE40 1.279246  1        1.131038
    ## CBk           2.945106  1        1.716131
    ## CBk2          1.472917  1        1.213638
    ## CREAD         8.502768  1        2.915951
    ## CMATH         8.227938  1        2.868438
    ## [1] "Nonlinearity Diagnostics"

![](graphs/diagnostics-9.png)![](graphs/diagnostics-10.png)

    ## [1] "Test for Autocorrelated Errors"
    ##  lag Autocorrelation D-W Statistic p-value
    ##    1     -0.05313961      2.098962    0.43
    ##  Alternative hypothesis: rho != 0

Sensitivity Analysis
--------------------

### Propensity Score Analysis

#### Propensity scores

``` r
#data$NEIGHBORHOOD<-0; data$NEIGHBORHOOD[data$Type2=="Open Enrollment"]<-1
#data$CLUSTER<-0; data$CLUSTER[data$Type2=="Magnet Cluster"]<-1
data$TRAD_MAGNET<-0; data$TRAD_MAGNET[data$Type2=="Magnet"]<-1
m_ps <- glm(TRAD_MAGNET ~ CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2+CREAD+CMATH, family = binomial(), data = data)
prs_df<-data.frame(pr_score = predict(m_ps, type = "response"), TRAD_MAGNET = m_ps$model$TRAD_MAGNET)
```

#### Common Support

``` r
labs <- paste("Actual school type attended:", c("Traditional Magnet", "Other"))
prs_df %>%
  mutate(magnet = ifelse(TRAD_MAGNET == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~magnet) +
  xlab("Probability of going to a Traditional Magnet") +
  theme_bw()
```

![](graphs/unnamed-chunk-6-1.png)

#### Match It

``` r
data_cov<-c('CPERCAPCRIME','CMEDROOMS','CMEDROOMS2','CLOGMEDINCOME','CLOGPCT30MIN','CPCTHOMEAGE40','CBk','CBk2','CREAD','CMATH')
data_nomiss <- data %>%  # MatchIt does not allow missing values
  select(MEDVALUE, TRAD_MAGNET, one_of(data_cov)) %>%
  na.omit()

mod_match <- matchit(TRAD_MAGNET ~ CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2+CREAD+CMATH, method = "nearest", data = data_nomiss)

dta_m <- match.data(mod_match)
dim(dta_m)
```

    ## [1] 70 14

#### Visual Inspection

``` r
library(gridExtra)
grid.arrange(
   fn_bal(dta_m, "CPERCAPCRIME"),
   fn_bal(dta_m, "CMEDROOMS") + theme(legend.position = "none"),
   fn_bal(dta_m, "CLOGMEDINCOME"),
   fn_bal(dta_m, "CLOGPCT30MIN") + theme(legend.position = "none"),
   fn_bal(dta_m, "CPCTHOMEAGE40"),
   fn_bal(dta_m, "CBk") + theme(legend.position = "none"),
   fn_bal(dta_m, "CREAD"),
   fn_bal(dta_m, "CMATH") + theme(legend.position = "none"),
   nrow = 5, widths = c(1, 0.8)
)
```

    ## Warning: Removed 36 rows containing missing values (geom_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_smooth).

![](graphs/unnamed-chunk-8-1.png)

#### Balance

``` r
dta_m %>%
  group_by(TRAD_MAGNET) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean))
```

    ## # A tibble: 2 x 11
    ##   TRAD_MAGNET CPERCAPCRIME   CMEDROOMS CMEDROOMS2 CLOGMEDINCOME
    ##         <dbl>        <dbl>       <dbl>      <dbl>         <dbl>
    ## 1           0  0.007096608 -0.06822533  1.2996751     0.2355267
    ## 2           1 -0.012764511 -0.13679676  0.8983378     0.1428872
    ## # ... with 6 more variables: CLOGPCT30MIN <dbl>, CPCTHOMEAGE40 <dbl>,
    ## #   CBk <dbl>, CBk2 <dbl>, CREAD <dbl>, CMATH <dbl>

#### Statistical Test of Treatment

``` r
lm_treat <- lm(log(MEDVALUE) ~ TRAD_MAGNET, data = dta_m)
summary(lm_treat)
```

    ## 
    ## Call:
    ## lm(formula = log(MEDVALUE) ~ TRAD_MAGNET, data = dta_m)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.29996 -0.41643 -0.02247  0.54430  1.34302 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.50908    0.09806 127.561   <2e-16 ***
    ## TRAD_MAGNET -0.03658    0.13868  -0.264    0.793    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5802 on 68 degrees of freedom
    ## Multiple R-squared:  0.001022,   Adjusted R-squared:  -0.01367 
    ## F-statistic: 0.06959 on 1 and 68 DF,  p-value: 0.7927

Other Exploratory Analysis
--------------------------

``` r
mod=lm(log(MEDVALUE)~Specialty+PERCAPCRIME+CBk+CBk2+log(PCT30MIN)+CMEDROOMS+CMEDROOMS2+PCTHOMEAGE40+log(MEDINCOME), data=data)

# When Interaction Term is included
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2+CREAD+CREAD*Type2, data=data)
mod=lm(log(MEDVALUE)~Type2+CPERCAPCRIME+CMEDROOMS+CMEDROOMS2+CLOGMEDINCOME+CLOGPCT30MIN+CPCTHOMEAGE40+CBk+CBk2+CMATH+CMATH*Type2, data=data)
```
