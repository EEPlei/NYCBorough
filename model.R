library(raster)
library(e1071)
library(dplyr)
load("data.Rdata")

#change column names in our cleaned data file
names(data) = c("Borough","long","lat")
#sample 4% of the data. Discovered that 4% sample optimized performance and efficiency through sensitivity analysis
data1 <- sample_frac(data, 0.04)

#set the number of random samples  drawn from a raster rectangle
N <- 20000
sample <- data.frame(x=runif(N,-74.3,-73.67),y=runif(N,40.5,40.91))
#create column of "N"s
sample$NYC <- rep("N",N)
#rename sample columns
names(sample) <- c("long", "lat", "borough")
##data below refers to the data we use for svm; colnames longitude and latitude 
#1 latitude = 111111 meters so 200 meters is roughly = 200 * 1/111111 degree of latitude
md200 <- 200 * 1/111111
that <- sample_frac(data1, 0.4)
#function to create fake data
notNYC <- function(this){
  v <- that[that$long < this[1] + md200,]
  if(dim(v)[1] == 0)
    return ("N")
  
  v <- v[v$long > this[1] - md200,]
  if(dim(v)[1] == 0)
    return ("N")
  
  v <- v[v$lat < this[2] + md200,]
  if(dim(v)[1] == 0)
    return ("N")
  
  v <- v[v$lat > this[2] - md200,]
  if(dim(v)[1] == 0)
    return ("N")
  
  else
    return("Y")
}

#apply function to all the randomly sampled data
sample$NYC <- unlist(apply(sample[,1:2], 1, notNYC))

#filter only the non-NYC data points
Not_NYC <- filter(sample,NYC == "N")
Not_NYC$borough <- NULL
Not_NYC1 <- cbind(Not_NYC[,"NYC"], Not_NYC[,c("long", "lat")])
colnames(Not_NYC1) <- c("Borough","long","lat")
Not_NYC1$Borough <- rep("XNYC", nrow(Not_NYC1))

#combine the sampled clean data and non-NYC data 
data2 <- rbind(data1,Not_NYC1)
data2$long <- as.numeric(data2$long)
data2$lat <- as.numeric(data2$lat)
#drop any "NAs" that were created
data2 <- na.omit(data2)
#change to a factor variable for SVM to run 
data2$Borough <- as.factor(data2$Borough)

#implement SVM 
l<-svm(Borough ~ .,
       data2,
       kernel = "radial")
#automate raster rectangle construction
#take square root of our combined data, throw out decimals and add integer 1 
#to end up with closest integer to the square root
dims = trunc(sqrt(nrow(data2))) + 1
boundary <- sapply(data2[,-1],range)
#specify xmin, xmax, ymin, and ymax
r = raster(nrows = dims, ncols = dims,
           xmn = boundary[,"long"][1] - 0.01, xmx = boundary[,"long"][2] + 0.01,
           ymn = boundary[,"lat"][1] - 0.01, ymx = boundary[,"lat"][2] + 0.01)
r[] = NA

pred_locs = data.frame(xyFromCell(r,1:dims^2))
colnames(pred_locs) <- c("long", "lat")
pred = predict(l,pred_locs)
r[] = pred
colors = c("red", "blue", "yellow", "green", "purple","white")
plot(r, col = colors)
points(data$long,data$lat,pch=16,cex=0.1)


## Create Polygons
short_to_long = c("BROOKLYN"="Brooklyn", 
                  "BRONX"="Bronx",
                  "MANHATTAN"="Manhattan",
                  "QUEENS"="Queens",
                  "STATEN ISLAND"="Staten Island")
poly = rasterToPolygons(r,dissolve=TRUE)
poly = poly[1:5, ]
names(poly@data) = "Name"
poly@data$Name = short_to_long[levels(pred)[1:5]]

source("write_geojson.R")
write_geojson(poly,"boroughs.json")

