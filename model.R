library(raster)
library(e1071)
library(dplyr)
load("data.Rdata")
load("NotNYC.Rdata")

names(data) = c("Borough","long","lat")

data1 <- sample_frac(data, 0.1)


sample$new <- sample$x
sample$x <- NULL
names(sample) <- c("Borough","long","lat")
sample$Borough <- rep("XNYC", times = nrow(sample))
data2 <- rbind(data1,sample)
data2$long <- as.numeric(data2$long)
data2$lat <- as.numeric(data2$lat)
data2 <- na.omit(data2)
data2$Borough <- as.factor(data2$Borough)


l<-svm(Borough ~ .,
       data2,
       kernel = "radial")
dims = trunc(sqrt(nrow(data2))) + 1
boundary <- sapply(data2[,-1],range)
r = raster(nrows = dims, ncols = dims,
           xmn = boundary[,"long"][1] - 0.01, xmx = boundary[,"long"][2] + 0.01,
           ymn = boundary[,"lat"][1] - 0.01, ymx = boundary[,"lat"][2] + 0.01)
r[] = NA

pred_locs = data.frame(xyFromCell(r,1:dims^2))
colnames(pred_locs) <- c("long", "lat")
pred = predict(l,pred_locs)
r[] = pred
plot(r)
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
