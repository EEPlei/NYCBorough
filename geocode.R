library(rgdal)
library(stringr)
library(magrittr)
library(dplyr)
library(rgeos)
library(foreign) 
library(data.table)
library(RSQLite)

base_path <- "~cr173/Sta523/data/nyc/"
database <- dbConnect(drv=RSQLite::SQLite(), dbname="~cr173/Sta523/data/nyc/nyc_311_index.sqlite")
tables <- dbListTables(database)
lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=database, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

nyc <- lDataFrames[[1]] 
nyc.important <- nyc[,c("Incident.Zip",
                        "Incident.Address",
                        "Street.Name",
                        "Cross.Street.1",
                        "Cross.Street.2",
                        "Intersection.Street.1",
                        "Intersection.Street.2",
                        "Address.Type",
                        "City",
                        "Borough")]
nyc.na <- na.omit(nyc.important)
nyc1 <- filter(nyc.na, 
               Cross.Street.1 != "" & Cross.Street.2 != "")
# both Cross Street Columns are filled #
nyc1.1 <- filter(nyc1, 
                 Intersection.Street.1 == "" & Intersection.Street.2 == "")
# both Intersection Street Columns are empty #
nyc2 <- filter(nyc.na, 
               Intersection.Street.1 != "" & Intersection.Street.2 != "")
# both Intersection Street Columns are filled # 
nyc2.1 <- filter(nyc2, 
                 Cross.Street.1 == "" & Cross.Street.2 == "")
# both Cross Street Columns are empty # 
nyc3 <- bind_rows(nyc1.1, nyc2.1)
# combine two data frames by rows. so stack nyc1.1 on nyc2.1 # 
# nrow(nyc1.1) + nrow(nyc2.1) X ncol(nyc1.1)|ncol(nyc2.1) #
filter1 <- (nyc3$Intersection.Street.1 != "" & 
              nyc3$Intersection.Street.2 != "" & 
              nyc3$Cross.Street.1 != "" & 
              nyc3$Cross.Street.2 != "")
# All four columns are empty # 
nyc4 <- filter(nyc3, !filter1)
# keep any of the data where at least one of the columns are filled # 
# the exact opposite of 'All four columns are empty # 
nyc5 <- filter(nyc4, Borough != "Unspecified")
# throw away data where the Borough information is "Unspecified" #
#new_indices <- which(str_detect(tolower(unique(nyc$City)),
#                               "new"))
# unique() finds all the names that appear in the City Column # 
# change everything to lower cases # 
# find "new" in any of the names # 
# find the indices for names containing "new" inside vector unique() #
#york_indices <- which(str_detect(tolower(unique(nyc$City)[new_indices]),
#                                "york"))
# find the indices for names containing "york" inside vector unique()[new]#
#yok_indices <- which(str_detect(tolower(unique(nyc$City)[new_indices]),
#                               "yok"))
# find the indices for names containing "yok" inside vector unique()[new]#
#names.okay <- c(unique(nyc$City)[new_indices][york_indices],
#               unique(nyc$City)[new_indices][yok_indices])
# make "new" + "york" and "new" + "yok" into one vector # 
#names.okay <- names.okay[c(1,2,5,6,10,11,13,14,15,16)]
# some are not part of New York City, subset out # 
#nyc6 <- filter(nyc5, City %in% names.okay)
# filter data points by City column so that all names in City Column # 
# is in names.okay vector # 
#nyc7 <- filter(nyc6, Address.Type != "BLOCKFACE")
nyc7 <- filter(nyc5, Address.Type != "BLOCKFACE")
# remove all rows where Address.Type column is "BLOCKFACE" #
nyc7.1 <- filter(nyc7, Address.Type != "")
# remove all rows where Address.Type column is empty # 
nyc7.2 <- filter(nyc7.1, Address.Type != "PLACENAME")
# remove all rows where Address.Type column is empty # 
#set.seed(1)
#nyc7.3 <- sample_frac(nyc7.2,0.5)


#load intersections

inter = readOGR(path.expand("~cr173/Sta523/data/nyc/intersections/"),
                "intersections",stringsAsFactors = FALSE)
inter = inter[!is.na(inter@data$streets),] %>%
  .[str_detect(.@data$streets,":"),] %>%
  .[!str_detect(.@data$streets,":.*:"),]

inter_data = inter@data %>%
  cbind(., coordinates(inter)) %>%
  cbind(., str_split_fixed(.$streets,":",2))

inter_data$id <- NULL
colnames(inter_data) = c("Streets","Longitude","Latitude","Street1","Street2")
inter_data <- inter_data %>% select(-Streets)

#extract intersections data from nyc7.2 to merge with inter_data

nyc_inter1 <- nyc7.2 %>% 
  select(Cross.Street.1,Cross.Street.2,Borough) 
names(nyc_inter1) <- c("Street1","Street2","borough")

nyc_inter2 <- nyc7.2 %>% 
  select(Intersection.Street.1,Intersection.Street.2,Borough)

names(nyc_inter2) <- c("Street1","Street2","borough")

nyc_inter <- rbind(nyc_inter1,nyc_inter2)
nyc_inter <- unique(nyc_inter)
#merge nyc_inter with inter_data

transform = function(v){
  v = str_replace_all(str_c(v), 
                      c("AVENUE"="AVE", "STREET"="ST", "DRIVE"="DR", 
                        "BOULEVARD"="BLVD", "ROAD"="RD", "PLACE"="PL", 
                        "EXPRESSWAY"="EXPY", "PARKWAY"="PKWY", 
                        "FREEWAY"="FWY", "CRESCENT"="CRES", "COURT"="CT",
                        "LANE"="LN", "EAST"="E", "WEST"="W", "SOUTH "="S",
                        "NORTH"="N"))
}

nyc_inter$Street1 <- lapply(nyc_inter$Street1, transform)
nyc_inter$Street2 <- lapply(nyc_inter$Street2, transform)
inter_data$Street1 <- lapply(inter_data$Street1, transform)
inter_data$Street2 <- lapply(inter_data$Street2, transform)

res1 <- merge(nyc_inter,inter_data)
names(inter_data) <- c("Longitude","Latitude","Street2","Street1")
res2 <- merge(nyc_inter, inter_data)
res_inter <- rbind(res1, res2)

save(res_inter, file = "res_inter.Rdata")

#load pluto

load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")


#extract pluto data from nyc7.1 to merge with pluto

nyc_pluto <- nyc7.2 %>% 
  filter(Address.Type == "ADDRESS") %>%
  select(contains("Incident"),Borough) %>%
  filter(Borough != "Unspecified")
names(nyc_pluto) <- c("ZipCode","Address","Borough")

#merge nyc_pluto with pluto

res_pluto <- merge(nyc_pluto,pluto, by = "Address")
res_pluto <- res_pluto %>% select(Address, ZipCode.x, Borough.x, x, y)
names(res_pluto) <- c("Address", "ZipCode","borough","Longitude","Latitude")

save(res_pluto, file = "res_pluto.Rdata")

#conbine res_pluto with res_inter
res_pluto <- res_pluto %>% select(borough, Longitude, Latitude)
res_inter <- res_inter %>% select(borough, Longitude, Latitude)

data <- rbind(res_pluto,res_inter)
data <- unique(data)
save(data, file = "data.Rdata")

