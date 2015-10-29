library(dplyr)
library(data.table)
library(magrittr)
library(stringr)
nyc = fread("/home/vis/cr173/Sta523/data/nyc/nyc_311.csv") %>% tbl_df()
nyc.important <- nyc[,c("Unique.Key",
                        "Complaint.Type",
                        "Descriptor",
                        "Incident.Zip",
                        "Incident.Address",
                        "Street.Name",
                        "Cross.Street.1",
                        "Cross.Street.2",
                        "Intersection.Street.1",
                        "Intersection.Street.2",
                        "Address.Type",
                        "City",
                        "Resolution.Description", "Borough")]
nyc1 <- filter(nyc.important, 
               Cross.Street.1 != "" & Cross.Street.2 != "")
# both Cross Street Columns are filled #
nyc1.1 <- filter(nyc1, 
                 Intersection.Street.1 == "" & Intersection.Street.2 == "")
# both Intersection Street Columns are empty #
nyc2 <- filter(nyc.important, 
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
new_indices <- which(str_detect(tolower(unique(nyc$City)),
                                "new"))
# unique() finds all the names that appear in the City Column # 
# change everything to lower cases # 
# find "new" in any of the names # 
# find the indices for names containing "new" inside vector unique() #
york_indices <- which(str_detect(tolower(unique(nyc$City)[new_indices]),
                                 "york"))
# find the indices for names containing "york" inside vector unique()[new]#
yok_indices <- which(str_detect(tolower(unique(nyc$City)[new_indices]),
                                "yok"))
# find the indices for names containing "yok" inside vector unique()[new]#
names.okay <- c(unique(nyc$City)[new_indices][york_indices],
                unique(nyc$City)[new_indices][yok_indices] )
# make "new" + "york" and "new" + "yok" into one vector # 
nyc6 <- filter(nyc5, City %in% names.okay)
# filter data points by City column so that all names in City Column # 
# is in names.okay vector # 
nyc7 <- filter(nyc6, Address.Type != "BLOCKFACE")
# remove all rows where Address.Type column is "BLOCKFACE" #
nyc7.1 <- filter(nyc7, Address.Type != "")
# remove all rows where Address.Type column is empty # 
transform <- function(v){
  v <- str_replace_all(v,"AVENUE","AVE") #ATTENTION
  v <- str_replace_all(v,"STREET","ST")
  v <- str_replace_all(v,"DRIVE","DR")
  v <- str_replace_all(v,"BOULEVARD","BLVD")
  v <- str_replace_all(v,"ROAD","RD")
  v <- str_replace_all(v,"PLACE","PL")
  v <- str_replace_all(v,"EXPRESSWAY","EXPY")
  v <- str_replace_all(v,"PARKWAY","PKWY")
  v <- str_replace_all(v,"FREEWAY","FWY")
  v <- str_replace_all(v,"CRESCENT","CRES")
  v <- str_replace_all(v,"COURT","CT")
  v <- str_replace_all(v,"LANE","LN")
  v <- str_replace_all(v,"EAST "," E ")
  v <- str_replace_all(v,"WEST ","W ")
  v <- str_replace_all(v,"SOUTH ","S ")
  v <- str_replace_all(v,"NORTH "," N ")
  v <- str_replace_all(v," EAST"," E")
  v <- str_replace_all(v," WEST"," W")
  v <- str_replace_all(v," SOUTH"," S")
  v <- str_replace_all(v," NORTH"," N")
  v <- str_replace_all(v,".* STREET WEST","WEST STREET .*")
  return(v)
}
load("~cr173/Sta523/data/nyc/intersections/intersections.Rdata")
save(, file = "geocode.Rdata")