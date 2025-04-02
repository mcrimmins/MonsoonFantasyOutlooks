# download latest CPC outlooks and summarize for AOI
# MAC 5/21/24
# adapted from getCPC.R /microApps

# load libraries
library(magrittr)
library(knitr)

# use metaOut for point lat/lons
#load("C:/Users/Crimmins/OneDrive - University of Arizona/RProjects/MonsoonFantasy/stationMeta.Rdata")
# dput(metaOut)

# metaOut included in script

metaOut<-structure(list(state = c("AZ", "AZ", "AZ", "NM", "TX"), elev = c(6999, 
                  1113, 2551, 5310, 3944), name = c("FLAGSTAFF AP", "PHOENIX AIRPORT", 
                  "TUCSON INTERNATIONAL AIRPORT", "ALBUQUERQUE INTL AP", "EL PASO INTL AP"
                  ), lon = c(-111.66637, -112.00365, -110.95638, -106.61545, -106.37737
                  ), lat = c(35.14427, 33.4278, 32.13153, 35.04189, 31.81234)), class = "data.frame", row.names = c(NA,5L))
# create sf object
metaOut <- sf::st_as_sf(metaOut, coords = c("lon","lat"))

####### 
# helper function
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
#

# set filenames
fileNames1<-c("monthupd_prcp_latest.zip",
              "seasprcp_latest.zip")
fileNames2<-c("610prcp_latest.zip",
              "814prcp_latest.zip")


# create lookup tables
cpcProbs<-rbind.data.frame( c(33,"33-40%"),
                            c(40,"40-50%"),
                            c(50,"50-60%"),
                            c(60,"60-70%"),
                            c(70,"70-80%"),
                            c(80,"80-90%"),
                            c(90,"90-100%"),
                            stringsAsFactors = FALSE)
colnames(cpcProbs)<-c("Prob","label")

cpcProbs34<-rbind.data.frame( c(50,"50-55%"),
                            c(55,"55-60%"),
                            c(60,"60-70%"),
                            c(70,"70-80%"),
                            c(80,"80-90%"),
                            c(90,"90-100%"),
                            stringsAsFactors = FALSE)
colnames(cpcProbs34)<-c("Prob","label")

cpcCond<-rbind.data.frame(
                          c("precipitation","Above","wetter-than-average"),
                          c("precipitation","Normal","near-average"),
                          c("precipitation","Below","drier-than-average"),
                          c("precipitation","EC","equal-chances"),
                          stringsAsFactors = FALSE)
colnames(cpcCond)<-c("var","Cat","label")

seasLabs<-rbind.data.frame(c("JFM","January-March"),
                           c("FMA","February-April"),
                           c("MAM","March-May"),
                           c("AMJ","April-June"),
                           c("MJJ","May-July"),
                           c("JJA","June-August"),
                           c("JAS","July-September"),
                           c("ASO","August-October"),
                           c("SON","September-November"),
                           c("OND","October-December"),
                           c("NDJ","November-January"),
                           c("DJF","December-February"),
                           stringsAsFactors = FALSE)
colnames(seasLabs)<-c("abb","fullMonths")

monthLabs<-cbind.data.frame(month.abb,month.name,
                            stringsAsFactors = FALSE)
colnames(monthLabs)<-c("abb","name")

fcstType<-cbind.data.frame(c("monthupd_prcp_latest.zip",
                           "seasprcp_latest.zip",
                           "610prcp_latest.zip",
                           "814prcp_latest.zip",
                           "wk34prcp_latest.zip"),
                           c("1-month",
                           "3-month",
                           "6-10 day",
                           "8-14 day",
                           "Week 3-4"),
                           stringsAsFactors = FALSE)
colnames(fcstType)<-c("filename","FcstType")

######

##### Seasonal Outlooks
# loop through file names
outListSeas<-list()  
for(i in 1:length(fileNames1)){
  # Create temp files
  temp <- tempfile()
  temp2 <- tempfile()
  # download files
  download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/",fileNames1[i]),
                destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  # read shapefile
  if( length(grep("seas",fileNames1[i]))==0){
    #outlook<-rgdal::readOGR(temp2)
    #outlook<- as(sf::st_read(temp2),"Spatial")
    outlook<- sf::st_read(temp2)
  }else{
    #outlook<-rgdal::readOGR(temp2,
    #                        rgdal::ogrListLayers(temp2)[grepl('lead1_', rgdal::ogrListLayers(temp2))])
    lyrs<-sf::st_layers(temp2)
    #outlook<-as(sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)]),"Spatial")
    outlook<-sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)])
  }

# intersect station points
  metaOut<-sf::st_set_crs(metaOut, sf::st_crs(outlook))
  outlook <- sf::st_intersection(metaOut, outlook)
  
  # remove factors
  outlook$Valid_Seas<-as.character(outlook$Valid_Seas)
  outlook$Cat<-as.character(outlook$Cat)
  
# get data from table, add labels
# outlook<-outlook@data[which.max(raster::area(outlook)),c("Fcst_Date","Valid_Seas","Prob","Cat")]
#  outlook$type<-ifelse(length(grep("seas",fileNames1[i]))==0,"1-month","3-month")
  outlook$type<- fcstType$FcstType[which(fileNames1[i]==fcstType$filename)]
  outlook$var<-ifelse(length(grep("prcp",fileNames1[i]))==0,"temperature","precipitation")
  #outlook$probLab<-cpcProbs[which(as.numeric(cpcProbs$prob)==outlook$Prob),2]
  outlook<-merge(outlook, cpcProbs, by="Prob")
  outlook$catLab<-ifelse(outlook$Cat=="EC", outlook$Cat,
    ifelse(outlook$Prob>=50, paste0("Likely ",outlook$Cat),paste0("Leaning ",outlook$Cat)))
  #outlook$condLab<-cpcCond$label[which(outlook$var==cpcCond$var & outlook$Cat==cpcCond$cat)]
  outlook<-merge(outlook, cpcCond[,c(2,3)], by="Cat")
  outlook$label.x<-ifelse(outlook$catLab=="EC", "33%-33%-33%",outlook$label.x)
  
  # if(outlook$type[1]=="1-month"){
  #   outlook<-merge(outlook, monthLabs, by.x="Valid_Seas",by.y="abb")
  # }else{
  #   outlook<-merge(outlook, seasLabs, by.x="Valid_Seas", by.y="abb")
  # }
  
  # put df in list
  outListSeas[[i]]<-outlook
}
######

##### Weekly Outlooks
# loop through file names
outListWeek<-list()  
for(i in 1:length(fileNames2)){
  # Create temp files
  temp <- tempfile()
  temp2 <- tempfile()
  # download files
  download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/",fileNames2[i]),
                destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  # read shapefile
  if( length(grep("seas",fileNames2[i]))==0){
    #outlook<-rgdal::readOGR(temp2)
    #outlook<- as(sf::st_read(temp2),"Spatial")
    outlook<- sf::st_read(temp2)
  }else{
    #outlook<-rgdal::readOGR(temp2,
    #                        rgdal::ogrListLayers(temp2)[grepl('lead1_', rgdal::ogrListLayers(temp2))])
    lyrs<-sf::st_layers(temp2)
    #outlook<-as(sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)]),"Spatial")
    outlook<-sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)])
  }
  
  # intersect station points
  metaOut<-sf::st_set_crs(metaOut, sf::st_crs(outlook))
  #outlook <- sf::st_intersection(metaOut, outlook)
  outlook <- sf::st_join(metaOut, outlook, left = TRUE)
  
  # remove factors
  outlook$Cat<-as.character(outlook$Cat)
  
  # fill in NAs
  outlook$Fcst_Date<-min(outlook$Fcst_Date, na.rm = TRUE)
  outlook$Start_Date<-min(outlook$Start_Date, na.rm = TRUE)  
  outlook$End_Date<-min(outlook$End_Date, na.rm = TRUE)
  outlook$Prob<-ifelse(is.na(outlook$Prob), 33, outlook$Prob)
  outlook$Cat<-ifelse(is.na(outlook$Cat), "Normal", outlook$Cat)

  # change normal to EC
  outlook$Cat<-ifelse(outlook$Cat=="Normal","EC",outlook$Cat)
  outlook$Prob<-ifelse(outlook$Cat=="EC",33,outlook$Prob)
      
  # add supporting data, labels
  #outlook$type<-fileNames2[i]
  outlook$type<- fcstType$FcstType[which(fileNames2[i]==fcstType$filename)]
  outlook$var<-"precipitation"
  outlook<-merge(outlook, cpcProbs, by="Prob", all.x=TRUE)
  outlook$catLab<-ifelse(outlook$Cat=="EC", outlook$Cat,
                         ifelse(outlook$Prob>=50, paste0("Likely ",outlook$Cat),paste0("Leaning ",outlook$Cat)))
  outlook<-merge(outlook, cpcCond[,c(2,3)], by="Cat", all.x=TRUE)
  outlook$label.x<-ifelse(outlook$catLab=="EC", "33%-33%-33%",outlook$label.x)
  
  # if(outlook$type[1]=="1-month"){
  #   outlook<-merge(outlook, monthLabs, by.x="Valid_Seas",by.y="abb")
  # }else{
  #   outlook<-merge(outlook, seasLabs, by.x="Valid_Seas", by.y="abb")
  # }
  
  # valid seas
  outlook$Valid_Seas<-paste0(format(outlook$Start_Date, "%b-%d-%Y")," to ",format(outlook$End_Date, "%b-%d-%Y"))
  
  # put df in list
  outListWeek[[i]]<-outlook
}
######

##### week 3-4 outlook
#"wk34prcp_latest.zip"

fileNames3<-"wk34prcp_latest.zip"
  # Create temp files
  temp <- tempfile()
  temp2 <- tempfile()
  # download files
  download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/",fileNames3),
                destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  # read shapefile
  if( length(grep("seas",fileNames3))==0){
    #outlook<-rgdal::readOGR(temp2)
    #outlook<- as(sf::st_read(temp2),"Spatial")
    outlook<- sf::st_read(temp2)
  }else{
    #outlook<-rgdal::readOGR(temp2,
    #                        rgdal::ogrListLayers(temp2)[grepl('lead1_', rgdal::ogrListLayers(temp2))])
    lyrs<-sf::st_layers(temp2)
    #outlook<-as(sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)]),"Spatial")
    outlook<-sf::st_read(temp2,layer=lyrs$name[which(grepl('lead1_', lyrs$name)==TRUE)])
  }
  
  # intersect station points
  metaOut<-sf::st_set_crs(metaOut, sf::st_crs(outlook))
  #outlook <- sf::st_intersection(metaOut, outlook)
  outlook <- sf::st_join(metaOut, outlook, left = TRUE)
  
  # remove factors
  outlook$Cat<-as.character(outlook$Cat)
  
  # fill in NAs
  outlook$Fcst_Date<-min(outlook$Fcst_Date, na.rm = TRUE)
  outlook$Start_Date<-min(outlook$Start_Date, na.rm = TRUE)  
  outlook$End_Date<-min(outlook$End_Date, na.rm = TRUE)
  outlook$Prob<-ifelse(is.na(outlook$Prob), 50, outlook$Prob)
  outlook$Cat<-ifelse(is.na(outlook$Cat), "Normal", outlook$Cat)
  
  # change normal to EC
  outlook$Cat<-ifelse(outlook$Cat=="Normal","EC",outlook$Cat)
  outlook$Prob<-ifelse(outlook$Cat=="EC",50,outlook$Prob)
  
  # add supporting data, labels
  #outlook$type<-fileNames2[i]
  outlook$type<- fcstType$FcstType[which(fileNames3==fcstType$filename)]
  outlook$var<-"precipitation"
  outlook<-merge(outlook, cpcProbs, by="Prob", all.x=TRUE)
  #outlook$catLab<-ifelse(outlook$Cat=="EC", outlook$Cat,
  #                       ifelse(outlook$Prob>=50, paste0("Likely ",outlook$Cat),paste0("Leaning ",outlook$Cat)))
  outlook<-merge(outlook, cpcCond[,c(2,3)], by="Cat", all.x=TRUE)
  outlook$label.x<-ifelse(outlook$Cat=="EC", "50%-50%",outlook$label.x)
  
  # valid seas
  outlook$Valid_Seas<-paste0(format(outlook$Start_Date, "%b-%d-%Y")," to ",format(outlook$End_Date, "%b-%d-%Y"))
  
  # put df in list
  outlook34<-outlook
#####

#####
# assemble forecasts by city
cities<-outListWeek[[1]]$name
cityList<-list()
  
for(i in 1:length(cities)){
  cityList[[i]]<-rbind.data.frame(
    outListWeek[[1]][which(outListWeek[[1]]$name==cities[i]),c("type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outListWeek[[2]][which(outListWeek[[2]]$name==cities[i]),c("type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outlook34[which(outlook34$name==cities[i]),c("type","Fcst_Date",
                                                 "Valid_Seas","label.x",
                                                 "label.y")],
    outListSeas[[1]][which(outListSeas[[1]]$name==cities[i]),c("type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outListSeas[[2]][which(outListSeas[[2]]$name==cities[i]),c("type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")]
  )    
  colnames(cityList[[i]])[1:5]<-c("Product","Forecast Date","Forecast Period","Probability","Forecast")
  cityList[[i]]<-sf::st_drop_geometry(cityList[[i]])
}

##### JSON output
# assemble forecasts by city
cities<-outListWeek[[1]]$name
cityListJSON<-list()

for(i in 1:length(cities)){
  cityListJSON[[i]]<-rbind.data.frame(
    outListWeek[[1]][which(outListWeek[[1]]$name==cities[i]),c("name","type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outListWeek[[2]][which(outListWeek[[2]]$name==cities[i]),c("name","type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outlook34[which(outlook34$name==cities[i]),c("name","type","Fcst_Date",
                                                        "Valid_Seas","label.x",
                                                        "label.y")],
    outListSeas[[1]][which(outListSeas[[1]]$name==cities[i]),c("name","type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")],
    outListSeas[[2]][which(outListSeas[[2]]$name==cities[i]),c("name","type","Fcst_Date",
                                                               "Valid_Seas","label.x",
                                                               "label.y")]
  )    
  colnames(cityListJSON[[i]])[1:6]<-c("Station","Product","Forecast Date","Forecast Period","Probability","Forecast")
  cityListJSON[[i]]<-sf::st_drop_geometry(cityListJSON[[i]])
}


# write to JSON file
fcstJSON<-jsonlite::toJSON(cityListJSON, pretty = TRUE)
write(fcstJSON, "SWMF_Outlooks.json")    
print("Write fcstJSON to file")

source('/home/crimmins/RProjects/MonsoonFantasyOutlooks/pushNotify.R')
