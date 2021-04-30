library(dplyr)
library(readr)
library(tidyr)

##########################################################################################
####################################### crime data #######################################
##########################################################################################

dates <- as.vector(read.csv("data/dates.csv")[,1])
wales <- c("dyfed-powys","gwent","north-wales","south-wales")
crime.wales = c()

for(w in wales){
  crime.csv = c()
  for(d in dates){
    file.name = paste0("data/wales/",d,"/",d,"-",w,"-street.csv")
    tmp.csv <- read.csv(file.name)
    crime.csv <- rbind(crime.csv,tmp.csv)
  }
  crime.wales <- rbind(crime.wales,crime.csv)
} 

write.csv(crime.wales,"data/crime-Wales.csv", row.names = FALSE)

############################################################################################
####################################### house price data ###################################
############################################################################################

house.uk = c()
years <- c("2018","2019","2020")

for(y in years){
  the.url = paste0("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-",y,".csv")
  house.csv <- read.csv(the.url)
  colnames(house.csv)<- name.col<- c("Transaction unique identifier","Price",	
                                     "Date of Transfer","Postcode","Property Type",
                                     "Old/New","Duration","PAON","SAON","Street",
                                     "Locality","Town/City","District",
                                     "County","PPD Category Type","Record Status")
  house.uk <- rbind(house.uk,house.csv)
} 

# remove data out of Wales
wales.counties <- c("BLAENAU GWENT","BRIDGEND","CAERPHILLY","CARDIFF",	
                    "CARMARTHENSHIRE","CEREDIGION","CONWY","DENBIGHSHIRE",	
                    "FLINTSHIRE","GWYNEDD","ISLE OF ANGLESEY","MERTHYR TYDFIL",	
                    "MONMOUTHSHIRE","NEATH PORT TALBOT","NEWPORT","PEMBROKESHIRE",
                    "POWYS","RHONDDA CYNON TAF","SWANSEA","TORFAEN","VALE OF GLAMORGAN",	
                    "WREXHAM")

house.wales <- house.uk %>%
  filter(County %in% wales.counties)

#convert postcode into coordinates
download.file("https://www.freemaptools.com/download/full-postcodes/ukpostcodes.zip","ukpostcodes.zip")
postcode.Coords <- read_csv("ukpostcodes.zip")%>%
  select(postcode, latitude, longitude)
colnames(postcode.Coords)<-  c("Postcode", "Latitude", "Longitude")

house.wales2 <- left_join(house.wales, postcode.Coords, by = "Postcode")  
write.csv(house.wales2,"data/pp-Wales.csv", row.names = FALSE)

############################################################################################################
######################################### boundary and population data #####################################
############################################################################################################
library(sf)
# read boundary file
wales.sector <- st_read(dsn = "data/Sectors", layer = "Sectors")%>%
  mutate(area = substr(name,1,2))%>%
  filter(area %in% c("LL","SY","LD","SA","NP","CF"))%>%
  select(!area)%>%
  rename("postcode.sector"=name)

#read population data
pop.wales <- read.csv("data/uk population.csv")%>%
  mutate(area = substr(postcode.sector,1,2))%>%
  filter(area %in% c("LL","SY","LD","SA","NP","CF"))%>%
  select(!area)%>%
  rename("population" = X2011)

pop.wales.sector <- wales.sector%>%
  left_join(pop.wales, by = "postcode.sector")%>%
  drop_na()

st_write(pop.wales.sector, "data/wales pop.shp", layer = "wales pop")
##################################################

#have a look 
library(ggplot2)
ggplot(pop.wales.sector)+
  geom_sf()

############################################################################################################
############################################ spatial Operation #############################################
############################################################################################################

crime.wales.sf <- crime.wales%>%
  drop_na(Longitude)%>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(wales.sector))%>%
  st_join(pop.wales.sector,join = st_nearest_feature,left = FALSE)

house.wales.sf <- house.wales2%>%
  drop_na(Longitude)%>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(wales.sector))%>%
  st_join(pop.wales.sector,join = st_nearest_feature,left = FALSE)

############################################################################################################
########################################## crime rates (cases/1k) #########################################
############################################################################################################

#monthly crime rates at each postcode level
crime.overall.rates.wales <- crime.wales.sf %>%
  st_set_geometry(NULL)%>%
  group_by(postcode.sector,Month)%>%
  summarize(amount=n())%>%
  left_join(pop.wales.sector,by = "postcode.sector")%>%
  mutate(crime.rate = amount/population*10000)%>%
  select(!c(geometry,population))
  

#monthly crime rates at each postcode level of each crime type
crime.type.rates.wales <- crime.wales.sf %>%
  st_set_geometry(NULL)%>%
  group_by(Crime.type,postcode.sector,Month)%>%
  summarize(amount=n())%>%
  left_join(pop.wales.sector,by = "postcode.sector")%>%
  mutate(crime.rate = amount/population*10000)%>%  
  select(!c(geometry,population))%>%
  ungroup()

#####################################################################################################
############################################# Spatial Join #############################################
#####################################################################################################
#overall crime rate

final.dataset<- house.wales.sf %>%
  st_set_geometry(NULL)%>%
  mutate(Month = substr(`Date of Transfer`,1,7))%>%
  left_join(crime.overall.rates.wales,by = c('postcode.sector','Month'))%>%
  rename("Overall crime rate" = crime.rate,
         "Overall crime amount"= amount)

types <- unique(crime.type.rates.wales$Crime.type)
for(c in types){
  #subset
  tmp <- crime.type.rates.wales%>%
    filter(Crime.type == c)%>%
    select(!c(Crime.type))
  
  #rename
  names(tmp)[names(tmp) == 'crime.rate'] <- paste(c,"crime rate")
  names(tmp)[names(tmp) == 'amount'] <- paste(c,"crime amount")
  
  #left join
  final.dataset <- left_join(final.dataset, tmp,by = c('postcode.sector','Month'))
}

