

#### A script to find all the units in a particular cleaned raw data set
raw_data_file_name<- file.choose()

raw_data<- read.csv(raw_data_file_name,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
dat_all<- raw_data



dat_all[dat_all$local_currency=="quetzales", colnames(dat_all)=="country"] <- "Guatemala"
dat_all[dat_all$local_currency=="dolares", colnames(dat_all)=="country"] <- "El_Salvador"
dat_all[dat_all$local_currency=="lempiras", colnames(dat_all)=="country"] <- "Honduras"


dat_all$country<- gsub("ethiopia","eth", dat_all$country)
dat_all$country<- gsub("eth","ethiopia", dat_all$country)
dat_all$country<- gsub("tnz","tanzania", dat_all$country)
dat_all$country<- gsub("burkina faso","burkina", dat_all$country)
dat_all$country<- gsub("burkina","Burkina_Faso", dat_all$country)
dat_all$country<- gsub(" ","_", dat_all$country)
dat_all$country<- gsub(" ","_", dat_all$country)
dat_all$country<- gsub("drc","DRC", dat_all$country)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
} # A function found online at: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case

dat_all$country<- firstup(dat_all$country)

#This Should include other units
#------------------------------------------------------------------------------------------------------------
####crop_yield_units ####

length_temp<-length(grep("crop_yield_units_", colnames(dat_all)))
unique_crop_units<-c()


for (crops in grep("crop_yield_units_", colnames(dat_all)))
{

  temp<- paste0(dat_all$country, " ", dat_all[,crops])
  unique_crop_units<-c(unique_crop_units, unique(as.character(temp)))
}    

unique_crop_units<- unique_crop_units[unique_crop_units!=""]
unique_crop_units<- unique(unique_crop_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_crop_units <- data.frame("Country"=sub(rexp,"\\1",unique_crop_units), "Unit"=sub(rexp,"\\2",unique_crop_units)) 
unique_crop_units <- data.frame(unique_crop_units, "Conversion_Factor"= rep(NA, nrow(unique_crop_units)))


write.csv(data.frame("crop_yield_units"=unique_crop_units), "./Units/crop_yield_units.csv", row.names = F)
#--------------
#### crop_sold_units_coefficients ####

length_temp<-length(grep("crop_sold_price_quantityunits_|crop_price_quantityunits_other_", colnames(dat_all)))
unique_crop_price_units<-c()


for (crops_price_quantity_units in grep("crop_sold_price_quantityunits_|crop_price_quantityunits_other_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,crops_price_quantity_units])
  unique_crop_price_units<-c(unique_crop_price_units, unique(as.character(temp)))
}    


unique_crop_price_units<- unique_crop_price_units[unique_crop_price_units!=""]
unique_crop_price_units<- unique(unique_crop_price_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_crop_price_units <- data.frame("Country"=sub(rexp,"\\1",unique_crop_price_units), "Unit"=sub(rexp,"\\2",unique_crop_price_units))
unique_crop_price_units <- data.frame(unique_crop_price_units,"Conversion_Factor"= rep(NA, nrow(unique_crop_price_units)))


write.csv(data.frame("crop_price_units"=unique_crop_price_units), "./Units/crop_price_units.csv", row.names = F)

#------------------------------------------------------------------------------------------------------------
#### unique_land_units ####

length_temp<-length(grep("unitland|areaunits_other|unitland_owned|unitland_rentin|unitland_rentout", colnames(dat_all)))
unique_land_units<-c()


for (land_area_units in grep("unitland|areaunits_other|unitland_owned|unitland_rentin|unitland_rentout", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,land_area_units])
  unique_land_units<-c(unique_land_units, unique(as.character(temp)))
}    


unique_land_units<- unique_land_units[unique_land_units!=""]
unique_land_units<- unique(unique_land_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_land_units <- data.frame("Country"=sub(rexp,"\\1",unique_land_units), "Unit"=sub(rexp,"\\2",unique_land_units))
unique_land_units <- data.frame(unique_land_units,"Conversion_Factor"= rep(NA, nrow(unique_land_units)))


write.csv(data.frame("land_area_units"=unique_land_units), "./Units/land_area_units.csv", row.names = F)

#------------------------------------------------------------------------------------------------------------
#### unique_milk_units ####

length_temp<-length(grep("milk_units_|milk_amount_units_other_", colnames(dat_all)))
unique_milk_units<-c()


for (milk_units in grep("milk_units_|milk_amount_units_other_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,milk_units])
  unique_milk_units<-c(unique_milk_units, unique(as.character(temp)))
}    

unique_milk_units<- unique_milk_units[unique_milk_units!=""]
unique_milk_units<- unique(unique_milk_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_milk_units <- data.frame("Country"=sub(rexp,"\\1",unique_milk_units), "Unit"=sub(rexp,"\\2",unique_milk_units))
unique_milk_units <- data.frame(unique_milk_units,"Conversion_Factor"= rep(NA, nrow(unique_milk_units)))


write.csv(data.frame("milk_amount_units"=unique_milk_units), "./Units/milk_amount_units.csv", row.names = F)

#--------------
####unique_milk_sold_price_units ####

length_temp<-length(grep("milk_sold_price_timeunits_|milk_amount_time_units_other_", colnames(dat_all)))
unique_milk_sold_price_units<-c()


for (milk_price_time_units in grep("milk_sold_price_timeunits_|milk_amount_time_units_other_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,milk_price_time_units])
  unique_milk_sold_price_units<-c(unique_milk_sold_price_units, unique(as.character(temp)))
}    

unique_milk_sold_price_units<- unique_milk_sold_price_units[unique_milk_sold_price_units!=""]
unique_milk_sold_price_units<- unique(unique_milk_sold_price_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_milk_sold_price_units <- data.frame("Country"=sub(rexp,"\\1",unique_milk_sold_price_units), "Unit"=sub(rexp,"\\2",unique_milk_sold_price_units))
unique_milk_sold_price_units <- data.frame(unique_milk_sold_price_units,"Conversion_Factor"= rep(NA, nrow(unique_milk_sold_price_units)))


write.csv(data.frame("milk_price_units"=unique_milk_sold_price_units), "./Units/milk_price_units.csv", row.names = F)


#------------------------------------------------------------------------------------------------------------
#### unique_butter_price_time_units ####
length_temp<-length(grep("butter_sold_price_timeunits", colnames(dat_all)))
unique_butter_price_time_units<-c()


for (units in grep("butter_sold_price_timeunits", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_butter_price_time_units<-c(unique_butter_price_time_units, unique(as.character(temp)))
}    

unique_butter_price_time_units<- unique_butter_price_time_units[unique_butter_price_time_units!=""]
unique_butter_price_time_units<- unique(unique_butter_price_time_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_butter_price_time_units <- data.frame("Country"=sub(rexp,"\\1",unique_butter_price_time_units), "Unit"=sub(rexp,"\\2",unique_butter_price_time_units))
unique_butter_price_time_units <- data.frame(unique_butter_price_time_units,"Conversion_Factor"= rep(NA, nrow(unique_butter_price_time_units)))


write.csv(data.frame("butter_price_time_units"=unique_butter_price_time_units), "./Units/butter_price_time_units.csv", row.names = F)

#--------------
####unique_butter_time_units ####

length_temp<-length(grep("butter_time_units|butter_amount_time_units_other", colnames(dat_all)))
unique_butter_time_units<-c()


for (units in grep("butter_time_units|butter_amount_time_units_other", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_butter_time_units<-c(unique_butter_time_units, unique(as.character(temp)))
}    


unique_butter_time_units<- unique_butter_time_units[unique_butter_time_units!=""]
unique_butter_time_units<- unique(unique_butter_time_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_butter_time_units <- data.frame("Country"=sub(rexp,"\\1",unique_butter_time_units), "Unit"=sub(rexp,"\\2",unique_butter_time_units))
unique_butter_time_units <- data.frame(unique_butter_time_units,"Conversion_Factor"= rep(NA, nrow(unique_butter_time_units)))


write.csv(data.frame("butter_time_units"=unique_butter_time_units), "./Units/butter_time_units.csv", row.names = F)

#--------------
####unique_butter_units AMOUNT ####
length_temp<-length(grep("butter_units|butter_amount_units_other", colnames(dat_all)))
unique_butter_units<-c()


for (units in grep("butter_units|butter_amount_units_other", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_butter_units<-c(unique_butter_units, unique(as.character(temp)))
}    


unique_butter_units<- unique_butter_units[unique_butter_units!=""]
unique_butter_units<- unique(unique_butter_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_butter_units <- data.frame("Country"=sub(rexp,"\\1",unique_butter_units), "Unit"=sub(rexp,"\\2",unique_butter_units))
unique_butter_units <- data.frame(unique_butter_units,"Conversion_Factor"= rep(NA, nrow(unique_butter_units)))


write.csv(data.frame("butter_amount_units"=unique_butter_units), "./Units/butter_amount_units.csv", row.names = F)

#------------------------------------------------------------------------------------------------------------
####unique_cheese_price_time_units####
length_temp<-length(grep("cheese_sold_price_timeunits", colnames(dat_all)))
unique_cheese_price_time_units<-c()


for (units in grep("cheese_sold_price_timeunits", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_cheese_price_time_units<-c(unique_cheese_price_time_units, unique(as.character(temp)))
}    

unique_cheese_price_time_units<- unique_cheese_price_time_units[unique_cheese_price_time_units!=""]
unique_cheese_price_time_units<- unique(unique_cheese_price_time_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_cheese_price_time_units <- data.frame("Country"=sub(rexp,"\\1",unique_cheese_price_time_units), "Unit"=sub(rexp,"\\2",unique_cheese_price_time_units))
unique_cheese_price_time_units <- data.frame(unique_cheese_price_time_units,"Conversion_Factor"= rep(NA, nrow(unique_cheese_price_time_units)))


write.csv(data.frame("cheese_price_time_units"=unique_cheese_price_time_units), "./Units/cheese_price_time_units.csv", row.names = F)

#--------------
####unique_cheese_time_units ####
length_temp<-length(grep("cheese_time_units|cheese_amount_time_units_other", colnames(dat_all)))
unique_cheese_time_units<-c()


for (units in grep("cheese_time_units|cheese_amount_time_units_other", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_cheese_time_units<-c(unique_cheese_time_units, unique(as.character(temp)))
}    

unique_cheese_time_units<- unique_cheese_time_units[unique_cheese_time_units!=""]
unique_cheese_time_units<- unique(unique_cheese_time_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_cheese_time_units <- data.frame("Country"=sub(rexp,"\\1",unique_cheese_time_units), "Unit"=sub(rexp,"\\2",unique_cheese_time_units))
unique_cheese_time_units <- data.frame(unique_cheese_time_units,"Conversion_Factor"= rep(NA, nrow(unique_cheese_time_units)))


write.csv(data.frame("cheese_time_units"=unique_cheese_time_units), "./Units/cheese_time_units.csv", row.names = F)
#--------------
####unique_cheese_units####
length_temp<-length(grep("cheese_units|cheese_amount_units_other", colnames(dat_all)))
unique_cheese_units<-c()


for (units in grep("cheese_units|cheese_amount_units_other", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_cheese_units<-c(unique_cheese_units, unique(as.character(temp)))
}    


unique_cheese_units<- unique_cheese_units[unique_cheese_units!=""]
unique_cheese_units<- unique(unique_cheese_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_cheese_units <- data.frame("Country"=sub(rexp,"\\1",unique_cheese_units), "Unit"=sub(rexp,"\\2",unique_cheese_units))
unique_cheese_units <- data.frame(unique_cheese_units,"Conversion_Factor"= rep(NA, nrow(unique_cheese_units)))


write.csv(data.frame("cheese_amount_units"=unique_cheese_units), "./Units/cheese_amount_units.csv", row.names = F)
#------------------------------------------------------------------------------------------------------------
####unique_bees_honey_units####
length_temp<-length(grep("bees_honey_production_units_", colnames(dat_all)))
unique_bees_honey_units<-c()


for (units in grep("bees_honey_production_units_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_bees_honey_units<-c(unique_bees_honey_units, unique(as.character(temp)))
}    

unique_bees_honey_units<- unique_bees_honey_units[unique_bees_honey_units!=""]
unique_bees_honey_units<- unique(unique_bees_honey_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_bees_honey_units <- data.frame("Country"=sub(rexp,"\\1",unique_bees_honey_units), "Unit"=sub(rexp,"\\2",unique_bees_honey_units))
unique_bees_honey_units <- data.frame(unique_bees_honey_units,"Conversion_Factor"= rep(NA, nrow(unique_bees_honey_units)))


write.csv(data.frame("honey_amount_units"=unique_bees_honey_units), "./Units/honey_amount_units.csv", row.names = F)
#------------------------------------------------------------------------------------------------------------
####unique_eggs_units####
length_temp<-length(grep("eggs_units_|eggs_amount_units_other_", colnames(dat_all)))
unique_eggs_units<-c()


for (units in grep("eggs_units_|eggs_amount_units_other_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_eggs_units<-c(unique_eggs_units, unique(as.character(temp)))
}    


unique_eggs_units<- unique_eggs_units[unique_eggs_units!=""]
unique_eggs_units<- unique(unique_eggs_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_eggs_units <- data.frame("Country"=sub(rexp,"\\1",unique_eggs_units), "Unit"=sub(rexp,"\\2",unique_eggs_units))
unique_eggs_units <- data.frame(unique_eggs_units,"Conversion_Factor"= rep(NA, nrow(unique_eggs_units)))


write.csv(data.frame("eggs_amount_units"=unique_eggs_units), "./Units/eggs_amount_units.csv", row.names = F)

#--------------
#### unique_eggs_price_units ####
length_temp<-length(grep("eggs_sold_price_timeunits_", colnames(dat_all)))
unique_eggs_price_units<-c()


for (units in grep("eggs_sold_price_timeunits_", colnames(dat_all)))
{
  
  temp<- paste0(dat_all$country, " ", dat_all[,units])
  unique_eggs_price_units<-c(unique_eggs_price_units, unique(as.character(temp)))
}    


unique_eggs_price_units<- unique_eggs_price_units[unique_eggs_price_units!=""]
unique_eggs_price_units<- unique(unique_eggs_price_units)

rexp <- "^(\\w+)\\s?(.*)$"
unique_eggs_price_units <- data.frame("Country"=sub(rexp,"\\1",unique_eggs_price_units), "Unit"=sub(rexp,"\\2",unique_eggs_price_units))
unique_eggs_price_units <- data.frame(unique_eggs_price_units,"Conversion_Factor"= rep(NA, nrow(unique_eggs_price_units)))


write.csv(data.frame("eggs_price_units"=unique_eggs_price_units), "./Units/eggs_price_units.csv", row.names = F)
#------------------------------------------------------------------------------------------------------------



