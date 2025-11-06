#setwd("ILRI/Indicator_Calculations_Mother_Data")

setwd("../../")

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

previous_crop_yield_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/crop_yield_units.csv",na=c('n/a','<NA>','NA')))#na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_crop_price_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/crop_price_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_land_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/land_area_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_milk_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/milk_amount_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_milk_sold_price_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/milk_price_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_butter_price_time_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/butter_price_time_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_butter_time_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/butter_time_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_butter_units_amount<-data.frame(read_csv("Units_and_Cleaning_Data/Units/butter_amount_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_cheese_price_time_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/cheese_price_time_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_cheese_time_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/cheese_time_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_cheese_units_amount<-data.frame(read_csv("Units_and_Cleaning_Data/Units/cheese_amount_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_bees_honey_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/honey_amount_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_eggs_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/eggs_amount_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_eggs_price_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/eggs_price_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_fertiliser_units<-data.frame(read_csv("Units_and_Cleaning_Data/Units/fertiliser_units.csv",na=c('n/a','<NA>','NA')))#,na.strings = c("n/a","<NA>", "NA"), fileEncoding="UTF-8", stringsAsFactors = FALSE)


#### The new data file for which units need to be checked


country<-firstup(unique(dat_all$country))

#This Should include other units
#------------------------------------------------------------------------------------------------------------
####crop_yield_units ####

length_temp<-length(grep("crop_yield_units_", colnames(dat_all)))
unique_crop_units<-c()


for (crops in grep("crop_yield_units_", colnames(dat_all)))
{
  
  temp<- dat_all[,crops]
  unique_crop_units<-c(unique_crop_units, unique(as.character(temp)))
}    

unique_crop_units<- unique_crop_units[unique_crop_units!=""]
unique_crop_units<- unique(unique_crop_units)

final_row<- nrow(previous_crop_yield_units)
for (i in unique_crop_units)
{
  if (!is.na(i))
  {
  if (i %in% previous_crop_yield_units[previous_crop_yield_units$crop_yield_units.Country==country,colnames(previous_crop_yield_units)=="crop_yield_units.Unit"]==FALSE)
  {
    previous_crop_yield_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_crop_yield_units), "Units_and_Cleaning_Data/Units/crop_yield_units.csv")# row.names = F, fileEncoding = 'UTF-8')
#--------------
#### crop_sold_units_coefficients ####

length_temp<-length(grep("crop_sold_price_quantityunits_|crop_price_quantityunits_other_", colnames(dat_all)))
unique_crop_price_units<-c()


for (crops_price_quantity_units in grep("crop_sold_price_quantityunits_|crop_price_quantityunits_other_", colnames(dat_all)))
{
  
  temp<- dat_all[,crops_price_quantity_units]
  unique_crop_price_units<-c(unique_crop_price_units, unique(as.character(temp)))
}    


unique_crop_price_units<- unique_crop_price_units[unique_crop_price_units!=""]
unique_crop_price_units<- unique(unique_crop_price_units)

final_row<- nrow(previous_crop_price_units)
for (i in unique_crop_price_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_crop_price_units[previous_crop_price_units$crop_price_units.Country==country,colnames(previous_crop_price_units)=="crop_price_units.Unit"]==FALSE)
  {
    previous_crop_price_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}


write_csv(data.frame(previous_crop_price_units), "Units_and_Cleaning_Data/Units/crop_price_units.csv")# row.names = F,  fileEncoding = "UTF-8")

#------------------------------------------------------------------------------------------------------------
#### unique_land_units ####

length_temp<-length(grep("unitland|areaunits_other|unitland_owned|unitland_rentin|unitland_rentout", colnames(dat_all)))
unique_land_units<-c()


for (land_area_units in grep("unitland|areaunits_other|unitland_owned|unitland_rentin|unitland_rentout", colnames(dat_all)))
{
  
  temp<- dat_all[,land_area_units]
  unique_land_units<-c(unique_land_units, unique(as.character(temp)))
}    


unique_land_units<- unique_land_units[unique_land_units!=""]
unique_land_units<- unique(unique_land_units)

final_row<- nrow(previous_land_units)
for (i in unique_land_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_land_units[previous_land_units$land_area_units.Country==country,colnames(previous_land_units)=="land_area_units.Unit"]==FALSE)
  {
    
    previous_land_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
  
}

write_csv(data.frame(previous_land_units), "Units_and_Cleaning_Data/Units/land_area_units.csv")#row.names = F,  fileEncoding = "UTF-8")

#------------------------------------------------------------------------------------------------------------
#### unique_milk_units ####

length_temp<-length(grep("milk_units_|milk_amount_units_other_", colnames(dat_all)))
unique_milk_units<-c()


for (milk_units in grep("milk_units_|milk_amount_units_other_", colnames(dat_all)))
{
  
  temp<- dat_all[,milk_units]
  unique_milk_units<-c(unique_milk_units, unique(as.character(temp)))
}    

unique_milk_units<- unique_milk_units[unique_milk_units!=""]
unique_milk_units<- unique(unique_milk_units)

final_row<- nrow(previous_milk_units)
for (i in unique_milk_units)
{
  if (!is.na(i))
  {
  
  if (i %in% previous_milk_units[previous_milk_units$milk_amount_units.Country==country,colnames(previous_milk_units)=="milk_amount_units.Unit"]==FALSE)
  {
    previous_milk_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
  
}

write_csv(data.frame(previous_milk_units), "Units_and_Cleaning_Data/Units/milk_amount_units.csv")#row.names = F,  fileEncoding = "UTF-8")

#--------------
####unique_milk_sold_price_units ####

length_temp<-length(grep("milk_sold_price_timeunits_|milk_amount_time_units_other_", colnames(dat_all)))
unique_milk_sold_price_units<-c()


for (milk_price_time_units in grep("milk_sold_price_timeunits_|milk_amount_time_units_other_", colnames(dat_all)))
{
  
  temp<- dat_all[,milk_price_time_units]
  unique_milk_sold_price_units<-c(unique_milk_sold_price_units, unique(as.character(temp)))
}    

unique_milk_sold_price_units<- unique_milk_sold_price_units[unique_milk_sold_price_units!=""]
unique_milk_sold_price_units<- unique(unique_milk_sold_price_units)

final_row<- nrow(previous_milk_sold_price_units)
for (i in unique_milk_sold_price_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_milk_sold_price_units[previous_milk_sold_price_units$milk_price_units.Country==country,colnames(previous_milk_sold_price_units)=="milk_price_units.Unit"]==FALSE)
  {
    previous_milk_sold_price_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_milk_sold_price_units), "Units_and_Cleaning_Data/Units/milk_price_units.csv")# row.names = F,  fileEncoding = "UTF-8")


#------------------------------------------------------------------------------------------------------------
#### unique_butter_price_time_units ####
length_temp<-length(grep("butter_sold_price_timeunits", colnames(dat_all)))
unique_butter_price_time_units<-c()


for (units in grep("butter_sold_price_timeunits", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_butter_price_time_units<-c(unique_butter_price_time_units, unique(as.character(temp)))
}    

unique_butter_price_time_units<- unique_butter_price_time_units[unique_butter_price_time_units!=""]
unique_butter_price_time_units<- unique(unique_butter_price_time_units)

final_row<- nrow(previous_butter_price_time_units)
for (i in unique_butter_price_time_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_butter_price_time_units[previous_butter_price_time_units$butter_price_time_units.Country==country,colnames(previous_butter_price_time_units)=="butter_price_time_units.Unit"]==FALSE)
  {
    previous_butter_price_time_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
  
}

write_csv(data.frame(previous_butter_price_time_units), "Units_and_Cleaning_Data/Units/butter_price_time_units.csv")# row.names = F,  fileEncoding = "UTF-8")

#--------------
####unique_butter_time_units ####

length_temp<-length(grep("butter_time_units|butter_amount_time_units_other", colnames(dat_all)))
unique_butter_time_units<-c()


for (units in grep("butter_time_units|butter_amount_time_units_other", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_butter_time_units<-c(unique_butter_time_units, unique(as.character(temp)))
}    


unique_butter_time_units<- unique_butter_time_units[unique_butter_time_units!=""]
unique_butter_time_units<- unique(unique_butter_time_units)

final_row<- nrow(previous_butter_time_units)
for (i in unique_butter_time_units)
{
  if (!is.na(i))
  {
  
  if (i %in% previous_butter_time_units[previous_butter_time_units$butter_time_units.Country==country,colnames(previous_butter_time_units)=="butter_time_units.Unit"]==FALSE)
  {
    previous_butter_time_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_butter_time_units), "Units_and_Cleaning_Data/Units/butter_time_units.csv")# row.names = F,  fileEncoding = "UTF-8")

#--------------
####unique_butter_units AMOUNT ####
length_temp<-length(grep("butter_units|butter_amount_units_other", colnames(dat_all)))
unique_butter_units<-c()


for (units in grep("butter_units|butter_amount_units_other", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_butter_units<-c(unique_butter_units, unique(as.character(temp)))
}    


unique_butter_units<- unique_butter_units[unique_butter_units!=""]
unique_butter_units<- unique(unique_butter_units)

final_row<- nrow(previous_butter_units_amount)
for (i in unique_butter_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_butter_units_amount[previous_butter_units_amount$butter_amount_units.Country==country,colnames(previous_butter_units_amount)=="butter_amount_units.Unit"]==FALSE)
  {
    previous_butter_units_amount[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_butter_units_amount), "Units_and_Cleaning_Data/Units/butter_amount_units.csv")# row.names = F,  fileEncoding = "UTF-8")

#------------------------------------------------------------------------------------------------------------
####unique_cheese_price_time_units####
length_temp<-length(grep("cheese_sold_price_timeunits", colnames(dat_all)))
unique_cheese_price_time_units<-c()


for (units in grep("cheese_sold_price_timeunits", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_cheese_price_time_units<-c(unique_cheese_price_time_units, unique(as.character(temp)))
}    

unique_cheese_price_time_units<- unique_cheese_price_time_units[unique_cheese_price_time_units!=""]
unique_cheese_price_time_units<- unique(unique_cheese_price_time_units)

final_row<- nrow(previous_cheese_price_time_units)
for (i in unique_cheese_price_time_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_cheese_price_time_units[previous_cheese_price_time_units$cheese_price_time_units.Country==country,colnames(previous_cheese_price_time_units)=="cheese_price_time_units.Unit"]==FALSE)
  {
    previous_cheese_price_time_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_cheese_price_time_units), "Units_and_Cleaning_Data/Units/cheese_price_time_units.csv")#row.names = F,  fileEncoding = "UTF-8")

#--------------
####unique_cheese_time_units ####
length_temp<-length(grep("cheese_time_units|cheese_amount_time_units_other", colnames(dat_all)))
unique_cheese_time_units<-c()


for (units in grep("cheese_time_units|cheese_amount_time_units_other", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_cheese_time_units<-c(unique_cheese_time_units, unique(as.character(temp)))
}    

unique_cheese_time_units<- unique_cheese_time_units[unique_cheese_time_units!=""]
unique_cheese_time_units<- unique(unique_cheese_time_units)

final_row<- nrow(previous_cheese_time_units)
for (i in unique_cheese_time_units)
{
  if (!is.na(i))
  {

  if (i %in% previous_cheese_time_units[previous_cheese_time_units$cheese_time_units.Country==country,colnames(previous_cheese_time_units)=="cheese_time_units.Unit"]==FALSE)
  {
    previous_cheese_time_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_cheese_time_units), "Units_and_Cleaning_Data/Units/cheese_time_units.csv")# row.names = F,  fileEncoding = "UTF-8")
#--------------
####unique_cheese_units####
length_temp<-length(grep("cheese_units|cheese_amount_units_other", colnames(dat_all)))
unique_cheese_units<-c()


for (units in grep("cheese_units|cheese_amount_units_other", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_cheese_units<-c(unique_cheese_units, unique(as.character(temp)))
}    


unique_cheese_units<- unique_cheese_units[unique_cheese_units!=""]
unique_cheese_units<- unique(unique_cheese_units)

final_row<- nrow(previous_cheese_units_amount)
for (i in unique_cheese_units)
{
  if (!is.na(i))
  {
  
  
  if (i %in% previous_cheese_units_amount[previous_cheese_units_amount$cheese_amount_units.Country==country,colnames(previous_cheese_units_amount)=="cheese_amount_units.Unit"]==FALSE)
  {
    previous_cheese_units_amount[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_cheese_units_amount), "Units_and_Cleaning_Data/Units/cheese_amount_units.csv")# row.names = F,  fileEncoding = "UTF-8")
#------------------------------------------------------------------------------------------------------------
####unique_bees_honey_units####
length_temp<-length(grep("bees_honey_production_units_", colnames(dat_all)))
unique_bees_honey_units<-c()


for (units in grep("bees_honey_production_units_", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_bees_honey_units<-c(unique_bees_honey_units, unique(as.character(temp)))
}    

unique_bees_honey_units<- unique_bees_honey_units[unique_bees_honey_units!=""]
unique_bees_honey_units<- unique(unique_bees_honey_units)

final_row<- nrow(previous_bees_honey_units)
for (i in unique_bees_honey_units)
{

  if (!is.na(i))
  {
  if (i %in% previous_bees_honey_units[previous_bees_honey_units$honey_amount_units.Country==country,colnames(previous_bees_honey_units)=="honey_amount_units.Unit"]==FALSE)
  {
    previous_bees_honey_units[final_row+1,]<- c(country,i,NA)
    final_row<-final_row+1
  }
  }
}

write_csv(data.frame(previous_bees_honey_units), "Units_and_Cleaning_Data/Units/honey_amount_units.csv")#row.names = F,  fileEncoding = "UTF-8")
#------------------------------------------------------------------------------------------------------------
####unique_eggs_units####
length_temp<-length(grep("eggs_units_|eggs_amount_units_other_", colnames(dat_all)))
unique_eggs_units<-c()


for (units in grep("eggs_units_|eggs_amount_units_other_", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_eggs_units<-c(unique_eggs_units, unique(as.character(temp)))
}    


unique_eggs_units<- unique_eggs_units[unique_eggs_units!=""]
unique_eggs_units<- unique(unique_eggs_units)

final_row<- nrow(previous_eggs_units)
for (i in unique_eggs_units)
{
  if (!is.na(i))
  {
    final_row<- nrow(previous_eggs_units)
    
    
  if (i %in% previous_eggs_units[previous_eggs_units$eggs_amount_units.Country==country,colnames(previous_eggs_units)=="eggs_amount_units.Unit"]==FALSE)
  {
    previous_eggs_units[final_row+1,]<- c(country,i,NA)
  }
  }
}

write_csv(data.frame(previous_eggs_units), "Units_and_Cleaning_Data/Units/eggs_amount_units.csv")# row.names = F,  fileEncoding = "UTF-8")

#--------------
#### unique_eggs_price_units ####
length_temp<-length(grep("eggs_sold_price_timeunits_", colnames(dat_all)))
unique_eggs_price_units<-c()


for (units in grep("eggs_sold_price_timeunits_", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_eggs_price_units<-c(unique_eggs_price_units, unique(as.character(temp)))
}    


unique_eggs_price_units<- unique_eggs_price_units[unique_eggs_price_units!=""]
unique_eggs_price_units<- unique(unique_eggs_price_units)

final_row<- nrow(previous_eggs_price_units)
for (i in unique_eggs_price_units)
{

  if (!is.na(i))
  {
    final_row<- nrow(previous_eggs_price_units)
    
  
  if (i %in% previous_eggs_price_units[previous_eggs_price_units$eggs_price_units.Country==country,colnames(previous_eggs_price_units)=="eggs_price_units.Unit"]==FALSE)
  {
    previous_eggs_price_units[final_row+1,]<- c(country,i,NA)
  }
  }
}

write_csv(data.frame(previous_eggs_price_units), "Units_and_Cleaning_Data/Units/eggs_price_units.csv")# row.names = F,  fileEncoding = "UTF-8")
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------

length_temp<-length(grep("fertiliser_units", colnames(dat_all)))
unique_fertiliser_units<-c()


for (units in grep("fertiliser_units", colnames(dat_all)))
{
  
  temp<- dat_all[,units]
  unique_fertiliser_units<-c(unique_fertiliser_units, unique(as.character(temp)))
}    


unique_fertiliser_units<- unique_fertiliser_units[unique_fertiliser_units!=""]
unique_fertiliser_units<- unique(unique_fertiliser_units)

for (i in unique_fertiliser_units)
{
  if (!is.na(i))
  {
  final_row<- nrow(previous_fertiliser_units)
  if (i %in% previous_fertiliser_units[previous_fertiliser_units$Country==country,colnames(previous_fertiliser_units)=="Unit"]==FALSE)
  {
    previous_fertiliser_units[final_row+1,]<- c(country,i,NA)
  }
  }
}

write_csv(data.frame(previous_fertiliser_units), "Units_and_Cleaning_Data/Units/fertiliser_units.csv")# row.names = F,  fileEncoding = "UTF-8")


 setwd(paste0("Projects/", project_ID))
# 
# write_csv(data.frame(previous_crop_yield_units), "Units_and_Cleaning_Data/Units/crop_yield_units.csv", row.names = F)
# write_csv(data.frame(previous_crop_price_units), "Units_and_Cleaning_Data/Units/crop_price_units.csv", row.names = F)
# write_csv(data.frame(previous_land_units), "Units_and_Cleaning_Data/Units/land_area_units.csv", row.names = F)
# write_csv(data.frame(previous_milk_units), "Units_and_Cleaning_Data/Units/milk_amount_units.csv", row.names = F)
# write_csv(data.frame(previous_milk_sold_price_units), "Units_and_Cleaning_Data/Units/milk_price_units.csv", row.names = F)
# write_csv(data.frame(previous_butter_price_time_units), "Units_and_Cleaning_Data/Units/butter_price_time_units.csv", row.names = F)
# write_csv(data.frame(previous_butter_time_units), "Units_and_Cleaning_Data/Units/butter_time_units.csv", row.names = F)
# write_csv(data.frame(previous_butter_units_amount), "Units_and_Cleaning_Data/Units/butter_amount_units.csv", row.names = F)
# write_csv(data.frame(previous_cheese_price_time_units), "Units_and_Cleaning_Data/Units/cheese_price_time_units.csv", row.names = F)
# write_csv(data.frame(previous_cheese_time_units), "Units_and_Cleaning_Data/Units/cheese_time_units.csv", row.names = F)
# write_csv(data.frame(previous_cheese_units_amount), "Units_and_Cleaning_Data/Units/cheese_amount_units.csv", row.names = F)
# write_csv(data.frame(previous_bees_honey_units), "Units_and_Cleaning_Data/Units/honey_amount_units.csv", row.names = F)
# write_csv(data.frame(previous_eggs_units), "Units_and_Cleaning_Data/Units/eggs_amount_units.csv", row.names = F)
# write_csv(data.frame(previous_eggs_price_units), "Units_and_Cleaning_Data/Units/eggs_price_units.csv", row.names = F)
# write_csv(data.frame(previous_fertiliser_units), "Units_and_Cleaning_Data/Units/fertiliser_units.csv", row.names = F)

