


previous_crop_yield_units<-read.csv("./Units/crop_yield_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_crop_price_units<-read.csv("./Units/crop_price_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_land_units<-read.csv("./Units/land_area_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_milk_units<-read.csv("./Units/milk_amount_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_milk_sold_price_units<-read.csv("./Units/milk_price_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_butter_price_time_units<-read.csv("./Units/butter_price_time_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_butter_time_units<-read.csv("./Units/butter_time_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_butter_units_amount<-read.csv("./Units/butter_amount_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_cheese_price_time_units<-read.csv("./Units/cheese_price_time_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_cheese_time_units<-read.csv("./Units/cheese_time_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_cheese_units_amount<-read.csv("./Units/cheese_amount_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_bees_honey_units<-read.csv("./Units/honey_amount_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_eggs_units<-read.csv("./Units/eggs_amount_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
previous_eggs_price_units<-read.csv("./Units/eggs_price_units.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)

  
#### The new data file for which units need to be checked
raw_data_file_name<- file.choose()

raw_data<- read.csv(raw_data_file_name,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
dat_all<- raw_data

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
  if (i %in% previous_crop_yield_units[,1]==FALSE)
  {
    previous_crop_yield_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("crop_yield_units"=previous_crop_yield_units), "./Units/crop_yield_units.csv", row.names = F)
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
  if (i %in% previous_crop_price_units[,1]==FALSE)
  {
    previous_crop_price_units[final_row+1,]<- c(i,NA)
  }
}


write.csv(data.frame("crop_price_units"=previous_crop_price_units), "./Units/crop_price_units.csv", row.names = F)

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
  if (i %in% previous_land_units[,1]==FALSE)
  {
    previous_land_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("land_area_units"=previous_land_units), "./Units/land_area_units.csv", row.names = F)

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
  if (i %in% previous_milk_units[,1]==FALSE)
  {
    previous_milk_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("milk_amount_units"=previous_milk_units), "./Units/milk_amount_units.csv", row.names = F)

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
  if (i %in% previous_milk_sold_price_units[,1]==FALSE)
  {
    previous_milk_sold_price_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("milk_price_units"=previous_milk_sold_price_units), "./Units/milk_price_units.csv", row.names = F)


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
  if (i %in% previous_butter_price_time_units[,1]==FALSE)
  {
    previous_butter_price_time_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("butter_price_time_units"=previous_butter_price_time_units), "./Units/butter_price_time_units.csv", row.names = F)

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
  if (i %in% previous_butter_time_units[,1]==FALSE)
  {
    previous_butter_time_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("butter_time_units"=previous_butter_time_units), "./Units/butter_time_units.csv", row.names = F)

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
  if (i %in% previous_butter_units_amount[,1]==FALSE)
  {
    previous_butter_units_amount[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("butter_amount_units"=previous_butter_units_amount), "./Units/butter_amount_units.csv", row.names = F)

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
  if (i %in% previous_cheese_price_time_units[,1]==FALSE)
  {
    previous_cheese_price_time_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("cheese_price_time_units"=previous_cheese_price_time_units), "./Units/cheese_price_time_units.csv", row.names = F)

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
  if (i %in% previous_cheese_time_units[,1]==FALSE)
  {
    previous_cheese_time_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("cheese_time_units"=previous_cheese_time_units), "./Units/cheese_time_units.csv", row.names = F)
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
  if (i %in% previous_cheese_units_amount[,1]==FALSE)
  {
    previous_cheese_units_amount[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("cheese_amount_units"=previous_cheese_units_amount), "./Units/cheese_amount_units.csv", row.names = F)
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
  if (i %in% previous_bees_honey_units[,1]==FALSE)
  {
    previous_bees_honey_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("honey_amount_units"=previous_bees_honey_units), "./Units/honey_amount_units.csv", row.names = F)
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
  if (i %in% previous_eggs_units[,1]==FALSE)
  {
    previous_eggs_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("eggs_amount_units"=previous_eggs_units), "./Units/eggs_amount_units.csv", row.names = F)

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
  if (i %in% previous_eggs_price_units[,1]==FALSE)
  {
    previous_eggs_price_units[final_row+1,]<- c(i,NA)
  }
}

write.csv(data.frame("eggs_price_units"=previous_eggs_price_units), "./Units/eggs_price_units.csv", row.names = F)
#------------------------------------------------------------------------------------------------------------



