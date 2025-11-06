library(readr)

ID_USED<- gsub('.*/','', getwd())

dir.create('Data_for_Sharing')

crop_data<- data.frame(read_csv('Data/Extra_Outputs/crop_details.csv', na=c('NA', 'n/a')), check.names = F)
crop_residue_use<-data.frame(read_csv('Data/Extra_Outputs/crop_residue_use.csv', na=c('NA', 'n/a')), check.names = F)
#crop_use<-data.frame(read_csv('Data/Extra_Outputs/crop_use.csv', na=c('NA')), check.names = F)
#Crops_Intercropped<-data.frame(read_csv('Data/Extra_Outputs/Crops_Intercropped.csv', na=c('NA', 'n/a')), check.names = F)
#Crops_Monocropped<-data.frame(read_csv('Data/Extra_Outputs/Crops_Monocropped.csv', na=c('NA', 'n/a')), check.names = F)
livestock_data<-data.frame(read_csv('Data/Extra_Outputs/livestock_details.csv', na=c('NA', 'n/a')), check.names = F)
core_data<-data.frame(read_csv('Data/Core_Data.csv', na=c('NA', 'n/a')), check.names = F)
raw_dat<-data.frame(read_csv('Data/Raw_Data.csv', na=c('NA', 'n/a')), check.names = F)
clean_data<-data.frame(read_csv('Data/Cleaned_Data.csv', na=c('NA', 'n/a')), check.names = F)
indicator_sheet<-data.frame(read_csv('Data/indicator_sheet.csv', na=c('NA', 'n/a')), check.names = F)


# prices_crop<-data.frame(read_csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/crop_prices_per_kg.csv')), check.names = F)
# prices_whole_livestock<-data.frame(read.csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/whole_livestock_prices.csv')), check.names = F)
# prices_milk<-data.frame(read_csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/milk_prices_per_l.csv')), check.names = F)
# prices_egg<-data.frame(read_csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/egg_price_per_egg.csv')), check.names = F)
# prices_meat<-data.frame(read_csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/meat_prices_per_kg.csv')), check.names = F)
# prices_honey<-data.frame(read_csv(paste0('../../Indicator_Calculations/Prices/',ID_USED,'/honey_price_per_litre.csv')), check.names = F)

prices_crop<-data.frame(read_csv('../../Indicator_Calculations/Prices/crop_prices_per_kg.csv'), check.names = F)
prices_whole_livestock<-data.frame(read.csv('../../Indicator_Calculations/Prices/whole_livestock_prices.csv'), check.names = F)
prices_milk<-data.frame(read_csv('../../Indicator_Calculations/Prices/milk_prices_per_l.csv'), check.names = F)
prices_egg<-data.frame(read_csv('../../Indicator_Calculations/Prices/egg_price_per_egg.csv'), check.names = F)
prices_meat<-data.frame(read_csv('../../Indicator_Calculations/Prices/meat_prices_per_kg.csv'), check.names = F)
prices_honey<-data.frame(read_csv('../../Indicator_Calculations/Prices/honey_price_per_litre.csv'), check.names = F)

#### Getting rid of previous IDs added #####

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(crop_data))>1)
{crop_data<- crop_data[,-which(colnames(crop_data) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(crop_residue_use))>1)
{crop_residue_use<- crop_residue_use[,-which(colnames(crop_residue_use) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

#if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(crop_use))>1)
#{crop_use<- crop_use[,-which(colnames(crop_use) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

#if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(Crops_Intercropped))>1)
#{Crops_Intercropped<- Crops_Intercropped[,-which(colnames(Crops_Intercropped) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

#if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(Crops_Monocropped))>1)
#{Crops_Monocropped<- Crops_Monocropped[,-which(colnames(Crops_Monocropped) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(livestock_data))>1)
{livestock_data<- livestock_data[,-which(colnames(livestock_data) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(core_data))>1)
{core_data<- core_data[,-which(colnames(core_data) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(raw_dat))>1)
{raw_dat<- raw_dat[,-which(colnames(raw_dat) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(clean_data))>1)
{clean_data<- clean_data[,-which(colnames(clean_data) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(indicator_sheet))>1)
{indicator_sheet<- indicator_sheet[,-which(colnames(indicator_sheet) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}

# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_crop))>1)
# {prices_crop<- prices_crop[,-which(colnames(prices_crop) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
# 
# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_whole_livestock))>1)
# {prices_whole_livestock<- prices_whole_livestock[,-which(colnames(prices_whole_livestock) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
# 
# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_milk))>1)
# {prices_milk<- prices_milk[,-which(colnames(prices_milk) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
# 
# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_egg))>1)
# {prices_egg<- prices_egg[,-which(colnames(prices_egg) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
# 
# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_meat))>1)
# {prices_meat<- prices_meat[,-which(colnames(prices_meat) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
# 
# if (sum(c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT') %in% colnames(prices_honey))>1)
# {prices_honey<- prices_honey[,-which(colnames(prices_honey) %in%c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'ID_HH', 'GPS_LAT','GPS_LON', 'GPS_ALT'))]}
 
 #--------
 
 
 
 
 
 
 #### Adding IDs and rounding GPS Co-ordinates ####
 
 YEAR<- rep(gsub('.*_','', ID_USED), nrow(core_data))
 COUNTRY_CODE<- rep(gsub('_.*','', ID_USED), nrow(core_data))
 PROJECT<- gsub(paste0('_',unique(YEAR)), '', ID_USED)
 PROJECT<- rep(gsub(paste0(unique(COUNTRY_CODE),'_'), '', PROJECT), nrow(core_data))
 
 ITERATION<-rep('1', nrow(core_data))
 
 ID_HH<- paste0(COUNTRY_CODE,'_', YEAR,'_',PROJECT,'_', row.names(core_data), '_',ITERATION)
 
 GPS_LAT<- round(as.numeric(as.character(core_data$X_GPS_latitude)), 2)
 GPS_LON<- round(as.numeric(as.character(core_data$X_GPS_longitude)), 2)
 GPS_ALT<- round(as.numeric(as.character(core_data$X_GPS_altitude)), 2)
 
 #---------------
 
 
 dir.create('Data_for_Sharing/Prices')
 dir.create('Data_for_Sharing/Extra_Outputs')
 
 #### Egg Price #####
 if ('ID_PROJ' %in% colnames(prices_egg)==F)
 {
   
   if ('chicken'%in% colnames(prices_egg)||'doves'%in% colnames(prices_egg)||'ducks'%in% colnames(prices_egg)||'geese'%in% colnames(prices_egg)||'otherpoultry'%in% colnames(prices_egg))
   {
     prices_egg<-data.frame(prices_egg[, grep('chicken|doves|ducks|geese|otherpoultry', colnames(prices_egg))])
   }else{prices_egg<- data.frame('chicken'=NA)}
   
   
   if ('chicken'%in% colnames(livestock_data)||'doves'%in% colnames(livestock_data)||'ducks'%in% colnames(livestock_data)||'geese'%in% colnames(livestock_data)||'otherpoultry'%in% colnames(livestock_data))
   {
     prices_egg<-data.frame(prices_egg[, grep('chicken|doves|ducks|geese|otherpoultry', colnames(prices_egg))])
   }else{prices_egg<- data.frame('chicken'=NA)}
   
   prices_egg<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,   prices_egg)
   colnames(prices_egg)<-gsub('.', ' ', colnames(prices_egg), fixed = T)
  write_csv(prices_egg,'Data_for_Sharing/Prices/egg_price_per_egg.csv')
}

if ('ID_PROJ' %in% colnames(prices_egg)==T)
{
  
  if ('chicken'%in% colnames(prices_egg)||'doves'%in% colnames(prices_egg)||'ducks'%in% colnames(prices_egg)||'geese'%in% colnames(prices_egg)||'otherpoultry'%in% colnames(prices_egg))
  {
    prices_egg<-data.frame(prices_egg[, grep('chicken|doves|ducks|geese|otherpoultry', colnames(prices_egg))])
  }else{prices_egg<- data.frame('chicken'=NA)}
  
  
  if ('chicken'%in% colnames(livestock_data)||'doves'%in% colnames(livestock_data)||'ducks'%in% colnames(livestock_data)||'geese'%in% colnames(livestock_data)||'otherpoultry'%in% colnames(livestock_data))
  {
    prices_egg<-data.frame(prices_egg[, grep('chicken|doves|ducks|geese|otherpoultry', colnames(prices_egg))])
  }else{prices_egg<- data.frame('chicken'=NA)}
  
  prices_egg<- data.frame(prices_egg)
  colnames(prices_egg)<-gsub('.', ' ', colnames(prices_egg), fixed = T)
  write_csv(prices_egg,'Data_for_Sharing/Prices/egg_price_per_egg.csv')
}
#---------------
#### Milk Price #####
if ('ID_PROJ' %in% colnames(prices_milk)==F)
{
  if ('chicken'%in% colnames(prices_milk)||'doves'%in% colnames(prices_milk)||'ducks'%in% colnames(prices_milk)||'geese'%in% colnames(prices_milk)||'otherpoultry'%in% colnames(prices_milk)||'bees'%in% colnames(prices_milk)||'donkeys_horses'%in% colnames(prices_milk)||'guinea pigs'%in% colnames(prices_milk)||'small_mammals'%in% colnames(prices_milk)||'rabbits'%in% colnames(prices_milk)||'pigs'%in% colnames(prices_milk)||'fish'%in% colnames(prices_milk)||'dog'%in% colnames(prices_milk)||'cats'%in% colnames(prices_milk))
  {
    prices_milk<-data.frame(prices_milk[, -grep('chicken|doves|ducks|geese|otherpoultry|bees|donkeys_horses|guinea pigs|small_mammals|rabbits|pigs|fish|dog|cats',colnames(prices_milk))])
  }else{prices_milk<- data.frame('cattle'=NA)}
  
  prices_milk<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,   prices_milk)
  colnames(prices_milk)<-gsub('.', ' ', colnames(prices_milk), fixed = T)
  write_csv(prices_milk,'Data_for_Sharing/Prices/milk_prices_per_l.csv')
}
if ('ID_PROJ' %in% colnames(prices_milk)==T)
{
  if ('chicken'%in% colnames(prices_milk)||'doves'%in% colnames(prices_milk)||'ducks'%in% colnames(prices_milk)||'geese'%in% colnames(prices_milk)||'otherpoultry'%in% colnames(prices_milk)||'bees'%in% colnames(prices_milk)||'donkeys_horses'%in% colnames(prices_milk)||'guinea pigs'%in% colnames(prices_milk)||'small_mammals'%in% colnames(prices_milk)||'rabbits'%in% colnames(prices_milk)||'pigs'%in% colnames(prices_milk)||'fish'%in% colnames(prices_milk)||'dog'%in% colnames(prices_milk)||'cats'%in% colnames(prices_milk))
  {
    prices_milk<-data.frame(prices_milk[, -grep('chicken|doves|ducks|geese|otherpoultry|bees|donkeys_horses|guinea pigs|small_mammals|rabbits|pigs|fish|dog|cats',colnames(prices_milk))])
  }else{prices_milk<- data.frame('cattle'=NA)}
  
  prices_milk<- data.frame(prices_milk)
  colnames(prices_milk)<-gsub('.', ' ', colnames(prices_milk), fixed = T)
  write_csv(prices_milk,'Data_for_Sharing/Prices/milk_prices_per_l.csv')
}
#--------------
#### Honey Price ####
if ('ID_PROJ' %in% colnames(prices_honey)==F)
{
  
  if ('bees'%in% colnames(prices_honey))
  {
    prices_honey<-data.frame('bees'=prices_honey[, grep('bees', colnames(prices_honey))])
  }else{prices_honey<- data.frame('bees'=NA)}
  prices_honey<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,   prices_honey)
  colnames(prices_honey)<-gsub('.', ' ', colnames(prices_honey), fixed = T)
  write_csv(prices_honey,'Data_for_Sharing/Prices/honey_price_per_litre.csv')
} 

if ('ID_PROJ' %in% colnames(prices_honey)==T)
{
  
  if ('bees'%in% colnames(prices_honey))
  {
    prices_honey<-data.frame('bees'=prices_honey[, grep('bees', colnames(prices_honey))])
  }else{prices_honey<- data.frame('bees'=NA)}
  prices_honey<- data.frame(prices_honey)
  colnames(prices_honey)<-gsub('.', ' ', colnames(prices_honey), fixed = T)
  write_csv(prices_honey,'Data_for_Sharing/Prices/honey_price_per_litre.csv')
} 

#--------------
#### Meat Price #####
if ('ID_PROJ' %in% colnames(prices_meat)==F)
{
  
  if ('bees'%in% colnames(prices_meat))
  {
    prices_meat<-data.frame(prices_meat[, -grep('bees',colnames(prices_meat))])
  }else{prices_meat<- data.frame('cattle'=NA)}
  
  prices_meat<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,   prices_meat)
  colnames(prices_meat)<-gsub('.', ' ', colnames(prices_meat), fixed = T)
  write_csv(prices_meat,'Data_for_Sharing/Prices/meat_prices_per_kg.csv')
}

if ('ID_PROJ' %in% colnames(prices_meat)==T)
{
  
  if ('bees'%in% colnames(prices_meat))
  {
    prices_meat<-data.frame(prices_meat[, -grep('bees',colnames(prices_meat))])
  }else{prices_meat<- data.frame('cattle'=NA)}
  
  prices_meat<- data.frame(prices_meat)
  colnames(prices_meat)<-gsub('.', ' ', colnames(prices_meat), fixed = T)
  write_csv(prices_meat,'Data_for_Sharing/Prices/meat_prices_per_kg.csv')
}
#--------------
#### Crop Price #####
if ('ID_PROJ' %in% colnames(prices_crop)==F)
{
  prices_crop<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,   prices_crop)
  colnames(prices_crop)<-gsub('.', ' ', colnames(prices_crop), fixed = T)
  write_csv(prices_crop,'Data_for_Sharing/Prices/crop_prices_per_kg.csv')
}

if ('ID_PROJ' %in% colnames(prices_crop)==T)
{
  prices_crop<- data.frame(prices_crop)
  colnames(prices_crop)<-gsub('.', ' ', colnames(prices_crop), fixed = T)
  write_csv(prices_crop,'Data_for_Sharing/Prices/crop_prices_per_kg.csv')
}
#--------------
#### Whole lvstk Price #####
if ('ID_PROJ' %in% colnames(prices_whole_livestock)==F)
{
  prices_whole_livestock<- data.frame('ID_PROJ'=unique(PROJECT),'ID_COUNTRY'=unique(COUNTRY_CODE),'YEAR'=unique(YEAR),'ITERATION'=unique(ITERATION),'SURVEY_ID'=ID_USED,  prices_whole_livestock)
  colnames(prices_whole_livestock)<-gsub('.', ' ', colnames(prices_whole_livestock), fixed = T)
  write_csv(prices_whole_livestock,'Data_for_Sharing/Prices/whole_livestock_prices.csv')
}

if ('ID_PROJ' %in% colnames(prices_whole_livestock)==T)
{
  prices_whole_livestock<- data.frame( prices_whole_livestock)
  colnames(prices_whole_livestock)<-gsub('.', ' ', colnames(prices_whole_livestock), fixed = T)
  write_csv(prices_whole_livestock,'Data_for_Sharing/Prices/whole_livestock_prices.csv')
}
#--------------
#### Crop Data #####
if ('ID_PROJ' %in% colnames(crop_data)==F)
{
  crop_data<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  crop_data)
  colnames(crop_data)<-gsub('.', ' ', colnames(crop_data), fixed = T)
  write_csv(crop_data,'Data/Extra_Outputs/crop_details.csv')
  write_csv(crop_data,'Data_for_Sharing/Extra_Outputs/crop_details.csv')
}
#--------------
#### Crop Residue Use #####
if ('ID_PROJ' %in% colnames(crop_residue_use)==F)
{
  crop_residue_use<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  crop_residue_use)
  colnames(crop_residue_use)<-gsub('.', ' ', colnames(crop_residue_use), fixed = T)
  write_csv(crop_residue_use,'Data/Extra_Outputs/crop_residue_use.csv')
  write_csv(crop_residue_use,'Data_for_Sharing/Extra_Outputs/crop_residue_use.csv')
}
#--------------
#### Crop Use ####
#if ('ID_PROJ' %in% colnames(crop_use)==F)
# {
#   crop_use<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  crop_use)
#   colnames(crop_use)<-gsub('.', ' ', colnames(crop_use), fixed = T)
#   write_csv(crop_use,'Data/Extra_Outputs/crop_use.csv')
#   write_csv(crop_use,'Data_for_Sharing/Extra_Outputs/crop_use.csv')
# }
#--------------
#### Crops Intercropped ####
# if ('ID_PROJ' %in% colnames(Crops_Intercropped)==F)
# {
#   Crops_Intercropped<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  Crops_Intercropped)
#   colnames(Crops_Intercropped)<-gsub('.', ' ', colnames(Crops_Intercropped), fixed = T)
#   write_csv(Crops_Intercropped,'Data/Extra_Outputs/Crops_Intercropped.csv')
#   write_csv(Crops_Intercropped,'Data_for_Sharing/Extra_Outputs/Crops_Intercropped.csv')
# }
# #--------------
# #### Crops Monocropped ####
# if ('ID_PROJ' %in% colnames(Crops_Monocropped)==F)
# {
#   Crops_Monocropped<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  Crops_Monocropped)
#   colnames(Crops_Monocropped)<-gsub('.', ' ', colnames(Crops_Monocropped), fixed = T)
#   write_csv(Crops_Monocropped,'Data/Extra_Outputs/Crops_Monocropped.csv')
#   write_csv(Crops_Monocropped,'Data_for_Sharing/Extra_Outputs/Crops_Monocropped.csv')
# }
#--------------
#### Livestock Data ####
if ('ID_PROJ' %in% colnames(livestock_data)==F)
{
  livestock_data<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  livestock_data)
  colnames(livestock_data)<-gsub('.', ' ', colnames(livestock_data), fixed = T)
  write_csv(livestock_data,'Data/Extra_Outputs/livestock_details.csv')
  write_csv(livestock_data,'Data_for_Sharing/Extra_Outputs/livestock_details.csv')
}
#--------------
#### Core Data Cleaning ####
if ('ID_PROJ' %in% colnames(core_data)==F)
{
  core_data<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  core_data)
  write_csv(core_data,'Data/Core_Data.csv')
  
  
  #### Moving Crop and Livestock Loops ######
  crop_cols<-c(which(colnames(core_data)=='crop_name_1'):which(colnames(core_data)=='crop_residue_use_8'))
  livestock_cols<-c(which(colnames(core_data)=='livestock_name_1'):which(colnames(core_data)=='bees_who_control_eating_5'))
  off_farm_cols<-c(which(colnames(core_data)=='offfarm_income_name_1'):which(colnames(core_data)=='offfarm_who_control_revenue_6'))
  
  
  crops_section_start<- which(colnames(core_data)=='crop_count')
  livestock_section_start<-which(colnames(core_data)=='livestock_count')
  off_farm_section_start<- which(colnames(core_data)=='offfarm_incomes_count')
  
  core_data<-data.frame(core_data[,1:crops_section_start], core_data[,crop_cols], core_data[,c((crops_section_start+1):livestock_section_start)], core_data[,livestock_cols], core_data[,c((livestock_section_start+1):off_farm_section_start)],core_data[,off_farm_cols],  core_data[,c((off_farm_section_start+1):ncol(core_data))])
  
  if (length(grep('\\.', colnames(core_data)))>0)
  {
    core_data[,grep('\\.', colnames(core_data))]<-NULL
  }
  
  #-------------
  
  #### Removing Extra Household Loops ####
  remove<-c(grep('person_age_', colnames(core_data)))
  remove<-c(remove,grep('hh_pop_rep_num_', colnames(core_data)))
  remove<-c(remove,grep('head_person_', colnames(core_data)))
  remove<-c(remove,grep('person_gender', colnames(core_data)))
  
  
  if (length(remove)>1)
  {
    core_data<-core_data[,-remove]
  }
  #------------
  #### Fixing Proportions ####
  #Crop
  length_temp<-length(grep("crop_name_", colnames(core_data)))
  
  for (i in 1:length_temp)
  {  
    
    for(j in 1:nrow(core_data))
    {
      if(length(unlist(strsplit(as.character(core_data[j,colnames(core_data)==paste0('crop_use_', i)]),' ')))==1)
      {
        if(!is.na(core_data[j,colnames(core_data)==paste0('crop_use_', i)]))
        {
          if (core_data[j,colnames(core_data)==paste0('crop_use_', i)]=="eat")
          {
            core_data[j,colnames(core_data)==paste0('crop_consumed_prop_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('crop_sold_prop_', i)]<-'none'
            core_data[j,colnames(core_data)==paste0('crop_feed_lstk_prop_', i)]<-'none'
          }
          
          if (core_data[j,colnames(core_data)==paste0('crop_use_', i)]=="sell")
          {
            core_data[j,colnames(core_data)==paste0('crop_sold_prop_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('crop_consumed_prop_', i)]<-'none'
            core_data[j,colnames(core_data)==paste0('crop_feed_lstk_prop_', i)]<-'none'
            
          }
          
          if (core_data[j,colnames(core_data)==paste0('crop_use_', i)]=="feed_livestock")
          {
            core_data[j,colnames(core_data)==paste0('crop_feed_lstk_prop_', i)]<-'all'  
            core_data[j,colnames(core_data)==paste0('crop_consumed_prop_', i)]<-'none'
            core_data[j,colnames(core_data)==paste0('crop_sold_prop_', i)]<-'none'
          }
        }
      }
    }
    
  }
  #Meat
  length_temp<-length(grep("livestock_name_", colnames(core_data)))
  
  for (i in 1:length_temp)
  {  
    
    for(j in 1:nrow(core_data))
    {
      if(length(unlist(strsplit(as.character(core_data[j,colnames(core_data)==paste0('meat_use_', i)]),' ')))==1)
      {
        if(!is.na(core_data[j,colnames(core_data)==paste0('meat_use_', i)]))
        {
          if (core_data[j,colnames(core_data)==paste0('meat_use_', i)]=="eat")
          {
            core_data[j,colnames(core_data)==paste0('meat_consumed_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('meat_sell_amount_', i)]<-'none'
          }
          
          if (core_data[j,colnames(core_data)==paste0('meat_use_', i)]=="sell")
          {
            core_data[j,colnames(core_data)==paste0('meat_sell_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('meat_consumed_amount_', i)]<-'none'
            
          }
          
        }
      }
    }
    
  }
  #Milk
  length_temp<-length(grep("livestock_name_", colnames(core_data)))
  
  for (i in 1:length_temp)
  {  
    
    for(j in 1:nrow(core_data))
    {
      if(length(unlist(strsplit(as.character(core_data[j,colnames(core_data)==paste0('milk_use_', i)]),' ')))==1)
      {
        if(!is.na(core_data[j,colnames(core_data)==paste0('milk_use_', i)]))
        {
          if (core_data[j,colnames(core_data)==paste0('milk_use_', i)]=="eat")
          {
            core_data[j,colnames(core_data)==paste0('milk_consumed_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('milk_sell_amount_', i)]<-'none'
          }
          
          if (core_data[j,colnames(core_data)==paste0('milk_use_', i)]=="sell")
          {
            core_data[j,colnames(core_data)==paste0('milk_sell_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('milk_consumed_amount_', i)]<-'none'
            
          }
          
        }
      }
    }
    
  }
  #Honey
  length_temp<-length(grep("livestock_name_", colnames(core_data)))
  
  for (i in 1:length_temp)
  {  
    
    for(j in 1:nrow(core_data))
    {
      if(length(unlist(strsplit(as.character(core_data[j,colnames(core_data)==paste0('bees_honey_use_', i)]),' ')))==1)
      {
        if(!is.na(core_data[j,colnames(core_data)==paste0('bees_honey_use_', i)]))
        {
          if (core_data[j,colnames(core_data)==paste0('bees_honey_use_', i)]=="eat")
          {
            core_data[j,colnames(core_data)==paste0('bees_honey_consumed_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('bees_honey_sell_amount_', i)]<-'none'
            
          }
          
          if (core_data[j,colnames(core_data)==paste0('bees_honey_use_', i)]=="sell")
          {
            core_data[j,colnames(core_data)==paste0('bees_honey_sell_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('bees_honey_consumed_amount_', i)]<-'none'
          }
          
        }
      }
    }
    
  }
  #Eggs
  length_temp<-length(grep("livestock_name_", colnames(core_data)))
  
  for (i in 1:length_temp)
  {  
    
    for(j in 1:nrow(core_data))
    {
      if(length(unlist(strsplit(as.character(core_data[j,colnames(core_data)==paste0('eggs_use_', i)]),' ')))==1)
      {
        if(!is.na(core_data[j,colnames(core_data)==paste0('eggs_use_', i)]))
        {
          if (core_data[j,colnames(core_data)==paste0('eggs_use_', i)]=="eat")
          {
            core_data[j,colnames(core_data)==paste0('eggs_consumed_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('eggs_sell_amount_', i)]<-'none'
            
          }
          
          if (core_data[j,colnames(core_data)==paste0('eggs_use_', i)]=="sell")
          {
            core_data[j,colnames(core_data)==paste0('eggs_sell_amount_', i)]<-'all'
            core_data[j,colnames(core_data)==paste0('eggs_consumed_amount_', i)]<-'none'
          }
          
        }
      }
    }
    
  }
  #------------
  
  core_data$HouseholdID<-NULL
  core_data$X_GPS_latitude<-round(as.numeric(as.character(core_data$X_GPS_latitude)),2)
  core_data$X_GPS_altitude<-round(as.numeric(as.character(core_data$X_GPS_altitude)),2)
  core_data$X_GPS_longitude<-round(as.numeric(as.character(core_data$X_GPS_longitude)),2)
  core_data$notes<-NULL
  core_data$X_notes<-NULL
  core_data$village<-NULL
  
  write_csv(core_data,'Data_for_Sharing/Core_Data.csv')
}

#--------------
#### Indicator Sheet Cleaning ####
if ('ID_PROJ' %in% colnames(indicator_sheet)==F)
{
  indicator_sheet<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  indicator_sheet)
  write_csv(indicator_sheet,'Data/indicator_sheet.csv')
  
  indicator_sheet$HHid<-NULL
  indicator_sheet$Village<-NULL
  indicator_sheet$Longitude<-NULL
  indicator_sheet$Latitude<-NULL
  indicator_sheet$TotalEnergyAvailable <-NULL
  indicator_sheet$FAEnergyBought <-NULL
  indicator_sheet$FAEnergyOffFarm <-NULL
  indicator_sheet$FAEnergyCropConsumption <-NULL
  indicator_sheet$FAEnergyCropSales <-NULL
  indicator_sheet$FAEnergyLivestockConsumption <-NULL
  indicator_sheet$FAEnergyLivestockSales <-NULL
  indicator_sheet$FAEnergyFarmBased <-NULL
  indicator_sheet$FAMarketOrientation <-NULL
  indicator_sheet$FALivestockOrientation <-NULL
  indicator_sheet$CropDiv<-NULL
  indicator_sheet$LivestockDiv<-NULL
  indicator_sheet$RelImpWildFoods<-NULL
  
  
  indicator_sheet$Market_Orientation<- (indicator_sheet$cropsales+indicator_sheet$livestockprodsales)/indicator_sheet$valuefarmproduce
  indicator_sheet$Livestock_Orientation<-indicator_sheet$valuelivestockproduction/indicator_sheet$valuefarmproduce
  
  
  
  indicator_sheet$Gender_MaleControl<-rowSums(data.frame(indicator_sheet$Gender_MaleControl,indicator_sheet$Gender_MaleYouthControl), na.rm = T)
  indicator_sheet$Gender_FemaleControl<-rowSums(data.frame(indicator_sheet$Gender_FemaleControl,indicator_sheet$Gender_FemaleYouthControl), na.rm = T)
  
  for (i in 1:nrow(dat_all))
  {
    if (!is.na(indicator_sheet$Gender_MaleControl[i])&&!is.na(indicator_sheet$Gender_FemaleControl[i]))
    {
      if ((indicator_sheet$Gender_MaleControl[i]+indicator_sheet$Gender_FemaleControl[i])!=1)
      {
        indicator_sheet$Gender_MaleControl[i]<-NA
        indicator_sheet$Gender_FemaleControl[i]<-NA
      }
    }
  }
  
  indicator_sheet$Gender_MaleYouthControl<-NULL
  indicator_sheet$Gender_FemaleYouthControl<-NULL
  
  
  if (sum(!is.na(core_data$FIES_1))==0&&sum(!is.na(core_data$FIES_2))==0&&sum(!is.na(core_data$FIES_3))==0&&sum(!is.na(core_data$FIES_4))==0&&sum(!is.na(core_data$FIES_5))==0&&sum(!is.na(core_data$FIES_6))==0&&sum(!is.na(core_data$FIES_7))==0&&sum(!is.na(core_data$FIES_8))==0)
  {
    indicator_sheet$FIES_Score<- rep(NA, nrow(core_data))
  }
  colnames(indicator_sheet)<- gsub('TVA_pmae_pday', 'TVA_USD_PPP_pmae_pday', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('total_income', 'total_income_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('offfarm_income', 'offfarm_income_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('\\<farm_income\\>', 'farm_income_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('valuefarmproduce', 'value_farm_produce_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('cropsales', 'crop_sales_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('valuecropproduce', 'value_crop_produce_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('valuecropconsumed', 'value_crop_consumed_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('livestockprodsales', 'livestock_prodsales_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('valuelivestockproduction', 'value_livestock_production_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('valuelivestockprodconsumed', 'value_livestock_prod_consumed_USD_PPP_pHH_Yr', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('FoodAvailability', 'Food_Availability_kCal_MAE_day', colnames(indicator_sheet))
  colnames(indicator_sheet)<- gsub('FoodSelfSufficiency', 'Food_Self_Sufficiency_kCal_MAE_day', colnames(indicator_sheet))
  
  insert_orientation<-which(colnames(indicator_sheet)=='value_livestock_prod_consumed_USD_PPP_pHH_Yr')
  indicator_sheet<- data.frame(indicator_sheet[,1:insert_orientation], 'Market_Orientation'=indicator_sheet$Market_Orientation, 'Livestock_Orientation'=indicator_sheet$Livestock_Orientation, indicator_sheet[,(1+insert_orientation):ncol(indicator_sheet)])
  indicator_sheet<- indicator_sheet[, -grep('\\.', colnames(indicator_sheet))]
  
  
  write_csv(indicator_sheet,'Data_for_Sharing/indicator_sheet.csv')
  
  
}
#--------------
if ('ID_PROJ' %in% colnames(clean_data)==F)
{
  clean_data<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION,'SURVEY_ID'=rep(ID_USED, nrow(crop_data)), 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  clean_data)
  write_csv(clean_data,'Data/Cleaned_Data.csv')
  
  
}

if ('ID_PROJ' %in% colnames(raw_dat)==F)
{
  raw_dat<- data.frame('ID_PROJ'=PROJECT,'ID_COUNTRY'=COUNTRY_CODE,'YEAR'=YEAR,'ITERATION'=ITERATION, 'ID_HH'=ID_HH,GPS_LAT,GPS_LON,GPS_ALT,  raw_dat)
  write_csv(raw_dat,'Data/Raw_Data.csv')
  
}
























