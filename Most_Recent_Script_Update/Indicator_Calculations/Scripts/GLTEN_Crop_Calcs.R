GLTEN_Crop_Loop<- data.frame(read_csv('Data/GLTEN_Extra_Crop_Loops.csv',na=c('n/a','<NA>', '-999')), check.names = F)

LandOwned<- data.frame(read_csv('Data/LandOwned.csv',na=c('n/a','<NA>', '-999')), check.names = F)


source("../../Most_Recent_Script_Update/Cleaning/Shorten_Column_Names.R")
short_column_names<- shortened_column_names(GLTEN_Crop_Loop)
colnames(GLTEN_Crop_Loop)<- short_column_names

dat_all<-GLTEN_Crop_Loop

colnames(dat_all)<- gsub('glten_','', colnames(dat_all))
#--------------------------------------

previous_crop_names<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv",na = c("n/a","<NA>", "999", 'NA')))#, fileEncoding="UTF-8", stringsAsFactors = FALSE)

for (i in 1:length(grep("crop_name_", colnames(dat_all))))
{
  
  for (j in 1:nrow(dat_all))
  {
    if(!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
    {
      
      
      if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other1")
      {
        dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other1[j] 
      }
      
      if (!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other2")
        {
          dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other2[j] 
        }
      }
      if (!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other3")
        {
          dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other3[j] 
        }
      }
    }
  }
  
  crop_names_temp<- unique(dat_all[, colnames(dat_all)==paste0("crop_name_",i)])
  
  
  for (j in 1:length(crop_names_temp))
  {
    if ( crop_names_temp[j]%in%previous_crop_names$Original_Spelling==FALSE && crop_names_temp[j]%in%previous_crop_names$Standardised_Spelling==FALSE)
    {
      previous_crop_names<- rbind(previous_crop_names, data.frame("Original_Spelling"=crop_names_temp[j], "Standardised_Spelling"=NA))
    }
  }
  
}

previous_crop_names<- write_csv(previous_crop_names,"../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv")



#-------------------------------------


tocorrect<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv", na = c('NA', 'N/A')))#,stringsAsFactors = F, na.strings = c('na', 'NA', 'N/A'))
totdat<-dat_all

for (j in 1:length(tocorrect[,1])) {
  if (length(grep('crops_other', colnames(dat_all)))==3)
    for (i in 1:3) {
      index<-as.character(totdat[,paste0('crops_other',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('crops_other',as.character(i))])
      totdat[,paste0('crops_other',as.character(i))][index]<-tocorrect[j,2]
    }
  for (i in 1:length(grep('crop_name_', colnames(totdat)))) {
    index<-as.character(totdat[,paste0('crop_name_',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('crop_name_',as.character(i))])
    totdat[,paste0('crop_name_',as.character(i))][index]<-tocorrect[j,2]
  }
  
  
}

dat_all<-totdat




#------------------------

length_temp<-length(grep("crop_name_", colnames(dat_all)))

unique_crops<-c()
for (crops in 1:length(grep("crop_name_", colnames(dat_all))))
{
  for (j in 1:nrow(dat_all))
  {
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]=="other1")
    {
      dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]<- dat_all$crops_other1[j] 
    }
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]=="other2")
    {
      dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]<- dat_all$crops_other2[j] 
    }
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]=="other3")
    {
      dat_all[j,colnames(dat_all)==paste0("crop_name_",crops)]<- dat_all$crops_other3[j] 
    }
  }
  temp<- dat_all[,colnames(dat_all)==paste0("crop_name_",crops)]
  unique_crops<-c(unique_crops, unique(as.character(temp)))
}    
unique_crops<- unique(unique_crops)
for (i in 1: nrow(dat_all))
{
  if ("other" %in% unique_crops)
  {
    unique_crops<- unique_crops[-grep("other", unique_crops)]
  }
}

if (length(unique_crops[-grep("NA", unique_crops)])!=0)
{
  unique_crops<- unique_crops[-grep("NA", unique_crops)]
}
unique_crops<- unique_crops[unique_crops!=""]

GLTEN_Crop_Loop<- dat_all

#### Long Season #####

dat_all<- GLTEN_Crop_Loop[,-grep('short_rains|all_year', colnames(GLTEN_Crop_Loop))]

colnames(dat_all)<- gsub('long_rains_', '', colnames(dat_all))
#SEARCH_Harvested_crop: follow variable created below for the remainder of the calculation
crop_yield_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg)<- unique_crops

crop_yield_kg_temp<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg_temp)<- unique_crops

# Looping through each of the crop repititions eg. crop_name_1, crop_consumed_prop_1.... crop_name_8, crop_consumed_prop_8
for (i in 1:length_temp)
{
  
  # creating temperary variables for each of the crop repititions 
  units_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_",i)]
  units_other_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_other_",i)]
  
  
  
  # Running through each of the rows #
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j] %in% colnames(crop_coefficients)==TRUE && units_column[j]!="other"&& units_column[j]!="NA")
      {units_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_column[j]]}
    }
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j]=="other")
      {units_column[j]<-NA}
    }
    
    if(units_other_column[j] %in% colnames(crop_coefficients)==TRUE&& units_other_column[j]!="NA")
    {units_other_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_other_column[j]]}
    
    
    #Taking into account if all of the crop is eaten, sold, or fed to lvstk
    
    crop_yield_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_yield_",i)])              
    crop_yield_unit_temp<- as.numeric(units_column[j])
    crop_yield_unit_other_temp<- as.numeric(units_other_column[j])
    
    
    #determining crop yield
    if (!is.na(as.numeric(crop_yield_unit_temp)))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_temp*as.numeric(crop_yield_temp)
    }
    if (!is.na(crop_yield_unit_other_temp))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_other_temp*as.numeric(crop_yield_temp)
    }
    
    #' Calculating the proportions of crops used for different purposes 
    #' assigning the crop values into a data frame with the appropriate crop names
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_yield_kg[j,colnames(crop_yield_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(crop_yield_kg_temp[j,i])
    }
  }
}

crop_yield_long_rains<- crop_yield_kg



#####Four key data frames for crops
# crop_yield_kg
# crop_sold_kg
# crop_consumed_kg
#' crop_fed_lvstk_kg  
#' Male_Crop_sold
#' Male_Youth_Crop_sold
#' Female_Crop_sold
#' Female_Youth_Crop_sold
#' Male_Crop_consumed
#' Male_Youth_Crop_consumed
#' Female_Crop_consumed
#' Female_Youth_Crop_consumed

#------------------

#### Crop land proportions and crop areas ###




crop_land_proportion<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_land_proportion)<- unique_crops

for (i in 1:length_temp)
{
  
  proportion_column<-as.character(dat_all[,colnames(dat_all)==paste0("crop_land_",i)])
  proportion_column<-tolower(proportion_column)
  for(j in 1:nrow(dat_all))
  {
    if(proportion_column[j] %in% colnames(prop_switch)==TRUE)
    {proportion_column[j]<- prop_switch[1,colnames(prop_switch)==proportion_column[j]]}
    land_prop_temp<-proportion_column[j]
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_land_proportion[j,colnames(crop_land_proportion)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(land_prop_temp)
      
    }
  }
}


crop_land<- apply(crop_land_proportion, 2, function (x) x*as.numeric(as.character(LandOwned[,1])))
crop_land<-as.data.frame(crop_land)

crop_land_long_rains<-crop_land
#SEARCH_Yield_Crop follow variable created below for remainder of calculation
crop_yield_per_ha<- data.frame(matrix(NA, ncol=ncol(crop_yield_kg), nrow=nrow(crop_yield_kg)))

for (i in 1:ncol(crop_yield_per_ha))
{
  crop_yield_per_ha[,i]<- crop_yield_kg[,i]/crop_land[,i]
  for (j in 1:length(crop_yield_kg[,i]))
  {
    if(is.infinite(crop_yield_per_ha[j,i]))
    {
      crop_yield_per_ha[j,i]<- NA
    }
  }
}

colnames(crop_yield_per_ha)<- colnames(crop_yield_kg)
crop_yield_per_ha_long_rains<-crop_yield_per_ha

#### Short Season #####

dat_all<- GLTEN_Crop_Loop[,-grep('long_rains|all_year', colnames(GLTEN_Crop_Loop))]

colnames(dat_all)<- gsub('short_rains_', '', colnames(dat_all))
#SEARCH_Harvested_crop: follow variable created below for the remainder of the calculation
crop_yield_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg)<- unique_crops

crop_yield_kg_temp<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg_temp)<- unique_crops

# Looping through each of the crop repititions eg. crop_name_1, crop_consumed_prop_1.... crop_name_8, crop_consumed_prop_8
for (i in 1:length_temp)
{
  
  # creating temperary variables for each of the crop repititions 
  units_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_",i)]
  units_other_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_other_",i)]
  
  
  
  # Running through each of the rows #
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j] %in% colnames(crop_coefficients)==TRUE && units_column[j]!="other"&& units_column[j]!="NA")
      {units_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_column[j]]}
    }
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j]=="other")
      {units_column[j]<-NA}
    }
    
    if(units_other_column[j] %in% colnames(crop_coefficients)==TRUE&& units_other_column[j]!="NA")
    {units_other_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_other_column[j]]}
    
    
    #Taking into account if all of the crop is eaten, sold, or fed to lvstk
    
    crop_yield_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_yield_",i)])              
    crop_yield_unit_temp<- as.numeric(units_column[j])
    crop_yield_unit_other_temp<- as.numeric(units_other_column[j])
    
    
    #determining crop yield
    if (!is.na(as.numeric(crop_yield_unit_temp)))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_temp*as.numeric(crop_yield_temp)
    }
    if (!is.na(crop_yield_unit_other_temp))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_other_temp*as.numeric(crop_yield_temp)
    }
    
    #' Calculating the proportions of crops used for different purposes 
    #' assigning the crop values into a data frame with the appropriate crop names
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_yield_kg[j,colnames(crop_yield_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(crop_yield_kg_temp[j,i])
    }
  }
}

crop_yield_short_rains<- crop_yield_kg



#####Four key data frames for crops
# crop_yield_kg
# crop_sold_kg
# crop_consumed_kg
#' crop_fed_lvstk_kg  
#' Male_Crop_sold
#' Male_Youth_Crop_sold
#' Female_Crop_sold
#' Female_Youth_Crop_sold
#' Male_Crop_consumed
#' Male_Youth_Crop_consumed
#' Female_Crop_consumed
#' Female_Youth_Crop_consumed

#------------------

#### Crop land proportions and crop areas ###






crop_land_proportion<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_land_proportion)<- unique_crops

for (i in 1:length_temp)
{
  
  proportion_column<-as.character(dat_all[,colnames(dat_all)==paste0("crop_land_",i)])
  proportion_column<-tolower(proportion_column)
  for(j in 1:nrow(dat_all))
  {
    if(proportion_column[j] %in% colnames(prop_switch)==TRUE)
    {proportion_column[j]<- prop_switch[1,colnames(prop_switch)==proportion_column[j]]}
    land_prop_temp<-proportion_column[j]
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_land_proportion[j,colnames(crop_land_proportion)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(land_prop_temp)
      
    }
  }
}


crop_land<- apply(crop_land_proportion, 2, function (x) x*as.numeric(as.character(LandOwned[,1])))
crop_land<-as.data.frame(crop_land)

crop_land_short_rains<-crop_land
#SEARCH_Yield_Crop follow variable created below for remainder of calculation
crop_yield_per_ha<- data.frame(matrix(NA, ncol=ncol(crop_yield_kg), nrow=nrow(crop_yield_kg)))

for (i in 1:ncol(crop_yield_per_ha))
{
  crop_yield_per_ha[,i]<- crop_yield_kg[,i]/crop_land[,i]
  for (j in 1:length(crop_yield_kg[,i]))
  {
    if(is.infinite(crop_yield_per_ha[j,i]))
    {
      crop_yield_per_ha[j,i]<- NA
    }
  }
}

colnames(crop_yield_per_ha)<- colnames(crop_yield_kg)
crop_yield_per_ha_short_rains<-crop_yield_per_ha


#### All_year #####

dat_all<- GLTEN_Crop_Loop[,-grep('short_rains|long_rains', colnames(GLTEN_Crop_Loop))]

colnames(dat_all)<- gsub('all_year_', '', colnames(dat_all))
#SEARCH_Harvested_crop: follow variable created below for the remainder of the calculation
crop_yield_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg)<- unique_crops

crop_yield_kg_temp<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg_temp)<- unique_crops

# Looping through each of the crop repititions eg. crop_name_1, crop_consumed_prop_1.... crop_name_8, crop_consumed_prop_8
for (i in 1:length_temp)
{
  
  # creating temperary variables for each of the crop repititions 
  units_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_",i)]
  units_other_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_unit_other_",i)]
  
  
  
  # Running through each of the rows #
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j] %in% colnames(crop_coefficients)==TRUE && units_column[j]!="other"&& units_column[j]!="NA")
      {units_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_column[j]]}
    }
    if(!is.na(units_column[j])==TRUE)
    {
      if(units_column[j]=="other")
      {units_column[j]<-NA}
    }
    
    if(units_other_column[j] %in% colnames(crop_coefficients)==TRUE&& units_other_column[j]!="NA")
    {units_other_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_other_column[j]]}
    
    
    #Taking into account if all of the crop is eaten, sold, or fed to lvstk
    
    crop_yield_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_yield_",i)])              
    crop_yield_unit_temp<- as.numeric(units_column[j])
    crop_yield_unit_other_temp<- as.numeric(units_other_column[j])
    
    
    #determining crop yield
    if (!is.na(as.numeric(crop_yield_unit_temp)))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_temp*as.numeric(crop_yield_temp)
    }
    if (!is.na(crop_yield_unit_other_temp))
    {
      crop_yield_kg_temp[j,i]<- crop_yield_unit_other_temp*as.numeric(crop_yield_temp)
    }
    
    #' Calculating the proportions of crops used for different purposes 
    #' assigning the crop values into a data frame with the appropriate crop names
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_yield_kg[j,colnames(crop_yield_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(crop_yield_kg_temp[j,i])
    }
  }
}

crop_yield_all_year<- crop_yield_kg


for (i in 1:ncol(crop_yield_all_year))
{
  for (j in 1:nrow(crop_yield_all_year))
  {
    if (is.na(crop_yield_all_year[j,i]))
    {
      if (!is.na(crop_yield_long_rains[j,i])|| !is.na(crop_yield_short_rains[j,i]))
      {
        temp<-sum(c(crop_yield_long_rains[j,i],crop_yield_short_rains[j,i]), na.rm=T)
        crop_yield_all_year[j,i]<-temp
      }
    }
  }
}

crop_yield_kg<-crop_yield_all_year



#####Four key data frames for crops
# crop_yield_kg
# crop_sold_kg
# crop_consumed_kg
#' crop_fed_lvstk_kg  
#' Male_Crop_sold
#' Male_Youth_Crop_sold
#' Female_Crop_sold
#' Female_Youth_Crop_sold
#' Male_Crop_consumed
#' Male_Youth_Crop_consumed
#' Female_Crop_consumed
#' Female_Youth_Crop_consumed

#------------------

#### Crop land proportions and crop areas ###






crop_land_proportion<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_land_proportion)<- unique_crops

for (i in 1:length_temp)
{
  
  proportion_column<-as.character(dat_all[,colnames(dat_all)==paste0("crop_land_",i)])
  proportion_column<-tolower(proportion_column)
  for(j in 1:nrow(dat_all))
  {
    if(proportion_column[j] %in% colnames(prop_switch)==TRUE)
    {proportion_column[j]<- prop_switch[1,colnames(prop_switch)==proportion_column[j]]}
    land_prop_temp<-proportion_column[j]
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_land_proportion[j,colnames(crop_land_proportion)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(land_prop_temp)
      
    }
  }
}


crop_land<- apply(crop_land_proportion, 2, function (x) x*as.numeric(as.character(LandOwned[,1])))
crop_land<-as.data.frame(crop_land)

crop_land_all_year<-crop_land

for (i in 1:ncol(crop_land_all_year))
{
  for (j in 1:nrow(crop_land_all_year))
  {
    if (is.na(crop_land_all_year[j,i]))
    {
      if (!is.na(crop_land_long_rains[j,i])|| !is.na(crop_land_short_rains[j,i]))
      {
        temp<-mean(c(crop_land_long_rains[j,i],crop_land_short_rains[j,i]), na.rm=T)
        crop_land_all_year[j,i]<-temp
      }
    }
  }
}

crop_land<-crop_land_all_year









#SEARCH_Yield_Crop follow variable created below for remainder of calculation
crop_yield_per_ha<- data.frame(matrix(NA, ncol=ncol(crop_yield_kg), nrow=nrow(crop_yield_kg)))

for (i in 1:ncol(crop_yield_per_ha))
{
  crop_yield_per_ha[,i]<- crop_yield_kg[,i]/crop_land[,i]
  for (j in 1:length(crop_yield_kg[,i]))
  {
    if(is.infinite(crop_yield_per_ha[j,i]))
    {
      crop_yield_per_ha[j,i]<- NA
    }
  }
}

colnames(crop_yield_per_ha)<- colnames(crop_yield_kg)
crop_yield_per_ha_all_year<-crop_yield_per_ha



dir.create('Data/GLTEN_Crop_Outputs')
write_csv(crop_yield_long_rains,'Data/GLTEN_Crop_Outputs/Harvest_Long_Rains.csv')
write_csv(crop_yield_per_ha_long_rains,'Data/GLTEN_Crop_Outputs/Yield_Long_Rains.csv')
write_csv(crop_land_long_rains,'Data/GLTEN_Crop_Outputs/Land_Long_Rains.csv')


write_csv(crop_yield_short_rains,'Data/GLTEN_Crop_Outputs/Harvest_Short_Rains.csv')
write_csv(crop_yield_per_ha_short_rains,'Data/GLTEN_Crop_Outputs/Yield_Short_Rains.csv')
write_csv(crop_land_short_rains,'Data/GLTEN_Crop_Outputs/Land_Short_Rains.csv')


write_csv(crop_yield_all_year,'Data/GLTEN_Crop_Outputs/Harvest_All_Year.csv')
write_csv(crop_yield_per_ha_all_year,'Data/GLTEN_Crop_Outputs/Yield_All_Year.csv')
write_csv(crop_land_all_year,'Data/GLTEN_Crop_Outputs/Land_All_Year.csv')



dat_all<-GLTEN_Crop_Loop

Crops_Intercropped<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Crops_Intercropped)<- unique_crops

Crops_Monocropped<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Crops_Monocropped)<- unique_crops


for (i in 1:length(grep('crop_intercrop_yn_', colnames(dat_all))))
{
  for (j in 1:nrow(dat_all))
  {
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      Crops_Intercropped[j,colnames(Crops_Intercropped)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-dat_all[j,colnames(dat_all)==paste0("crop_intercrop_yn_",i)]
      
    }
  }
}

Crops_Intercropped<-data.frame(lapply(Crops_Intercropped, function (x) gsub('N',FALSE,x)))
Crops_Intercropped<-data.frame(lapply(Crops_Intercropped, function (x) gsub('Y',TRUE,x)))

Crops_Monocropped<-data.frame(lapply(Crops_Intercropped, function (x) x==F))


write_csv(Crops_Intercropped,'Data/GLTEN_Crop_Outputs/Crops_Intercropped.csv')
write_csv(Crops_Monocropped,'Data/GLTEN_Crop_Outputs/Crops_Monocropped.csv')

