


# Crop Calculation scripts

#' A script which calculates crop yield -kg-, crop sold -kg-, crop consumed -kg-, crop used to feed livestock -kg- and income from crops -international $ / year-
#' This script also calculates the amounts, of the variables listed above, controlled by female, female_youth, male, male_youth


#
crop_yield_kg<-data.frame(read_csv('Data/GLTEN_Crop_Outputs/Harvest_All_Year.csv', na = c("n/a","<NA>", "999", 'NA')),check.names = F)

crop_yield_per_ha<-data.frame(read_csv('Data/GLTEN_Crop_Outputs/Yield_All_Year.csv', na = c("n/a","<NA>", "999", 'NA')),check.names = F)

crop_land<-data.frame(read_csv('Data/GLTEN_Crop_Outputs/Land_All_Year.csv', na = c("n/a","<NA>", "999", 'NA')),check.names = F)


Crops_Intercropped<-data.frame(read_csv('Data/GLTEN_Crop_Outputs/Crops_Intercropped.csv', na = c("n/a","<NA>", "999", 'NA')),check.names = F)

Crops_Monocropped<-data.frame(read_csv('Data/GLTEN_Crop_Outputs/Crops_Monocropped.csv', na = c("n/a","<NA>", "999", 'NA')),check.names = F)



### Investigating the different crops grown ####
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
unique_crops<-c(unique_crops)






#------------------
#### Crop sales and consumption ####

#Creating empty data frames to be filled with the for loop

#SEARCH_Harvested_crop: follow variable created below for the remainder of the calculation

crop_yield_kg<-crop_yield_kg[, which(colnames(crop_yield_kg)%in% unique_crops)]
crop_yield_kg<-crop_yield_kg[which(colnames(crop_yield_kg)%in% unique_crops)]

crop_yield_per_ha<-crop_yield_per_ha[, which(colnames(crop_yield_per_ha)%in% unique_crops)]
crop_yield_per_ha<-crop_yield_per_ha[which(colnames(crop_yield_per_ha)%in% unique_crops)]

crop_land<-crop_land[, which(colnames(crop_land)%in% unique_crops)]
crop_land<-crop_land[which(colnames(crop_land)%in% unique_crops)]


unique_crops<-unique_crops[which(unique_crops%in% colnames(crop_yield_kg))]


crop_yield_kg<-crop_yield_kg[,unique_crops]

crop_yield_per_ha<-crop_yield_per_ha[,unique_crops]
crop_land<-crop_land[,unique_crops]


#crop_yield_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
#colnames(crop_yield_kg)<- unique_crops

#SEARCH_Sold_Crop: follow the variable created below for the remainder of the calculation
crop_sold_kg<- data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_sold_kg)<- unique_crops

#SEARCH_Consumed_crop: follow the variable created below for the remainder of the calculation
crop_consumed_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_consumed_kg)<- unique_crops

crop_fed_lvstk_kg<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_fed_lvstk_kg)<- unique_crops

#SEARCH_Income_Crop: follow the variable created below for the remainder of the calculation
crop_sold_income<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_sold_income)<- unique_crops

crop_yield_kg_temp<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_yield_kg_temp)<- unique_crops



Male_Youth_Crop_sold<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Male_Youth_Crop_sold)<- unique_crops
Male_Youth_Crop_consumed<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Male_Youth_Crop_consumed)<- unique_crops
Male_Crop_sold<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Male_Crop_sold)<- unique_crops
Male_Crop_consumed<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Male_Crop_consumed)<- unique_crops
Female_Youth_Crop_sold<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Female_Youth_Crop_sold)<- unique_crops
Female_Youth_Crop_consumed<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Female_Youth_Crop_consumed)<- unique_crops
Female_Crop_sold<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Female_Crop_sold)<- unique_crops
Female_Crop_consumed<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(Female_Crop_consumed)<- unique_crops

# Looping through each of the crop repititions eg. crop_name_1, crop_consumed_prop_1.... crop_name_8, crop_consumed_prop_8
for (i in 1:length_temp)
{
  
  # creating temperary variables for each of the crop repititions 
 # units_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_units_",i)]
 # units_other_column<-dat_all[,colnames(dat_all)==paste0("crop_yield_units_other_",i)]
  consumed_prop_column<- dat_all[,colnames(dat_all)==paste0("crop_consumed_prop_",i)]
  consumed_prop_column<-tolower(gsub("NA",NA,consumed_prop_column))
  sold_prop_column<- dat_all[,colnames(dat_all)==paste0("crop_sold_prop_",i)]
  sold_prop_column<-tolower(gsub("NA",NA,sold_prop_column))
  feed_lstk_column<- dat_all[,colnames(dat_all)==paste0("crop_feed_lstk_prop_",i)]
  feed_lstk_column<-tolower(gsub("NA",NA,feed_lstk_column))
  sold_income_units_column<-dat_all[,colnames(dat_all)==paste0("crop_sold_price_quantityunits_",i)]
  sold_income_units_other_column<-dat_all[,colnames(dat_all)==paste0("crop_price_quantityunits_other_",i)]
  who_control_sold<- dat_all[,colnames(dat_all)==paste0("crop_who_control_revenue_",i)]
  who_control_sold <-tolower(gsub("NA",NA,who_control_sold))
  who_control_consume<- dat_all[,colnames(dat_all)==paste0("crop_consume_control_",i)]
  who_control_consume<-tolower(gsub("NA",NA,who_control_consume))
  
  use_column<- dat_all[,colnames(dat_all)==paste0("crop_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  
  # Running through each of the rows #
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    sold_income_character_temp<-sold_income_units_column[j]
    sold_income_other_character_temp<-sold_income_units_other_column[j]
    #if(!is.na(units_column[j])==TRUE)
    #{
    #  if(units_column[j] %in% colnames(crop_coefficients)==TRUE && units_column[j]!="other"&& units_column[j]!="NA")
    #  {units_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_column[j]]}
    #}
    #if(!is.na(units_column[j])==TRUE)
    #{
    #  if(units_column[j]=="other")
    #  {units_column[j]<-NA}
    #}
    # 
    # if(units_other_column[j] %in% colnames(crop_coefficients)==TRUE&& units_other_column[j]!="NA")
    # {units_other_column[j]<- crop_coefficients[1,colnames(crop_coefficients)==units_other_column[j]]}
    # 
    if(consumed_prop_column[j] %in% colnames(prop_switch)==TRUE&& consumed_prop_column[j]!="NA")
    {consumed_prop_column[j]<- prop_switch[1,colnames(prop_switch)==consumed_prop_column[j]]}
    
    if(sold_prop_column[j] %in% colnames(prop_switch)==TRUE && sold_prop_column[j]!="NA")
    {sold_prop_column[j]<- prop_switch[1,colnames(prop_switch)==sold_prop_column[j]]}
    
    if(feed_lstk_column[j] %in% colnames(prop_switch)==TRUE  && feed_lstk_column[j]!="NA")
    {feed_lstk_column[j]<- prop_switch[1,colnames(prop_switch)==feed_lstk_column[j]]}
    
    if(sold_income_units_column[j] %in% colnames(crop_sold_units_coefficients)==TRUE && sold_income_units_column[j]!="NA")
    {sold_income_units_column[j]<- crop_sold_units_coefficients[1,colnames(crop_sold_units_coefficients)==sold_income_units_column[j]]}
    
    if(sold_income_units_other_column[j] %in% colnames(crop_sold_units_coefficients)==TRUE && sold_income_units_other_column[j]!="NA")
    {sold_income_units_column[j]<- crop_sold_units_coefficients[1,colnames(crop_sold_units_coefficients)==sold_income_units_other_column[j]]}
    
    #Taking into account if all of the crop is eaten, sold, or fed to lvstk
    
    use_temp<- as.character(use_column[j])
    use_temp<- unlist(strsplit(use_temp, " "))
    
    if(length(use_temp)==1)
    {
      if(!is.na(use_temp))
      {
        if (use_temp=="eat")
        {
          consumed_prop_column[j]<-1
        }
        
        if (use_temp=="sell")
        {
          sold_prop_column[j]<-1
        }
        
        if (use_temp=="feed_livestock")
        {
          feed_lstk_column[j]<-1
        }
      }
      
    }
    
    
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    # for (gend in 1:length(gender_sold_temp))
    # {
    #   if(gender_sold_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_sold_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_sold_temp[gend]])}
    # }
    # 
    # #Changing the gender labels to all be consistent
    gender_consume_temp<-unlist(strsplit(as.character(who_control_consume[j]), " "))
    # for (gend in 1:length(gender_consume_temp))
    # {
    #   if(gender_consume_temp[gend] %in% colnames(Male_female_control_switch)==TRUE)
    #   {gender_consume_temp[gend]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==gender_consume_temp[gend]])}
    # }
    # 
    # if(who_control_sold[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_sold[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_sold[j]])}
    # 
    # if(who_control_consume[j] %in% colnames(Male_female_control_switch)==TRUE)
    # {who_control_consume[j]<- as.character(Male_female_control_switch[1,colnames(Male_female_control_switch)==who_control_consume[j]])}
    # 
  #  crop_yield_temp<-   as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_yield_",i)])              
  #  crop_yield_unit_temp<- as.numeric(units_column[j])
   # crop_yield_unit_other_temp<- as.numeric(units_other_column[j])
    
    
    #determining crop yield
    if (dat_all[j,colnames(dat_all)==paste0('crop_name_',i)]%in% unique_crops)
    {
      crop_yield_kg_temp[j,i]<- crop_yield_kg[j,colnames(crop_yield_kg)==dat_all[j,colnames(dat_all)==paste0('crop_name_',i)]]
    }
  # if (!is.na(crop_yield_unit_other_temp))
  #   {
  #     crop_yield_kg_temp[j,i]<- crop_yield_unit_other_temp*as.numeric(crop_yield_temp)
  #   }
    
    #' Calculating the proportions of crops used for different purposes 
    #' assigning the crop values into a data frame with the appropriate crop names
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_yield_kg[j,colnames(crop_yield_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(crop_yield_kg_temp[j,i])
      crop_sold_kg[j,colnames(crop_sold_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(sold_prop_column[j])*crop_yield_kg_temp[j,i]
      crop_consumed_kg[j,colnames(crop_consumed_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(consumed_prop_column[j])*crop_yield_kg_temp[j,i]
      crop_fed_lvstk_kg[j,colnames(crop_fed_lvstk_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(feed_lstk_column[j])*crop_yield_kg_temp[j,i]
      
      # if(!is.na(sold_income_units_column[j]) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
      # {
      #   if (as.character(sold_income_character_temp)=="total_income_per_year" || as.character(sold_income_character_temp)=="total")
      #   {
      #     crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])
      #   }
      #   
      #   
      #   
      #   if(!is.na(crop_yield_kg_temp[j,i]) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
      #   {
      #     if(!is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])/crop_yield_kg_temp[j,i]))
      #     {
      #     if(as.character(sold_income_character_temp)!="total_income_per_year" && as.character(sold_income_character_temp)!="total" && as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])/crop_yield_kg_temp[j,i]<10)
      #     {
      #       crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(sold_income_units_column[j])*as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])*crop_sold_kg[j,colnames(crop_sold_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]
      #     }
      #     }
      #   }
      #   
      #   
      #   if(!is.na(as.numeric(sold_income_units_column[j])) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
      #   {
      #     if(!is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])/crop_yield_kg_temp[j,i]))
      #     {
      #     if(as.character(sold_income_character_temp)!="total_income_per_year" && as.character(sold_income_character_temp)!="total" && as.character(sold_income_character_temp)!="other" && as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])*as.numeric(sold_income_units_column[j])>10)
      #     {
      #       crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])
      #     }
      #     }
      #   }
      #   if(!is.na(crop_yield_kg_temp[j,i]) && !is.na(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)]))
      #   {
      #     if(!is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])/crop_yield_kg_temp[j,i]))
      #     {
      #     if(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])/crop_yield_kg_temp[j,i]>10)
      #     {
      #       crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])
      #     }
      #     }
      #   }
      #   
      
      if(!is.na(sold_income_units_column[j]) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
      {
        if (as.character(sold_income_character_temp)=="total_income_per_year" || as.character(sold_income_character_temp)=="total")
        {
          crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])
        }
        
        
        
        if(!is.na(crop_sold_kg[j,colnames(crop_sold_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
        {
          
          if(as.character(sold_income_character_temp)!="total_income_per_year" && as.character(sold_income_character_temp)!="total")
          {
            crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(sold_income_units_column[j])*as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])*as.numeric(crop_sold_kg[j,colnames(crop_sold_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]])
          }
          
        }
        
        
        # if(!is.na(as.numeric(sold_income_units_column[j])) && !is.na(as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])))
        # {
        #  
        #     if(as.character(sold_income_character_temp)!="total_income_per_year" || as.character(sold_income_character_temp)!="total")
        #     {
        #       crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(dat_all[j,colnames(dat_all)==paste0("crop_sold_income_",i)])*as.numeric(sold_income_units_column)*as.numeric(as.character(crop_sold_kg[j,colnames(crop_sold_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]
        #     }
        #   }
        #   
        
        
      }
      
      
      
      
      
      # gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
      # gender_consume_temp<- unlist(strsplit(as.character(who_control_consume[j]), " "))
      sold<-crop_sold_income[j,colnames(crop_sold_income)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]
      consumed<- crop_consumed_kg[j,colnames(crop_consumed_kg)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]
      if("female_youth" %in% gender_consume_temp==TRUE)
      {Female_Youth_Crop_consumed[j,colnames(Female_Youth_Crop_consumed)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_adult" %in% gender_consume_temp==TRUE |"female_head" %in% gender_consume_temp==TRUE)
      {Female_Crop_consumed[j,colnames(Female_Crop_consumed)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_youth" %in% gender_consume_temp==TRUE)
      {Male_Youth_Crop_consumed[j,colnames(Male_Youth_Crop_consumed)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("male_adult" %in% gender_consume_temp==TRUE | "male_head" %in% gender_consume_temp==TRUE)
      {Male_Crop_consumed[j,colnames(Male_Crop_consumed)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-consumed/length(gender_consume_temp)}
      if("female_youth" %in% gender_sold_temp==TRUE)
      {Female_Youth_Crop_sold[j,colnames(Female_Youth_Crop_sold)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-sold/length(gender_sold_temp)}
      if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
      {Female_Crop_sold[j,colnames(Female_Crop_sold)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-sold/length(gender_sold_temp)}
      if("male_youth" %in% gender_sold_temp==TRUE)
      {Male_Youth_Crop_sold[j,colnames(Male_Youth_Crop_sold)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-sold/length(gender_sold_temp)}
      if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
      {Male_Crop_sold[j,colnames(Male_Crop_sold)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-sold/length(gender_sold_temp)}
    }
    
    
  }
}



crops_income<- data.frame(apply(crop_sold_income, 2, function (x) as.numeric(as.character(x))))
crops_income<- data.frame("crops_income"=rowSums(crop_sold_income, na.rm = T))
female_crops_income<-data.frame(apply(Female_Crop_sold, 2, function (x) as.numeric(x)))
female_crops_income<- data.frame("female_crops_income"=rowSums(female_crops_income, na.rm = T))
female_youth_crops_income<-data.frame(apply(Female_Youth_Crop_sold, 2, function (x) as.numeric(x)))
female_youth_crops_income<- data.frame("female_Youth_crops_income"=rowSums(female_youth_crops_income, na.rm = T))
male_crops_income<-data.frame(apply(Male_Crop_sold, 2, function (x) as.numeric(x)))
male_crops_income<- data.frame("male_crops_income"=rowSums(male_crops_income, na.rm = T))
male_youth_crops_income<-data.frame(apply(Male_Youth_Crop_sold, 2, function (x) as.numeric(x)))
male_youth_crops_income<- data.frame("male_crops_income"=rowSums(male_youth_crops_income, na.rm = T))



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




# crop_land_proportion<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
# colnames(crop_land_proportion)<- unique_crops
# 
# for (i in 1:length_temp)
# {
#   
#   proportion_column<-as.character(dat_all[,colnames(dat_all)==paste0("crop_land_area_",i)])
#   proportion_column<-tolower(proportion_column)
#   for(j in 1:nrow(dat_all))
#   {
#     if(proportion_column[j] %in% colnames(prop_switch)==TRUE)
#     {proportion_column[j]<- prop_switch[1,colnames(prop_switch)==proportion_column[j]]}
#     land_prop_temp<-proportion_column[j]
#     
#     if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
#     {
#       crop_land_proportion[j,colnames(crop_land_proportion)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.numeric(land_prop_temp)
#       
#     }
#   }
# }
# 
# crop_land<- apply(crop_land_proportion, 2, function (x) x*LandOwned)
# crop_land<-as.data.frame(crop_land)
# 
# #SEARCH_Yield_Crop follow variable created below for remainder of calculation
# crop_yield_per_ha<- data.frame(matrix(NA, ncol=ncol(crop_yield_kg), nrow=nrow(crop_yield_kg)))
# 
# for (i in 1:ncol(crop_yield_per_ha))
# {
#   crop_yield_per_ha[,i]<- crop_yield_kg[,i]/crop_land[,i]
#   for (j in 1:length(crop_yield_kg[,i]))
#   {
#     if(is.infinite(crop_yield_per_ha[j,i]))
#     {
#       crop_yield_per_ha[j,i]<- NA
#     }
#   }
# }
# 
# colnames(crop_yield_per_ha)<- colnames(crop_yield_kg)




crop_use<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_use)<- unique_crops

for (i in 1:length_temp)
{
  
  use_column<- dat_all[,colnames(dat_all)==paste0("crop_use_",i)]
  use_column<-tolower(gsub("NA",NA,use_column))
  use_column<-gsub("c(","",use_column, fixed=T)
  use_column<-gsub("[[:punct:]]","",use_column)
  
  
  
  for(j in 1:nrow(dat_all))
  {
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_use[j,colnames(crop_use)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.character(use_column[j])
      
    }
  }
}


crop_residue_use<-data.frame(matrix(NA,ncol=length(unique_crops), nrow = nrow(dat_all)))
colnames(crop_residue_use)<- unique_crops

for (i in 1:length_temp)
{
  
  residue_use_column<- dat_all[,colnames(dat_all)==paste0("crop_residue_use_",i)]
  residue_use_column<-tolower(gsub("NA",NA,residue_use_column))
  residue_use_column<-gsub("c(","",residue_use_column, fixed=T)
  residue_use_column<-gsub("[[:punct:]]","",residue_use_column)
  
  
  
  for(j in 1:nrow(dat_all))
  {
    
    if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
    {
      crop_residue_use[j,colnames(crop_residue_use)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-as.character(residue_use_column[j])
      
    }
  }
}


# Crops_Intercropped<-data.frame(matrix(FALSE,ncol=length(unique_crops), nrow = nrow(dat_all)))
# colnames(Crops_Intercropped)<- unique_crops
# 
# Crops_Monocropped<-data.frame(matrix(FALSE,ncol=length(unique_crops), nrow = nrow(dat_all)))
# colnames(Crops_Monocropped)<- unique_crops
# 
# for (i in 1:length_temp)
# {
#   
#   intercropping<- dat_all[,colnames(dat_all)==paste0("crop_intercrop_",i)]
#   intercropping<-tolower(gsub("NA",NA,intercropping))
#   intercropping<-gsub("c(","",intercropping, fixed=T)
#   intercropping<-gsub("[[:punct:]]","",intercropping)
#   
#   
#   
#   for(j in 1:nrow(dat_all))
#   {
#     
#     if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)] %in% unique_crops==TRUE)
#     {
#       if (length(grep("intercrop", intercropping[j]))>0)
#       {
#         Crops_Intercropped[j,colnames(Crops_Intercropped)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-TRUE
#       }
#       if (length(grep("monoculture", intercropping[j]))>0)
#       {
#         Crops_Monocropped[j,colnames(Crops_Monocropped)==dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]]<-TRUE
#       }
#     }
#   }
# }

