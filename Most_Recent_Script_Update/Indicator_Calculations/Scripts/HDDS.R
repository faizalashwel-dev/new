

if(unique(dat_all$HDDS_type)=='Fourteen_Groups')
{
  remove_cols<-c('GrainsRootsTubers_good_season',
                 'GrainsRootsTubers_bad_season',
                 'GrainsRootsTubers_last_month',
                 'GrainsRootsTubers_source_good',
                 'GrainsRootsTubers_source_bad',
                 'GrainsRootsTubers_source_last_month',
                 'Legumes_good_season',
                 'Legumes_bad_season',
                 'Legumes_last_month',
                 'Legumes_source_good',
                 'Legumes_source_bad',
                 'Legumes_source_last_month',
                 'Nuts_Seeds_good_season',
                 'Nuts_Seeds_bad_season',
                 'Nuts_Seeds_last_month',
                 'Nuts_Seeds_source_good',
                 'Nuts_Seeds_source_bad',
                 'Nuts_Seeds_source_last_month',
                 'Veg_Leafy_good_season',
                 'Veg_Leafy_bad_season',
                 'Veg_Leafy_last_month',
                 'Veg_Leafy_source_good',
                 'Veg_Leafy_source_bad',
                 'Veg_Leafy_source_last_month',
                 'VitA_Veg_Fruit_good_season',
                 'VitA_Veg_Fruit_bad_season',
                 'VitA_Veg_Fruit_last_month',
                 'VitA_Veg_Fruit_source_good',
                 'VitA_Veg_Fruit_source_bad',
                 'VitA_Veg_Fruit_source_last_month',
                 'Vegetables_good_season',
                 'Vegetables_bad_season',
                 'Vegetables_last_month',
                 'Vegetables_source_good',
                 'Vegetables_source_bad',
                 'Vegetables_source_last_month',
                 'Fruits_good_season',
                 'Fruits_bad_season',
                 'Fruits_last_month',
                 'Fruits_source_good',
                 'Fruits_source_bad',
                 'Fruits_source_last_month',
                 'Meat_good_season',
                 'Meat_bad_season',
                 'Meat_last_month',
                 'Meat_source_good',
                 'Meat_source_bad',
                 'Meat_source_last_month',
                 'Eggs_good_season',
                 'Eggs_bad_season',
                 'Eggs_last_month',
                 'Eggs_source_good',
                 'Eggs_source_bad',
                 'Eggs_source_last_month',
                 'Milk_Dairy_good_season',
                 'Milk_Dairy_bad_season',
                 'Milk_Dairy_last_month',
                 'Milk_Dairy_source_good',
                 'Milk_Dairy_source_bad',
                 'Milk_Dairy_source_last_month')
  
  dat_all<-dat_all[,-which(colnames(dat_all)%in%remove_cols)]  
}

if(unique(dat_all$HDDS_type)=='Ten_Groups')
{
  remove_cols<-c('grains_24hr',
                 'grains_good_season',
                 'grains_bad_season',
                 'grains_last_month',
                 'grains_source_good',
                 'grains_source_bad',
                 'grains_source_last_month',
                 'roots_tubers_24hr',
                 'roots_tubers_good_season',
                 'roots_tubers_bad_season',
                 'roots_tubers_last_month',
                 'roots_tubers_source_good',
                 'roots_tubers_source_bad',
                 'roots_tubers_source_last_month',
                 'pulses_24hr',
                 'pulses_good_season',
                 'pulses_bad_season',
                 'pulses_last_month',
                 'pulses_source_good',
                 'pulses_source_bad',
                 'pulses_source_last_month',
                 'nuts_seeds_24hr',
                 'nuts_seeds_good_season',
                 'nuts_seeds_bad_season',
                 'nuts_seeds_last_month',
                 'nuts_seeds_source_good',
                 'nuts_seeds_source_bad',
                 'nuts_seeds_source_last_month',
                 'milk_24hr',
                 'milk_good_season',
                 'milk_bad_season',
                 'milk_last_month',
                 'milk_source_good',
                 'milk_source_bad',
                 'milk_source_last_month',
                 'organ_meat_24hr',
                 'organ_meat_good_season',
                 'organ_meat_bad_season',
                 'organ_meat_last_month',
                 'organ_meat_source_good',
                 'organ_meat_source_bad',
                 'organ_meat_source_last_month',
                 'meat_poultry_24hr',
                 'meat_poultry_good_season',
                 'meat_poultry_bad_season',
                 'meat_poultry_last_month',
                 'meat_poultry_source_good',
                 'meat_poultry_source_bad',
                 'meat_poultry_source_last_month',
                 'fish_seafood_24hr',
                 'fish_seafood_good_season',
                 'fish_seafood_bad_season',
                 'fish_seafood_last_month',
                 'fish_seafood_source_good',
                 'fish_seafood_source_bad',
                 'fish_seafood_source_last_month',
                 'eggs_24hr',
                 'eggs_good_season',
                 'eggs_bad_season',
                 'eggs_last_month',
                 'eggs_source_good',
                 'eggs_source_bad',
                 'eggs_source_last_month',
                 'green_veg_24hr',
                 'green_veg_good_season',
                 'green_veg_bad_season',
                 'green_veg_last_month',
                 'green_veg_source_good',
                 'green_veg_source_bad',
                 'green_veg_source_last_month',
                 'vitA_veg_24hr',
                 'vitA_veg_good_season',
                 'vitA_veg_bad_season',
                 'vitA_veg_last_month',
                 'vitA_veg_source_good',
                 'vitA_veg_source_bad',
                 'vitA_veg_source_last_month',
                 'vitA_fruits_24hr',
                 'vitA_fruits_good_season',
                 'vitA_fruits_bad_season',
                 'vitA_fruits_last_month',
                 'vitA_fruits_source_good',
                 'vitA_fruits_source_bad',
                 'vitA_fruits_source_last_month',
                 'other_veg_24hr',
                 'other_veg_good_season',
                 'other_veg_bad_season',
                 'other_veg_last_month',
                 'other_veg_source_good',
                 'other_veg_source_bad',
                 'other_veg_source_last_month',
                 'other_fruits_24hr',
                 'other_fruits_good_season',
                 'other_fruits_bad_season',
                 'other_fruits_last_month',
                 'other_fruits_source_good',
                 'other_fruits_source_bad',
                 'other_fruits_source_last_month')
  
  dat_all<-dat_all[,-which(colnames(dat_all)%in%remove_cols)]  
  
  
}


if (dat_all$HDDS_type=='Fourteen_Groups')
{
  foodstuffs<- c("grains",
                 "roots_tubers",
                 "pulses",
                 "nuts_seeds",
                 "milk",
                 "organ_meat",
                 "meat_poultry",
                 "fish_seafood",
                 "eggs",
                 "green_veg",
                 "vitA_veg",
                 "vitA_fruits",
                 "other_veg",
                 "other_fruits")
  
}

if (dat_all$HDDS_type=='Ten_Groups')
{
  foodstuffs<- c("GrainsRootsTubers",
                 "Legumes",
                 "Nuts_Seeds",
                 "Veg_Leafy",
                 "VitA_Veg_Fruit",
                 "Vegetables",
                 "Fruits",
                 "Meat",
                 "Eggs",
                 "Milk_Dairy"
  )
  
}


#season<- c("good", "bad", "last_month", "24hr")
season<- c("good", "bad", "last_month")



# SEARCH_score_HDDS_GoodSeason: follow the variable created below to track it through the calculation
HDDS_Good<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_Good)<-foodstuffs

# SEARCH_score_HDDS_BadSeason: follow the variable created below to track it through the calculation
HDDS_Bad<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_Bad)<-foodstuffs

HDDS_Last_Month<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_Last_Month)<-foodstuffs

HDDS_24hr<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_24hr)<-foodstuffs



# SEARCH_score_HDDS_farmbasedGoodSeason: follow the variable created below to track it through the calculation
HDDS_FARM_Good<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_FARM_Good)<-foodstuffs

# SEARCH_score_HDDS_farmbasedBadSeason: follow the variable created below to track it through the calculation
HDDS_FARM_Bad<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_FARM_Bad)<-foodstuffs

HDDS_FARM_Last_Month<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_FARM_Last_Month)<-foodstuffs



# SEARCH_score_HDDS_purchasedGoodSeason: follow the variable created below to track it through the calculation
HDDS_PURCHASED_Good<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_PURCHASED_Good)<-foodstuffs

# SEARCH_score_HDDS_purchasedBadSeason: follow the variable created below to track it through the calculation
HDDS_PURCHASED_Bad<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_PURCHASED_Bad)<-foodstuffs

HDDS_PURCHASED_Last_Month<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
colnames(HDDS_PURCHASED_Last_Month)<-foodstuffs




# HDDS_FREE_Good<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
# colnames(HDDS_PURCHASED_Good)<-foodstuffs
# 
# HDDS_FREE_Bad<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
# colnames(HDDS_PURCHASED_Bad)<-foodstuffs
# 
# HDDS_FREE_Last_Month<-data.frame(matrix(NA, ncol=length(foodstuffs), nrow=nrow(dat_all)))
# colnames(HDDS_PURCHASED_Last_Month)<-foodstuffs




for (i in foodstuffs)
{
  temp<- dat_all[,grep(i, colnames(dat_all))]
  colnames(temp)<-gsub(paste0(i,'_'),'', colnames(temp))
  
  if (length(grep('24hr', colnames(temp)))>1)
  {
    temp_24hr<-gsub('Y', 1,temp$`24hr`)
    temp_24hr<-gsub('N', 0,temp_24hr)
    HDDS_24hr[,i]<- as.numeric(temp_24hr)
  }
  temp_HDDS_Good<-temp$good_season
  temp_HDDS_Good<- gsub('daily',1,temp_HDDS_Good)
  temp_HDDS_Good<- gsub('weekly',1,temp_HDDS_Good)
  temp_HDDS_Good<- gsub('never',0,temp_HDDS_Good)
  temp_HDDS_Good<- gsub('monthly',0,temp_HDDS_Good)
  HDDS_Good[,i]<-temp_HDDS_Good
  
  temp_HDDS_Bad<-temp$bad_season
  temp_HDDS_Bad<- gsub('daily',1,temp_HDDS_Bad)
  temp_HDDS_Bad<- gsub('weekly',1,temp_HDDS_Bad)
  temp_HDDS_Bad<- gsub('never',0,temp_HDDS_Bad)
  temp_HDDS_Bad<- gsub('monthly',0,temp_HDDS_Bad)
  HDDS_Bad[,i]<-temp_HDDS_Bad
  
  
  
  temp_HDDS_Last<-temp$last_month
  temp_HDDS_Last<- gsub('daily',1,temp_HDDS_Last)
  temp_HDDS_Last<- gsub('weekly',1,temp_HDDS_Last)
  temp_HDDS_Last<- gsub('never',0,temp_HDDS_Last)
  temp_HDDS_Last<- gsub('monthly',0,temp_HDDS_Last)
  HDDS_Last_Month[,i]<-temp_HDDS_Last
  
  for (j in 1:nrow(dat_all))
  {
    
    ###----- Good Season ####
    if(!is.na(HDDS_Good[j,i]))
    {
      
      if (HDDS_Good[j,i]==1)
      {
        if ('on-farm'%in%temp$source_good[j])
        {
          HDDS_FARM_Good[j,i]<-1
        }
        if ('bought'%in%temp$source_good[j])
        {
          HDDS_PURCHASED_Good[j,i]<-1
        }
      }
      
      if (HDDS_Good[j,i]==0)
      {
        if ('on-farm'%in%temp$source_good[j])
        {
          HDDS_FARM_Good[j,i]<-0
        }
        if ('bought'%in%temp$source_good[j])
        {
          HDDS_PURCHASED_Good[j,i]<-0
        }
      }
      
    }
    
    
    ###----- Bad Season ####
    
    if(!is.na(HDDS_Bad[j,i]))
    {
      
      if (HDDS_Bad[j,i]==1)
      {
        if ('on-farm'%in%temp$source_bad[j])
        {
          HDDS_FARM_Bad[j,i]<-1
        }
        if ('bought'%in%temp$source_bad[j])
        {
          HDDS_PURCHASED_Bad[j,i]<-1
        }
      }
      
      if (HDDS_Bad[j,i]==0)
      {
        if ('on-farm'%in%temp$source_bad[j])
        {
          HDDS_FARM_Bad[j,i]<-0
        }
        if ('bought'%in%temp$source_bad[j])
        {
          HDDS_PURCHASED_Bad[j,i]<-0
        }
      }
      
    }
    
    
    ###----- Good Season ####
    
    if(!is.na(HDDS_Last_Month[j,i]))
    {
      
      if (HDDS_Last_Month[j,i]==1)
      {
        if ('on-farm'%in%temp$source_last_month[j])
        {
          HDDS_FARM_Last_Month[j,i]<-1
        }
        if ('bought'%in%temp$source_last_month[j])
        {
          HDDS_PURCHASED_Last_Month[j,i]<-1
        }
      }
      
      if (HDDS_Last_Month[j,i]==0)
      {
        if ('on-farm'%in%temp$source_last_month[j])
        {
          HDDS_FARM_Last_Month[j,i]<-0
        }
        if ('bought'%in%temp$source_last_month[j])
        {
          HDDS_PURCHASED_Last_Month[j,i]<-0
        }
      }
      
    }
    
  }
}


##### Changing the groups ####

HDDS_Good<-data.frame(lapply(HDDS_Good, function(x) as.numeric(as.character(x))))
HDDS_Bad<-data.frame(lapply(HDDS_Bad, function(x) as.numeric(as.character(x))))
HDDS_Last_Month<-data.frame(lapply(HDDS_Last_Month, function(x) as.numeric(as.character(x))))
HDDS_24hr<-data.frame(lapply(HDDS_24hr, function(x) as.numeric(as.character(x))))
HDDS_FARM_Good<-data.frame(lapply(HDDS_FARM_Good, function(x) as.numeric(as.character(x))))
HDDS_FARM_Bad<-data.frame(lapply(HDDS_FARM_Bad, function(x) as.numeric(as.character(x))))
HDDS_FARM_Last_Month<-data.frame(lapply(HDDS_FARM_Last_Month, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Good<-data.frame(lapply(HDDS_PURCHASED_Good, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Bad<-data.frame(lapply(HDDS_PURCHASED_Bad, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Last_Month<-data.frame(lapply(HDDS_PURCHASED_Last_Month, function(x) as.numeric(as.character(x))))


for (j in 1:nrow(HDDS_Good))
{
  if (rowSums(HDDS_Good[j,], na.rm = T)==0 && rowSums(HDDS_Bad[j,], na.rm = T)==0)  
  {
    HDDS_Good[j,]<-HDDS_Last_Month[j,]
    HDDS_Bad[j,]<-HDDS_Last_Month[j,]
    
    HDDS_FARM_Good[j,]<- HDDS_FARM_Last_Month[j,]
    HDDS_FARM_Bad[j,]<-HDDS_FARM_Last_Month[j,]
    
    HDDS_PURCHASED_Bad[j,]<- HDDS_PURCHASED_Last_Month[j,]
    HDDS_PURCHASED_Good[j,]<- HDDS_PURCHASED_Last_Month[j,]
    
    
  }
  
}


# HDDS_Good
# HDDS_Bad
# HDDS_Last_Month
# HDDS_24hr
# 
# HDDS_FARM_Good
# HDDS_FARM_Bad
# HDDS_FARM_Last_Month
# 
# HDDS_PURCHASED_Good
# HDDS_PURCHASED_Bad
# HDDS_PURCHASED_Last_Month








# dir.create('14_Food_Groups')
# 
# write_csv(HDDS_Good,'14_food_Groups/HDDS_Good.csv')
# write_csv(HDDS_Bad,'14_food_Groups/HDDS_Bad.csv')
# write_csv(HDDS_Last_Month,'14_food_Groups/HDDS_Last_Month.csv')
# write_csv(HDDS_24hr,'14_food_Groups/HDDS_24hr.csv')
# 
# write_csv(HDDS_FARM_Good,'14_food_Groups/HDDS_FARM_Good.csv')
# write_csv(HDDS_FARM_Bad,'14_food_Groups/HDDS_FARM_Bad.csv')
# write_csv(HDDS_FARM_Last_Month,'14_food_Groups/HDDS_FARM_Last_Month.csv')
# 
# write_csv(HDDS_PURCHASED_Good,'14_food_Groups/HDDS_PURCHASED_Good.csv')
# write_csv(HDDS_PURCHASED_Bad,'14_food_Groups/HDDS_PURCHASED_Bad.csv')
# write_csv(HDDS_PURCHASED_Last_Month,'14_food_Groups/HDDS_PURCHASED_Last_Month.csv')



#dir.create('10_Food_Groups')


if (length(foodstuffs)==14)
{
  convert_HDDS<-function(temp){
    temp$grains_roots_tubers<- rowSums(data.frame(as.numeric(temp$grains), as.numeric(temp$roots_tubers)),na.rm = T)
    temp$grains_roots_tubers<-as.numeric(gsub('2',1,as.character(temp$grains_roots_tubers)))
    temp$grains<-NULL
    temp$roots_tubers<-NULL
    
    temp$meat_poultry_fish<- rowSums(data.frame(as.numeric(temp$meat_poultry), as.numeric(temp$organ_meat), as.numeric(temp$fish_seafood)),na.rm = T)
    temp$meat_poultry_fish<-as.numeric(gsub('2',1,as.character(temp$meat_poultry_fish)))
    temp$meat_poultry_fish<-as.numeric(gsub('3',1,as.character(temp$meat_poultry_fish)))
    temp$meat_poultry<-NULL
    temp$organ_meat<-NULL
    temp$fish_seafood<-NULL
    
    temp$vitA_fruit_veg<- rowSums(data.frame(as.numeric(temp$vitA_veg), as.numeric(temp$vitA_fruits)),na.rm = T)
    temp$vitA_fruit_veg<-as.numeric(gsub('2',1,as.character(temp$vitA_fruit_veg)))
    temp$vitA_veg<-NULL
    temp$vitA_fruits<-NULL
    
    return(temp)
  }
  
  HDDS_Good<-convert_HDDS(HDDS_Good)
  
  HDDS_Bad<-convert_HDDS(HDDS_Bad)
  HDDS_Last_Month<-convert_HDDS(HDDS_Last_Month)
  HDDS_24hr<-convert_HDDS(HDDS_24hr)
  HDDS_FARM_Good<-convert_HDDS(HDDS_FARM_Good)
  HDDS_FARM_Bad<-convert_HDDS(HDDS_FARM_Bad)
  HDDS_FARM_Last_Month<-convert_HDDS(HDDS_FARM_Last_Month)
  
  HDDS_PURCHASED_Good<-convert_HDDS(HDDS_PURCHASED_Good)
  HDDS_PURCHASED_Bad<-convert_HDDS(HDDS_PURCHASED_Bad)
  HDDS_PURCHASED_Last_Month<-convert_HDDS(HDDS_PURCHASED_Last_Month)
}

# write_csv(HDDS_Good,'10_Food_Groups/HDDS_Good.csv')
# write_csv(HDDS_Bad,'10_Food_Groups/HDDS_Bad.csv')
# write_csv(HDDS_Last_Month,'10_Food_Groups/HDDS_Last_Month.csv')
# write_csv(HDDS_24hr,'10_Food_Groups/HDDS_24hr.csv')
# 
# write_csv(HDDS_FARM_Good,'10_Food_Groups/HDDS_FARM_Good.csv')
# write_csv(HDDS_FARM_Bad,'10_Food_Groups/HDDS_FARM_Bad.csv')
# write_csv(HDDS_FARM_Last_Month,'10_Food_Groups/HDDS_FARM_Last_Month.csv')
# 
# write_csv(HDDS_PURCHASED_Good,'10_Food_Groups/HDDS_PURCHASED_Good.csv')
# write_csv(HDDS_PURCHASED_Bad,'10_Food_Groups/HDDS_PURCHASED_Bad.csv')
# write_csv(HDDS_PURCHASED_Last_Month,'10_Food_Groups/HDDS_PURCHASED_Last_Month.csv')



#dir.create('Collapsed_Scores')

HDDS_Good<-data.frame(lapply(HDDS_Good, function(x) as.numeric(as.character(x))))
HDDS_Bad<-data.frame(lapply(HDDS_Bad, function(x) as.numeric(as.character(x))))
HDDS_Last_Month<-data.frame(lapply(HDDS_Last_Month, function(x) as.numeric(as.character(x))))
HDDS_24hr<-data.frame(lapply(HDDS_24hr, function(x) as.numeric(as.character(x))))
HDDS_FARM_Good<-data.frame(lapply(HDDS_FARM_Good, function(x) as.numeric(as.character(x))))
HDDS_FARM_Bad<-data.frame(lapply(HDDS_FARM_Bad, function(x) as.numeric(as.character(x))))
HDDS_FARM_Last_Month<-data.frame(lapply(HDDS_FARM_Last_Month, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Good<-data.frame(lapply(HDDS_PURCHASED_Good, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Bad<-data.frame(lapply(HDDS_PURCHASED_Bad, function(x) as.numeric(as.character(x))))
HDDS_PURCHASED_Last_Month<-data.frame(lapply(HDDS_PURCHASED_Last_Month, function(x) as.numeric(as.character(x))))


Collapsed_scores<-data.frame('HDDS_Good'=rowSums(HDDS_Good,na.rm = T),
                             'HDDS_Bad'=rowSums(HDDS_Bad,na.rm = T),
                             'HDDS_Last_Month'=rowSums(HDDS_Last_Month,na.rm = T),
                             'HDDS_24hr'=rowSums(HDDS_24hr,na.rm = T),
                             'HDDS_FARM_Good'=rowSums(HDDS_FARM_Good,na.rm = T),
                             'HDDS_FARM_Bad'=rowSums(HDDS_FARM_Bad,na.rm = T),
                             'HDDS_FARM_Last_Month'=rowSums(HDDS_FARM_Last_Month,na.rm = T),
                             'HDDS_PURCHASED_Good'=rowSums(HDDS_PURCHASED_Good,na.rm = T),
                             'HDDS_PURCHASED_Bad'=rowSums(HDDS_PURCHASED_Bad,na.rm = T),
                             'HDDS_PURCHASED_Last_Month'=rowSums(HDDS_PURCHASED_Last_Month,na.rm = T)
                             
                             
)

score_HDDS_goodseason<- data.frame(Collapsed_scores$HDDS_Good)
score_HDDS_farmbasedGoodSeason<- data.frame(Collapsed_scores$HDDS_FARM_Good)
score_HDDS_purchasedGoodSeason<- data.frame(Collapsed_scores$HDDS_PURCHASED_Good)
score_HDDS_badseason<- data.frame(Collapsed_scores$HDDS_Bad)
score_HDDS_farmbasedBadSeason<- data.frame(Collapsed_scores$HDDS_FARM_Bad)
score_HDDS_purchasedBadSeason<- data.frame(Collapsed_scores$HDDS_PURCHASED_Bad)





#write_csv(Collapsed_scores,'Collapsed_Scores/Collapsed_scores.csv')





