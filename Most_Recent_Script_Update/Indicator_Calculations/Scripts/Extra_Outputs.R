dir.create("Indicator_Calculations/Outputs/Extra_Outpus")

####-----------------
#### Crop Outputs ####
crop_amount<- crop_yield_kg
colnames(crop_amount)<- paste0("Harvested_",colnames(crop_amount))

crop_consumed_amount<-crop_consumed_kg
colnames(crop_consumed_amount)<- paste0("Consumed_",colnames(crop_consumed_amount))


crop_sold_amount<-crop_sold_kg
colnames(crop_sold_amount)<- paste0("Sold_",colnames(crop_sold_amount))

crop_income_export<-crop_sold_income
colnames(crop_income_export)<- paste0("Income_",colnames(crop_income_export))


crop_yield_export<- crop_yield_per_ha
colnames(crop_yield_export)<- paste0("Yield_",colnames(crop_yield_export))

crop_land_export<- crop_land
colnames(crop_land_export)<- paste0("Land_",colnames(crop_land_export))

crop_use_export<- crop_use
colnames(crop_use_export)<- paste0("Use_",colnames(crop_use_export))

crop_intercrop_export<- Crops_Intercropped
colnames(crop_intercrop_export)<- paste0("Intercropped_",colnames(crop_intercrop_export))


crop_master_sheet<- data.frame(crop_amount,crop_consumed_amount, crop_sold_amount, crop_income_export, crop_yield_export, crop_land_export, crop_use_export, crop_intercrop_export)
####-----------------

####--------------------
#### Milk Variables ####
if ('cattle' %in% unique_livestock ||'sheep' %in% unique_livestock ||'goat' %in% unique_livestock||'camel' %in% unique_livestock ||'alpaca' %in% unique_livestock||'llama' %in% unique_livestock)
{
  milk_income_variables<-data.frame(milk_income_per_day[,grep("cattle|sheep|goat|camel|alpaca|llama", colnames(milk_income_per_day))])
  if (ncol(milk_income_variables)==1)
  {
    colnames(milk_income_variables)<-grep("cattle|sheep|goat|camelalpaca|llama", colnames(milk_income_per_day), value = T)
  }
  milk_income_per_year<- data.frame(lapply(milk_income_variables, function (x) 365*(as.numeric(x))))
  colnames(milk_income_per_year)<- paste0("Milk_Income_",colnames(milk_income_per_year) )
  
  
  milk_sold_variables<-data.frame(average_milk_sold_per_day[,grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_sold_per_day))])
  if (ncol(milk_sold_variables)==1)
  {
    colnames(milk_sold_variables)<-grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_sold_per_day), value = T)
  }
  milk_sold_per_year<- data.frame(lapply(milk_sold_variables, function (x) 365*(as.numeric(x))))
  colnames(milk_sold_per_year)<- paste0("Milk_Sold_",colnames(milk_sold_per_year) )
  
  
  milk_consumed_variables<-data.frame(average_milk_consumed_per_day[,grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_consumed_per_day))])
  if (ncol(milk_consumed_variables)==1)
  {
    colnames(milk_consumed_variables)<-grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_consumed_per_day), value = T)
  }
  milk_consumed_per_year<- data.frame(lapply(milk_consumed_variables, function (x) 365*(as.numeric(x))))
  colnames(milk_consumed_per_year)<- paste0("Milk_Consumed_",colnames(milk_consumed_per_year) )
  
  
  milk_collected_variables<-data.frame(average_milk_collected_per_day[,grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_collected_per_day))])
  if (ncol(milk_collected_variables)==1)
  {
    colnames(milk_collected_variables)<-grep("cattle|sheep|goat|camelalpaca|llama", colnames(average_milk_collected_per_day), value = T)
  }
  milk_collected_per_year<- data.frame(lapply(milk_collected_variables, function (x) 365*(as.numeric(x))))
  colnames(milk_collected_per_year)<- paste0("Milk_Amount_",colnames(milk_collected_per_year) )
}

if ('cattle' %in% unique_livestock==F&&'sheep' %in% unique_livestock==F &&'goat' %in% unique_livestock==F&&'camel' %in% unique_livestock==F&&'alpaca' %in% unique_livestock==F&&'llama' %in% unique_livestock==F)
{
  milk_income_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(milk_income_per_year)==c('cattle')
  
  milk_sold_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(milk_sold_per_year)==c('cattle')
  
  milk_consumed_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(milk_consumed_per_year)==c('cattle')
  
  milk_collected_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(milk_collected_per_year)==c('cattle')
  
  
}
####--------------------

####-------------------
#### Eggs Variables ####
if ('ducks' %in% unique_livestock ||'chicken' %in% unique_livestock ||'guineafowl' %in% unique_livestock||'otherpoultry' %in% unique_livestock || 'doves' %in% unique_livestock || 'geese' %in% unique_livestock)
{
  eggs_income_variables<- data.frame(eggs_sold_per_day_income[,grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_sold_per_day_income))])
  if (ncol(eggs_income_variables)==1)
  {
    colnames(eggs_income_variables)<-grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_sold_per_day_income), value = T)
  }
  eggs_income_per_year<- data.frame(lapply(eggs_income_variables, function (x) 365*(as.numeric(x))))
  colnames(eggs_income_per_year)<- paste0("Eggs_Income_",colnames(eggs_income_per_year) )
  
  
  eggs_sold_variables<- data.frame(eggs_sold_per_day[,grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_sold_per_day))])
  if (ncol(eggs_sold_variables)==1)
  {
    colnames(eggs_sold_variables)<-grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_sold_per_day), value = T)
  }
  eggs_sold_per_year<- data.frame(lapply(eggs_sold_variables, function (x) 365*(as.numeric(x))))
  colnames(eggs_sold_per_year)<- paste0("Eggs_Sold_",colnames(eggs_sold_per_year) )
  
  
  eggs_consumed_variables<- data.frame(eggs_consumed_per_day[,grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_consumed_per_day))])
  if (ncol(eggs_consumed_variables)==1)
  {
    colnames(eggs_consumed_variables)<-grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_consumed_per_day), value = T)
  }
  eggs_consumed_per_year<- data.frame(lapply(eggs_consumed_variables, function (x) 365*(as.numeric(x))))
  colnames(eggs_consumed_per_year)<- paste0("Eggs_Consumed_",colnames(eggs_consumed_per_year) )
  
  
  eggs_collected_variables<- data.frame(eggs_collected_per_day[,grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_collected_per_day))])
  if (ncol(eggs_collected_variables)==1)
  {
    colnames(eggs_collected_variables)<-grep("guineafowl|chicken|doves|ducks|geese|otherpoultry", colnames(eggs_collected_per_day), value = T)
  }
  eggs_collected_per_year<- data.frame(lapply(eggs_collected_variables, function (x) 365*(as.numeric(x))))
  colnames(eggs_collected_per_year)<- paste0("Eggs_Collected_",colnames(eggs_collected_per_year) )
}


if ('ducks' %in% unique_livestock==F&&'chicken' %in% unique_livestock==F &&'guineafowl' %in% unique_livestock==F&&'otherpoultry' %in% unique_livestock==F &&'geese' %in% unique_livestock==F&&'doves' %in% unique_livestock==F )
{
  eggs_sold_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(eggs_sold_per_year)==c('chicken')
  
  eggs_consumed_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(eggs_consumed_per_year)==c('chicken')
  
  eggs_collected_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(eggs_collected_per_year)==c('chicken')
  
  eggs_income_per_year<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
  colnames(eggs_income_per_year)==c('chicken')
  
  
}

####-------------------

####-------------------
#### Meat Variables ####


Meat_income_per_year<- meat_sold_income
colnames(Meat_income_per_year)<- paste0("Meat_Income_",colnames(Meat_income_per_year) )

if ('bees' %in% unique_livestock)
{
Meat_sold_per_year<- meat_sold_kg[,-grep("bees", colnames(meat_sold_kg))]
}else{Meat_sold_per_year<- meat_sold_kg}
colnames(Meat_sold_per_year)<- paste0("Meat_Sold_",colnames(Meat_sold_per_year) )

if ('bees' %in% unique_livestock)
{
Meat_consumed_per_year<-meat_consumed_kg[,-grep("bees", colnames(meat_consumed_kg))]
}else{Meat_consumed_per_year<-meat_consumed_kg}
colnames(Meat_consumed_per_year)<- paste0("Meat_Consumed_",colnames(Meat_consumed_per_year) )


if ('bees' %in% unique_livestock)
{
Meat_amount_per_year<- meat_amount_kg[,-grep("bees", colnames(meat_amount_kg))]
}else{Meat_amount_per_year<- meat_amount_kg}
colnames(Meat_amount_per_year)<- paste0("Meat_Amount_",colnames(Meat_amount_per_year))
####-------------------

####-----------------------------
#### Whole Livestock Variables ####
Livestock_Sold_per_year<- livestock_sold_number
colnames(livestock_sold_number)<- paste0("Whole_Livestock_Sold_Number_",colnames(livestock_sold_number))

Livestock_kept_number<- livestock_heads[,-grep("other", colnames(livestock_heads))]
colnames(Livestock_kept_number)<- paste0("Whole_Livestock_Kept_Number_",colnames(Livestock_kept_number))

Livestock_income_per_year<- livestock_sold_income
colnames(Livestock_income_per_year)<- paste0("Whole_Livestock_Sale_Income_",colnames(Livestock_income_per_year))

####-----------------------------

livestock_variables<-data.frame(Livestock_kept_number, 
                                livestock_sold_number, 
                                Livestock_income_per_year,
                                Meat_amount_per_year,
                                Meat_consumed_per_year,
                                Meat_sold_per_year,
                                Meat_income_per_year,
                                milk_collected_per_year,
                                milk_consumed_per_year,
                                milk_sold_per_year,
                                milk_income_per_year,
                                eggs_collected_per_year,
                                eggs_consumed_per_year,
                                eggs_sold_per_year,
                                eggs_income_per_year
)
####--------------------
#### Honey Variables ####
# Honey_sold_income_per_year<-data.frame(bees_honey_sold_income_per_year[,grep("bees", colnames(bees_honey_sold_income_per_year))])
# if(length(colnames(Honey_sold_income_per_year))==1)
# {
#   colnames(Honey_sold_income_per_year)<-c("Honey_Income")
# }
# 
# Honey_amount_per_year<-bees_honey_production_per_year[,grep("bees", colnames(bees_honey_production_per_year))]
# if(length(colnames(Honey_amount_per_year))==1)
# {
#   colnames(Honey_amount_per_year)<-c("Honey_Amount")
# }
# Honey_amount_consumed_per_year<- bees_honey_consumed_per_year[,grep("bees", colnames(bees_honey_consumed_per_year))]
# if(length(colnames(Honey_amount_consumed_per_year))==1)
# {
#   colnames(Honey_amount_consumed_per_year)<-c("Honey_Consumed")
# }
# Honey_amount_sold_per_year<- bees_honey_sold_per_year[,grep("bees", colnames(bees_honey_sold_per_year))]
# if(length(colnames(Honey_amount_sold_per_year))==1)
# {
#   colnames(Honey_amount_sold_per_year)<-c("Honey_Sold")
# }
####--------------------

####-----------------
#### Crop Inputs ####

# Nitrogen Fertiliser Amount
dat_all$fertiliser_crops
dat_all$fertiliser_amount
dat_all$fertiliser_units
dat_all$fertiliser_type
dat_all$fertiliser_units_other
dat_all$fertiliser_type_other
grep("fertiliser", colnames(dat_all), value=TRUE)
# Types of Nitrogen fertiliser used

# Crops To which nitrogen fertiliser was applied

# For manure, improved seeds ... just include the crops it was applied to

####-----------------

dir.create("Data/Extra_Outputs")

write_csv(crop_master_sheet,"Data/Extra_Outputs/crop_details.csv")# row.names = F, fileEncoding = 'UTF-8')

write_csv(livestock_variables,"Data/Extra_Outputs/livestock_details.csv")# row.names = F, fileEncoding = 'UTF-8')
#write.csv(,"Data/Extra_Outputs/crop_inputs", row.names = F)




write.csv(Crops_Monocropped,paste0("Data/","Extra_Outputs/Crops_Monocropped.csv"), row.names = F)
write.csv(Crops_Intercropped,paste0("Data/","Extra_Outputs/Crops_Intercropped.csv"), row.names = F)

write.csv(crop_residue_use,paste0("Data/","Extra_Outputs/crop_residue_use.csv"), row.names = F)

write.csv(crop_use,paste0("Data/","Extra_Outputs/crop_use.csv"), row.names = F)



