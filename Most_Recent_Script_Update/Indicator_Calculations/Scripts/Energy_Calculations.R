# Energy Calculatuions

#### Energy Values ####
#' FoodAvailability, 
#' FoodSelfSufficiency, 
#' TotalEnergyAvailable, 
#' FAEnergyBought, 
#' FAEnergyOffFarm, 
#' FAEnergyCropConsumption, 
#' FAEnergyCropSales, 
#' FAEnergyLivestockConsumption,
#' FAEnergyLivestockSales
#' FAEnergyFarmBased
#' FAMarketOrientation
#' FALivestockOrientation
#'     

#crops, livestock, meat, honey, milk, eggs, 
#HHsizeMAE

#SEARCH_Market_Orientation
#SEARCH_Livestock_Orientation
#SEARCH_Food_Availability_kCal_MAE_day
#SEARCH_Food_Self_Sufficiency_kCal_MAE_day



crop_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(crop_consumed_kg), nrow=nrow(crop_consumed_kg)))
colnames(crop_Eval_consumed_day)<- colnames(crop_consumed_kg)

male_youth_crop_eval_consumed_day<-data.frame(matrix(NA, ncol=ncol(Male_Youth_Crop_consumed), nrow=nrow(Male_Youth_Crop_consumed)))
colnames(male_youth_crop_eval_consumed_day)<- colnames(Male_Youth_Crop_consumed)
female_youth_crop_eval_consumed_day<-data.frame(matrix(NA, ncol=ncol(Female_Youth_Crop_consumed), nrow=nrow(Female_Youth_Crop_consumed)))
colnames(female_youth_crop_eval_consumed_day)<- colnames(Female_Youth_Crop_consumed)
female_crop_eval_consumed_day<-data.frame(matrix(NA, ncol=ncol(Female_Crop_consumed), nrow=nrow(Female_Crop_consumed)))
colnames(female_crop_eval_consumed_day)<- colnames(Female_Crop_consumed)
male_crop_eval_consumed_day<-data.frame(matrix(NA, ncol=ncol(Male_Crop_consumed), nrow=nrow(Male_Crop_consumed)))
colnames(male_crop_eval_consumed_day)<- colnames(Male_Crop_consumed)

crop_energy_table_temp<- crop_energy_table[is.na(crop_energy_table[,1])==0,]
for(i in 1:length(colnames(crop_consumed_kg)))
{
  if(colnames(crop_Eval_consumed_day)[i] %in% as.character(crop_energy_table_temp[,1])==TRUE)
  {
    for (j in 1: nrow(crop_Eval_consumed_day))
    {
      
      crop_Eval_consumed_day[j,i]<- (1/365)*unique(crop_energy_table_temp[as.character(crop_energy_table_temp[,1])==as.character(colnames(crop_Eval_consumed_day)[i]),2])*crop_consumed_kg[j,i]
      male_youth_crop_eval_consumed_day[j,i]<- (1/365)*unique(crop_energy_table_temp[as.character(crop_energy_table_temp[,1])==as.character(colnames(male_youth_crop_eval_consumed_day)[i]),2])*Male_Youth_Crop_consumed[j,i]
      female_youth_crop_eval_consumed_day[j,i]<- (1/365)*unique(crop_energy_table_temp[as.character(crop_energy_table_temp[,1])==as.character(colnames(female_youth_crop_eval_consumed_day)[i]),2])*Female_Youth_Crop_consumed[j,i]
      female_crop_eval_consumed_day[j,i]<- (1/365)*unique(crop_energy_table_temp[as.character(crop_energy_table_temp[,1])==as.character(colnames(female_crop_eval_consumed_day)[i]),2])*Female_Crop_consumed[j,i]
      male_crop_eval_consumed_day[j,i]<- (1/365)*unique(crop_energy_table_temp[as.character(crop_energy_table_temp[,1])==as.character(colnames(male_crop_eval_consumed_day)[i]),2])*Male_Crop_consumed[j,i]
    }
  }
}



lvst_products_temp<- c("eggs", "honey", "milk", "cheese", "butter")
energy_per_kilo_lvs_products_temp<- c(516, 2000, 597, 7200, 4000)
lvst_prod_energy_temp<- cbind(lvst_products_temp, energy_per_kilo_lvs_products_temp)
livestock_energy_table_temp<-rbind(weight_killed_animals_set[,-2], lvst_prod_energy_temp)
colnames(livestock_energy_table_temp)<- c("livestock_products","calories_per_kilogram")

products_df_temp<-data.frame("eggs"=(0.048)*rowSums(eggs_consumed_per_day, na.rm=TRUE),
                             "milk"= rowSums(average_milk_consumed_per_day, na.rm=TRUE),
                             "honey"= (1/365)*rowSums(bees_honey_consumed_per_year, na.rm = TRUE))
#"cheese"= cheese_consumed_per_day[,1],
#"butter"= butter_consumed_per_day[,1])

products_Male_Youth_df_temp<-data.frame("eggs"=(0.048)*rowSums(Male_Youth_eggs_consumed, na.rm=TRUE),
                                        "milk"= rowSums(Male_Youth_milk_consumed, na.rm=TRUE),
                                        "honey"= (1/365)*rowSums(Male_Youth_bees_honey_consumed, na.rm = TRUE))
products_Female_Youth_df_temp<-data.frame("eggs"=(0.048)*rowSums(Female_Youth_eggs_consumed, na.rm=TRUE),
                                          "milk"= rowSums(Female_Youth_milk_consumed, na.rm=TRUE),
                                          "honey"= (1/365)*rowSums(Female_Youth_bees_honey_consumed, na.rm = TRUE))
products_Female_df_temp<-data.frame("eggs"=(0.048)*rowSums(Female_eggs_consumed, na.rm=TRUE),
                                    "milk"= rowSums(Female_milk_consumed, na.rm=TRUE),
                                    "honey"= (1/365)*rowSums(Female_bees_honey_consumed, na.rm = TRUE))
products_Male_df_temp<-data.frame("eggs"=(0.048)*rowSums(Male_eggs_consumed, na.rm=TRUE),
                                  "milk"= rowSums(Male_milk_consumed, na.rm=TRUE),
                                  "honey"= (1/365)*rowSums(Male_bees_honey_consumed, na.rm = TRUE))




meat_per_day_temp<- data.frame(lapply(meat_consumed_kg, function(x) x/365))
livestock_prod_consumed_per_day<- data.frame(meat_per_day_temp, products_df_temp)

meat_per_day_temp<- data.frame(lapply(Male_Youth_meat_consumed, function(x) x/365))
male_youth_livestock_prod_consumed_per_day<- data.frame(meat_per_day_temp, products_Male_Youth_df_temp)
meat_per_day_temp<- data.frame(lapply(Female_Youth_meat_consumed, function(x) x/365))
female_youth_livestock_prod_consumed_per_day<- data.frame(meat_per_day_temp, products_Female_Youth_df_temp)
meat_per_day_temp<- data.frame(lapply(Female_meat_consumed, function(x) x/365))
female_livestock_prod_consumed_per_day<- data.frame(meat_per_day_temp, products_Female_df_temp)
meat_per_day_temp<- data.frame(lapply(Male_meat_consumed, function(x) x/365))
male_livestock_prod_consumed_per_day<- data.frame(meat_per_day_temp, products_Male_df_temp)

livestock_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(livestock_prod_consumed_per_day), nrow=nrow(livestock_prod_consumed_per_day)))
colnames(livestock_Eval_consumed_day)<- colnames(livestock_prod_consumed_per_day)

male_youth_livestock_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(male_youth_livestock_prod_consumed_per_day), nrow=nrow(male_youth_livestock_prod_consumed_per_day)))
colnames(male_youth_livestock_Eval_consumed_day)<- colnames(male_youth_livestock_prod_consumed_per_day)
female_youth_livestock_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(female_youth_livestock_prod_consumed_per_day), nrow=nrow(female_youth_livestock_prod_consumed_per_day)))
colnames(female_youth_livestock_Eval_consumed_day)<- colnames(female_youth_livestock_prod_consumed_per_day)
female_livestock_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(female_livestock_prod_consumed_per_day), nrow=nrow(female_livestock_prod_consumed_per_day)))
colnames(female_livestock_Eval_consumed_day)<- colnames(female_livestock_prod_consumed_per_day)
male_livestock_Eval_consumed_day<- data.frame(matrix(NA, ncol=ncol(male_livestock_prod_consumed_per_day), nrow=nrow(male_livestock_prod_consumed_per_day)))
colnames(male_livestock_Eval_consumed_day)<- colnames(male_livestock_prod_consumed_per_day)


for(i in 1:length(colnames(livestock_prod_consumed_per_day)))
{
  if(colnames(livestock_Eval_consumed_day)[i] %in% as.character(livestock_energy_table_temp[,1])==TRUE)
  {
    for (j in 1: nrow(livestock_Eval_consumed_day))
    {
      livestock_Eval_consumed_day[j,i]<- as.numeric(livestock_energy_table_temp[as.character(livestock_energy_table_temp[,1])==as.character(colnames(livestock_Eval_consumed_day)[i]),2])*livestock_prod_consumed_per_day[j,i]
      
    }
  }
}

for(i in 1:length(colnames(male_youth_livestock_Eval_consumed_day)))
{
  if(colnames(male_youth_livestock_Eval_consumed_day)[i] %in% as.character(livestock_energy_table_temp[,1])==TRUE)
  {
    for (j in 1: nrow(male_youth_livestock_Eval_consumed_day))
    {
      
      male_youth_livestock_Eval_consumed_day[j,i]<- as.numeric(livestock_energy_table_temp[as.character(livestock_energy_table_temp[,1])==as.character(colnames(male_youth_livestock_Eval_consumed_day)[i]),2])*male_youth_livestock_prod_consumed_per_day[j,i]
      female_youth_livestock_Eval_consumed_day[j,i]<- as.numeric(livestock_energy_table_temp[as.character(livestock_energy_table_temp[,1])==as.character(colnames(female_youth_livestock_Eval_consumed_day)[i]),2])*female_youth_livestock_prod_consumed_per_day[j,i]
      female_livestock_Eval_consumed_day[j,i]<- as.numeric(livestock_energy_table_temp[as.character(livestock_energy_table_temp[,1])==as.character(colnames(female_livestock_Eval_consumed_day)[i]),2])*female_livestock_prod_consumed_per_day[j,i]
      male_livestock_Eval_consumed_day[j,i]<- as.numeric(livestock_energy_table_temp[as.character(livestock_energy_table_temp[,1])==as.character(colnames(male_livestock_Eval_consumed_day)[i]),2])*male_livestock_prod_consumed_per_day[j,i]
      
    }
  }
}


crop_Eval_cons<- rowSums(crop_Eval_consumed_day,na.rm=TRUE)
female_youth_Eval_crop_controlled<- rowSums(female_youth_crop_eval_consumed_day,na.rm=TRUE)
male_youth_Eval_crop_controlled<- rowSums(male_youth_crop_eval_consumed_day,na.rm=TRUE)
female_Eval_crop_controlled<- rowSums(female_crop_eval_consumed_day)
male_Eval_crop_controlled<- rowSums(male_crop_eval_consumed_day,na.rm=TRUE)

lvst_Eval_cons<- rowSums(livestock_Eval_consumed_day,na.rm=TRUE)
female_youth_Eval_livestock_controlled<- rowSums(female_youth_livestock_Eval_consumed_day,na.rm=TRUE)
male_youth_Eval_livestock_controlled<- rowSums(male_youth_livestock_Eval_consumed_day,na.rm=TRUE)
female_Eval_livestock_controlled<- rowSums(female_livestock_Eval_consumed_day)
male_Eval_livestock_controlled<- rowSums(male_livestock_Eval_consumed_day,na.rm=TRUE)

#SEARCH_Food_Self_Sufficiency_kCal_MAE_day
foodselfsufficiency<-(crop_Eval_cons+lvst_Eval_cons)/HHsizeMAE

crop_energy_table<- crop_energy_table[!is.na(crop_energy_table$crop),]
staple_crop_price<- as.numeric(crop_price_means[, colnames(crop_price_means)==staple_crop])
staple_crop_energy_value<- crop_energy_table[firstup(crop_energy_table$crop)==firstup(staple_crop), colnames(crop_energy_table)=="energy"]


livestock_income_temp<- 
  data.frame(#cheese_income[,1]/365,
    bees_income[,1]/365,
    milk_income[,1]/365,
    # butter_income[,1]/365,
    livestock_income[,1]/365,
    eggs_income[,1]/365,
    meat_income[,1]/365)

male_youth_livestock_income_temp<- 
  data.frame(male_youth_bees_honey_income[,1]/365,
             male_youth_milk_income[,1]/365,
             male_youth_livestock_income[,1]/365,
             male_youth_eggs_income[,1]/365,
             male_youth_meat_income[,1]/365)
female_youth_livestock_income_temp<- 
  data.frame(female_youth_bees_honey_income[,1]/365,
             female_youth_milk_income[,1]/365,
             female_youth_livestock_income[,1]/365,
             female_youth_eggs_income[,1]/365,
             female_youth_meat_income[,1]/365)
male_livestock_income_temp<- 
  data.frame(male_bees_honey_income[,1]/365,
             male_milk_income[,1]/365,
             male_livestock_income[,1]/365,
             male_eggs_income[,1]/365,
             male_meat_income[,1]/365)
female_livestock_income_temp<- 
  data.frame(female_bees_honey_income[,1]/365,
             female_milk_income[,1]/365,
             female_livestock_income[,1]/365,
             female_eggs_income[,1]/365,
             female_meat_income[,1]/365)

female_youth_livestock_income_temp<- rowSums(female_youth_livestock_income_temp, na.rm = TRUE)
female_youth_crops_income_temp<- female_youth_crops_income[,1]/365

male_youth_livestock_income_temp<- rowSums(male_youth_livestock_income_temp, na.rm = TRUE)
male_youth_crops_income_temp<- male_youth_crops_income[,1]/365

female_livestock_income_temp<- rowSums(female_livestock_income_temp, na.rm = TRUE)
female_crops_income_temp<- female_crops_income[,1]/365

male_livestock_income_temp<- rowSums(male_livestock_income_temp, na.rm = TRUE)
male_crops_income_temp<- male_crops_income[,1]/365




#### Off-farm proportion calculations #####

who_control_off_farm<-dat_all[,grep('offfarm_who_control', colnames(dat_all))]


female_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
male_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
female_youth_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))
male_youth_offfarm_control<- data.frame(matrix(NA, nrow=nrow(dat_all), ncol=length(grep('offfarm_who_control', colnames(dat_all)))))

for (i in 1:length(grep('offfarm_who_control', colnames(dat_all))))
  
{
  who_control_sold<- dat_all[,colnames(dat_all)==paste0("offfarm_who_control_revenue_",i)]
  who_control_sold <-tolower(gsub("NA",NA,who_control_sold))
  
  
  for(j in 1:nrow(dat_all))
  {
    #Switching the variables with the appropriate units and proportion coefficients ###
    
    
    gender_sold_temp<-unlist(strsplit(as.character(who_control_sold[j]), " "))
    
    
    #Changing the gender labels to all be consistent
    
    
    
    
    if("female_youth" %in% gender_sold_temp==TRUE)
    {female_youth_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("female_adult" %in%gender_sold_temp==TRUE | "female_head" %in%gender_sold_temp==TRUE)
    {female_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("male_youth" %in% gender_sold_temp==TRUE)
    {male_youth_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    if("male_adult" %in%gender_sold_temp==TRUE | "male_head" %in%gender_sold_temp==TRUE)
    {male_offfarm_control[j,i]<-1/length(gender_sold_temp)}
    
  }
  
  
  
}

female_youth_offfarm_control<- rowSums(female_youth_offfarm_control, na.rm = T)
female_offfarm_control<- rowSums(female_offfarm_control, na.rm = T)
male_youth_offfarm_control<- rowSums(male_youth_offfarm_control, na.rm = T)
male_offfarm_control<- rowSums(male_offfarm_control, na.rm = T)


female_youth_offfarm_control<- female_youth_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
female_offfarm_control<- female_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
male_youth_offfarm_control<- male_youth_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)
male_offfarm_control<- male_offfarm_control/(female_youth_offfarm_control+female_offfarm_control+male_youth_offfarm_control+male_offfarm_control)


female_youth_offfarm_income<-female_youth_offfarm_control*off_farm_income[,1]/365
male_youth_offfarm_income<-male_youth_offfarm_control*off_farm_income[,1]/365
female_offfarm_income<-female_offfarm_control*off_farm_income[,1]/365
male_offfarm_income<-male_offfarm_control*off_farm_income[,1]/365

###---------------------------------------



female_youth_control_energy<- 
  data.frame(staple_crop_energy_value*female_youth_livestock_income_temp/staple_crop_price,
             staple_crop_energy_value*female_youth_crops_income_temp/staple_crop_price,
             staple_crop_energy_value*female_youth_offfarm_income/staple_crop_price,
             female_youth_Eval_livestock_controlled,
             female_youth_Eval_crop_controlled)
female_youth_control_energy<- rowSums(female_youth_control_energy, na.rm=TRUE)

male_youth_control_energy<- 
  data.frame(staple_crop_energy_value*male_youth_livestock_income_temp/staple_crop_price,
             staple_crop_energy_value*male_youth_crops_income_temp/staple_crop_price,  
             staple_crop_energy_value*male_youth_offfarm_income/staple_crop_price,  
             male_youth_Eval_livestock_controlled,
             male_youth_Eval_crop_controlled)
male_youth_control_energy<- rowSums(male_youth_control_energy, na.rm=TRUE)

female_control_energy<- 
  data.frame(staple_crop_energy_value*female_livestock_income_temp/staple_crop_price,
             staple_crop_energy_value*female_crops_income_temp/staple_crop_price,
             staple_crop_energy_value*female_offfarm_income/staple_crop_price,
             female_Eval_livestock_controlled,
             female_Eval_crop_controlled)
female_control_energy<- rowSums(female_control_energy, na.rm=TRUE)

male_control_energy<- 
  data.frame(staple_crop_energy_value*male_livestock_income_temp/staple_crop_price,
             staple_crop_energy_value*male_crops_income_temp/staple_crop_price,
             staple_crop_energy_value*male_offfarm_income/staple_crop_price,
             male_Eval_livestock_controlled,
             male_Eval_crop_controlled)
male_control_energy<- rowSums(male_control_energy, na.rm=TRUE)


Gender_Female_Youth_Control<-female_youth_control_energy/(female_youth_control_energy+male_control_energy+female_control_energy+male_youth_control_energy)
Gender_Female_control<-female_control_energy/(female_youth_control_energy+male_control_energy+female_control_energy+male_youth_control_energy)
Gender_male_youth_control<-male_youth_control_energy/(female_youth_control_energy+male_control_energy+female_control_energy+male_youth_control_energy)
Gender_male_control<-male_control_energy/(female_youth_control_energy+male_control_energy+female_control_energy+male_youth_control_energy)



livestock_income_temp<- (livestockprodsales)/365
crops_income_temp<- cropsales[,1]/365
off_farm_income_temp<- off_farm_income[,1]/365

energy_from_crops<-rowSums(crop_Eval_consumed_day,na.rm=TRUE)+(crops_income_temp*staple_crop_energy_value)/(staple_crop_price)
energy_from_lvst<-rowSums(livestock_Eval_consumed_day,na.rm=TRUE)+(livestock_income_temp*staple_crop_energy_value)/(staple_crop_price)
energy_from_off_farm<-staple_crop_energy_value*off_farm_income_temp/staple_crop_price

FAEnergyCropSales<- (crops_income_temp*staple_crop_energy_value)/(staple_crop_price)/HHsizeMAE
FAEnergyLivestockSales<- (livestock_income_temp[,1]*staple_crop_energy_value)/(staple_crop_price)/HHsizeMAE
FAEnergyCropConsumption<- rowSums(crop_Eval_consumed_day,na.rm=TRUE)/HHsizeMAE
FAEnergyLivestockConsumption<- rowSums(livestock_Eval_consumed_day,na.rm=TRUE)/HHsizeMAE
TotalEnergyAvailable<- staple_crop_energy_value*total_income[,1]/365/staple_crop_price+ rowSums(crop_Eval_consumed_day,na.rm=TRUE)+rowSums(livestock_Eval_consumed_day,na.rm=TRUE)
FAEnergyBought<- staple_crop_energy_value*total_income[,1]/365/staple_crop_price/HHsizeMAE
FAEnergyOffFarm<- energy_from_off_farm/HHsizeMAE
FAEnergyFarmBased<- (energy_from_crops+energy_from_lvst)/HHsizeMAE

#SEARCH_Food_Availability_kCal_MAE_day
FoodAvailability<- (TotalEnergyAvailable)/HHsizeMAE

#SEARCH_Livestock_Orientation
FALivestockOrientation<- (FAEnergyLivestockSales+FAEnergyLivestockConsumption)/FoodAvailability
#SEARCH_Market_Orientation
FAMarketOrientation<- (FAEnergyCropSales+FAEnergyLivestockSales)/FoodAvailability



#Energy From Wildfoods

months_collect_wildfoods<- List_to_True_False(as.character(dat_all$wildfood_collect_when))
months_collect_wildfoods$`NA`<-NULL

nr_ofmonths_wildfoods<- rowSums(months_collect_wildfoods)
wild_food_prop<- data.frame(unlist(sapply(as.character(dat_all$wildfood_amount),
                                          switch,
                                          "all"=coeff_All,
                                          "most"=coeff_most,
                                          "half"=coeff_Half,
                                          "underhalf"=coeff_Underhalf,
                                          "little"=coeff_Little,
                                          "none"=coeff_None,
                                          "NA"=NA)))


wild_food_prop_text<- as.character(dat_all$wildfood_amount)
wild_food_energy<- c()
for (i in 1:nrow(dat_all))
{
  wild_food_energy[i]<- (wild_food_prop[i,1]*(FAEnergyBought[i]+FAEnergyCropConsumption[i]+FAEnergyLivestockConsumption[i]))/(1-wild_food_prop[i,1])
}  
# Wildfood<- prop(wildfood)(food consumed + food bought)/ (1-prop wildfood)
#SEARCH_NrofMonthsWildFoodCons
NrofMonthsWildFoodCons<- nr_ofmonths_wildfoods

