# Fertiliser and Emissions

# A script containing Calculations relating to fertiliser and GHG emissions


#### Fertiliser Input #### 
#SEARCH_NFertInput
fertiliser_input<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
colnames(fertiliser_input)<- c("NFertInput")

units_column<- dat_all$fertiliser_units
units_other_column<- dat_all$fertiliser_units_other

for (j in 1:length(units_column))
{
  if(!is.na(units_column[j])==TRUE)
  {
    if(units_column[j] %in% colnames(fertiliser_units_coefficients)==TRUE && units_column[j]!="other"&& units_column[j]!="NA")
    {units_column[j]<- fertiliser_units_coefficients[1,colnames(fertiliser_units_coefficients)==units_column[j]]}
  }
  if(!is.na(units_column[j])==TRUE)
  {
    if(units_column[j]=="other")
    {units_column[j]<-NA}
  }
  
  if(units_other_column[j] %in% colnames(fertiliser_units_coefficients)==TRUE&& units_other_column[j]!="NA")
  {units_other_column[j]<- fertiliser_units_coefficients[1,colnames(fertiliser_units_coefficients)==units_other_column[j]]}
}

for(i in 1: nrow(fertiliser_input))
{
  fertiliser_amount_temp<- as.numeric(as.character(dat_all[i,colnames(dat_all)=="fertiliser_amount"]))
  
  if (!is.na(units_column[i]))
  {
  if ( units_column[i]!="other" && units_column[i]!="NA")
  {
    fertiliser_input[i,1]<- as.numeric(as.character(units_column[i]))*as.numeric(fertiliser_amount_temp)
  }
  if (units_column[i]=="other" && !is.na(units_other_column[i])&& units_other_column[i]!="NA" )
  {
    fertiliser_input[i,1]<- as.numeric(as.character(units_other_column[i]))*as.numeric(fertiliser_amount_temp)
  }
  }
}
#-------------------------------------------------------------------------------------------------
#### Emissions ####
#Livestock

manure_info<-List_to_True_False(as.character(dat_all$livestock_manure))
setje<-data.frame("chicken"=livestock_heads$chicken,"cattle"=livestock_heads$cattle,"ducks"=0,"goats"=(as.numeric(livestock_heads$goats)+as.numeric(livestock_heads$sheep)),"horses"=livestock_heads$donkeys,"turkey"=livestock_heads$otherpoultry,"pigs"=livestock_heads$pigs)
setje <- data.frame(lapply(setje, as.character), stringsAsFactors=FALSE)
setje <- data.frame(lapply(setje, as.numeric), stringsAsFactors=FALSE)


manure_info<- List_to_True_False(as.character(dat_all$livestock_manure))


manure_dispose<- manure_info$dispose
manure_store_pile<- manure_info$dispose
manure_enclosed<-manure_info$store_enclosed
manure_fuel<-manure_info$fuel
manure_sell<-manure_info$sell
manure_biodigester<- manure_info$biodigester

#Livestock Emissions
#-----------
length_temp<- length(grep("livestock_name_", colnames(dat_all)))
livestock_housing<- data.frame(matrix(NA, ncol=length(unique_livestock), nrow=nrow(dat_all)))
colnames(livestock_housing)<- unique_livestock
for (i in 1:length_temp)
{
  for(j in 1:nrow(dat_all))
  {
    livestock_housing_temp<- as.character(dat_all[j,colnames(dat_all)==paste0("livestock_housing_", i)])
    if (livestock_housing_temp!='na')
    {
    livestock_housing_temp<-as.numeric(housing_factor[housing_factor[,1]%in%livestock_housing_temp,2])
    if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)] %in% unique_livestock==TRUE)
    {
      if (length(livestock_housing_temp)>0)
      {
      livestock_housing[j,colnames(livestock_housing)==dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]]<- livestock_housing_temp
    
      }
    }
    }
  }
}
livestock_housing_final<- data.frame(matrix(NA, ncol=0, nrow=nrow(dat_all)))
livestock_housing_final$chicken<- livestock_housing[,grep("chicken",colnames(livestock_housing))]
livestock_housing_final$cattle<-livestock_housing[,grep("cattle",colnames(livestock_housing))]
livestock_housing_final$ducks<-livestock_housing[,grep("ducks",colnames(livestock_housing))]
livestock_housing_final$goats<-livestock_housing[,grep("goats",colnames(livestock_housing))]
if(length(livestock_housing[,grep("donkeys",colnames(livestock_housing))])>0){livestock_housing_final$horses<-livestock_housing[,grep("donkeys",colnames(livestock_housing))]}
if(length(livestock_housing[,grep("horses",colnames(livestock_housing))])>0){livestock_housing_final$horses<-livestock_housing[,grep("horses",colnames(livestock_housing))]}
if(length(livestock_housing[,grep("donkeys",colnames(livestock_housing))])>0 && length(livestock_housing[,grep("horses",colnames(livestock_housing))])>0){livestock_housing_final$horses<-livestock_housing[,grep("donkeys",colnames(livestock_housing))]+livestock_housing[,grep("horses",colnames(livestock_housing))]}
livestock_housing_final$turkey<-livestock_housing[,grep("turkey",colnames(livestock_housing))]
livestock_housing_final$pigs<-livestock_housing[,grep("pigs",colnames(livestock_housing))]


#-----------
Enteric_ferm_CH4<- data.frame(matrix(NA, nrow= nrow(setje), ncol=ncol(setje)))
for (i in 1:ncol(Enteric_ferm_CH4))
{
  for (j in 1:nrow(Enteric_ferm_CH4))
    Enteric_ferm_CH4[j,i]<- setje[j,i]*Entferm[i]
}
Enteric_ferm_CH4_total<- rowSums(Enteric_ferm_CH4, na.rm=TRUE)
#-----------
Annual_Nman_prod<- data.frame(matrix(NA, nrow= nrow(setje), ncol=ncol(setje)))
for (i in 1:ncol(Annual_Nman_prod))
{
  for (j in 1:nrow(Annual_Nman_prod))
    Annual_Nman_prod[j,i]<- setje[j,i]*Nex_values[i]*TAM_values[i]
}
Annual_Nman_prod_total<- rowSums(Annual_Nman_prod, na.rm=TRUE)
#-----------
Annual_CH4man_prod<- data.frame(matrix(NA, nrow= nrow(setje), ncol=ncol(setje)))
for (i in 1:ncol(Annual_CH4man_prod))
{
  for (j in 1:nrow(Annual_CH4man_prod))
    Annual_CH4man_prod[j,i]<- setje[j,i]*CH4manure_factors[i]
}
Annual_CH4man_prod_total<- rowSums(Annual_CH4man_prod, na.rm=TRUE)
#-----------
Nmaninstable<- data.frame(matrix(NA, nrow= nrow(setje), ncol=ncol(setje)))
counter<-0
for (i in colnames(setje))
{
  counter<-counter+1
  for(j in 1:nrow(setje))
  {
    if (i %in% colnames(livestock_housing_final)==TRUE && length(livestock_housing_final[,colnames(livestock_housing_final)==i])!=0)
    {
      Nmaninstable[j,counter]<- livestock_housing_final[j, colnames(livestock_housing_final)==i]*Nex_values[counter]*TAM_values[counter]*setje[j,counter]
    }
  }
}
Nmaninstable_total<- rowSums(Nmaninstable,na.rm=TRUE)
Nmaninfield<-Annual_Nman_prod_total-Nmaninstable_total
#-----------
Nlossesinstorage<-Nmaninstable_total*cattle_pile_NMM

N2O_manstorage<-Nlossesinstorage
N_inmanure<- Nmaninstable_total-Nlossesinstorage

Total_Nfaeces_directinfield<- Nmaninfield

#-----------
#-----------
#Crop Emissions calculations
#-----------
crop_residue_use<- data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(crop_residue_use)<- colnames(crop_yield_kg)
crop_residue_use_numbers<- data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(crop_residue_use_numbers)<- colnames(crop_yield_kg)
crop_residue_into_soil<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(crop_residue_into_soil)<- colnames(crop_yield_kg)
crop_residue_burnt<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(crop_residue_burnt)<- colnames(crop_yield_kg)


for(i in 1:length(grep("crop_sold_income_", colnames(dat_all))))
{
  for(j in 1:nrow(crop_residue_use))
  {
    crop_residue_temp<- as.character(dat_all[j,colnames(dat_all)==paste0("crop_residue_use_",i)])
    if (as.character(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]) %in% unique_crops==TRUE)
    {
      crop_residue_use[j,colnames(crop_residue_use)==as.character(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)])]<- crop_residue_temp
      crop_residue_use_numbers[j,colnames(crop_residue_use_numbers)==as.character(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)])]<- length(unlist(strsplit(crop_residue_temp, " ")))
      
      if("soil" %in% crop_residue_temp==TRUE)
      {
        crop_residue_into_soil[j,colnames(crop_residue_into_soil)==as.character(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)])]<-1/length(unlist(strsplit(crop_residue_temp, " ")))
      }
      if("burn" %in% crop_residue_temp==TRUE)
      {
        crop_residue_burnt[j,colnames(crop_residue_burnt)==as.character(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)])]<-1/length(unlist(strsplit(crop_residue_temp, " ")))
      }
    }
  }
}



HI_df<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(HI_df)<- colnames(crop_yield_kg)
ShootRoot_df<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(ShootRoot_df)<- colnames(crop_yield_kg)
Nabove_df<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(Nabove_df)<- colnames(crop_yield_kg)
Nbelow_df<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(Nbelow_df)<- colnames(crop_yield_kg)
counter<-0
for(i in colnames(crop_yield_kg))
{
  counter<- counter+1
  HI_df[,counter]<-0.5
  ShootRoot_df[,counter]<-0.5
  Nabove_df[,counter]<-0.02
  Nbelow_df[,counter]<-0.02
  if(length(Grains_crops[Grains_crops%in%i])>0)
  {HI_df[,counter]<-Grains[1];ShootRoot_df[,counter]<-Grains[2];Nabove_df[,counter]<-Grains[3];Nbelow_df[,counter]<-Grains[4]}
  if (length(Beanspulses_crops[Beanspulses_crops%in%i])>0) 
  {HI_df[,counter]<-Beanspulses[1];ShootRoot_df[,counter]<-Beanspulses[2];Nabove_df[,counter]<-Beanspulses[3];Nbelow_df[,counter]<-Beanspulses[4]}
  if (length(Tubers_crops[Tubers_crops%in%i])>0) 
  {HI_df[,counter]<-Tubers[1];ShootRoot_df[,counter]<-Tubers[2];Nabove_df[,counter]<-Tubers[3];Nbelow_df[,counter]<-Tubers[4]}
  if (length(Rootcropsother_crops[Rootcropsother_crops%in%i])>0)
  {HI_df[,counter]<-Rootcropsother[1];ShootRoot_df[,counter]<-Rootcropsother[2];Nabove_df[,counter]<-Rootcropsother[3];Nbelow_df[,counter]<-Rootcropsother[4]}
}

CR_total<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(CR_total)<- colnames(crop_yield_kg)
N_incropresidues<-data.frame((matrix(NA, ncol= ncol(crop_yield_kg), nrow=nrow(crop_yield_kg))))
colnames(N_incropresidues)<- colnames(crop_yield_kg)


for (i in 1:ncol(crop_yield_kg))
{
  for (j in 1:nrow(crop_yield_kg))
  {
    if(!is.na(HI_df[j,i]==TRUE))
    {
      CR_total[j,i]<- (crop_yield_kg[j,i]/HI_df[j,i])*(1-HI_df[j,i])*2
      Nabove_df[j,i]<-CR_total[j,i]*Nabove_df[j,i]*crop_residue_into_soil[j,i]
      Nbelow_df[j,i]<-CR_total[j,i]*(1-ShootRoot_df[j,i])/0.5*Nbelow_df[j,i]
    }
    N_incropresidues[j,i]<-Nabove_df[j,i]+Nbelow_df[j,i]
  }
}


Ncorrfact<-data.frame(matrix(0.2, ncol=1, nrow=nrow(fertiliser_input)))

for (i in 1:nrow(Ncorrfact)) 
{
  if (!is.na(as.character(dat_all$fertiliser_type[i])) && 'urea' %in% tolower(unlist(strsplit(as.character(dat_all$fertiliser_type[i]), " "))))
  {
    Ncorrfact[i,1]<-0.46
  }
}

N_inminfert<- c()
N_inminfert<-fertiliser_input*Ncorrfact

N2O_amendments_direct<-Soils_EF1*(N_inminfert[,1]+N_inmanure+rowSums(N_incropresidues, na.rm=TRUE))+Urine_EF_PRP*Total_Nfaeces_directinfield

#N2O emissions, indirect, deposition
N2O_amend_indirectDepos<-EF4*(N_inminfert[,1]*VolminfertN+(N_inmanure+Total_Nfaeces_directinfield)*VolorgNinput)

#CO2 emissions from urea application
#mineral fert used
urea_fert_kg<-0
CO2_emissions_urea<-CO2_EM_urea*urea_fert_kg


#total emissions
Emissions_livestock<-25*Enteric_ferm_CH4_total+25*Annual_CH4man_prod_total+298*0.02*(44/28)*(Total_Nfaeces_directinfield+N2O_manstorage)
Emissions_crops<-298*(N2O_amendments_direct+N2O_amend_indirectDepos)+CO2_emissions_urea

Emissions_total<-Emissions_livestock+Emissions_crops

