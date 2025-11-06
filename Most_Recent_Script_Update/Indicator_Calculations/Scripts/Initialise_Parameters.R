#' A very long script designed to remove any irregularities in the data set. It is also designed to ensure that consistent terminology is used throughout all versions of the RHoMIS survey

# All of the associated units sheet can be found in the 'Units' Folder

# More Cleaning of Country names
dat_all$country<- gsub("Burkina Faso","Burkina_Faso",dat_all$country)
dat_all$country<- gsub("Costa Rica","Costa_Rica",dat_all$country)

#SEARCH_Harvested_crop
crop_yield_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/crop_yield_units.csv",na=c('n/a','<NA>')))
crop_yield_units_original<-crop_yield_units_original[grep(unique(dat_all$country), crop_yield_units_original$crop_yield_units.Country, ignore.case = T),]
crop_yield_units_original$crop_yield_units.Country<- NULL
if (nrow(crop_yield_units_original)==0)
{
  crop_yield_units_original[1,]<-c(NA,NA)
}
crop_coefficients<- data.frame(lapply(crop_yield_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
crop_coefficients[1,grep("\\?",as.character(unlist(crop_coefficients[1,], use.names = F)))]<-NA
crop_coefficients<- data.frame(lapply(crop_coefficients, as.character), stringsAsFactors=FALSE)
colnames(crop_coefficients)<-c(crop_yield_units_original[,1])

#SEARCH_Income_Crop
crop_price_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/crop_price_units.csv",na=c('n/a','<NA>')))
crop_price_units_original<-crop_price_units_original[grep(unique(dat_all$country),  crop_price_units_original$crop_price_units.Country, ignore.case = T),]
crop_price_units_original$crop_price_units.Country<-NULL
if (nrow(crop_price_units_original)==0)
{
  crop_price_units_original[1,]<-c(NA,NA)
}
crop_sold_units_coefficients<- data.frame(lapply(crop_price_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
crop_sold_units_coefficients[1,grep("\\?",as.character(unlist(crop_sold_units_coefficients[1,], use.names = F)))]<-NA
crop_sold_units_coefficients<- data.frame(lapply(crop_sold_units_coefficients, as.character), stringsAsFactors=FALSE)
colnames(crop_sold_units_coefficients)<-c(crop_price_units_original[,1])

#SEARCH_NFertInput
fertiliser_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/fertiliser_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
fertiliser_units_original<-fertiliser_units_original[grep(unique(dat_all$country),  fertiliser_units_original$Country, ignore.case = T),]
fertiliser_units_original$Country<-NULL
if (nrow(fertiliser_units_original)==0)
{
  fertiliser_units_original[1,]<-c(NA,NA)
}
fertiliser_units_coefficients<- data.frame(lapply(fertiliser_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
fertiliser_units_coefficients[1,grep("\\?",as.character(unlist(fertiliser_units_coefficients[1,], use.names = F)))]<-NA
fertiliser_units_coefficients<- data.frame(lapply(fertiliser_units_coefficients, as.character), stringsAsFactors=FALSE)
colnames(fertiliser_units_coefficients)<-c(fertiliser_units_original[,1])


land_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/land_area_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
land_units_original<-land_units_original[grep(unique(dat_all$country),  land_units_original$land_area_units.Country, ignore.case = T),]
land_units_original$land_area_units.Country<-NULL
if (nrow(land_units_original)==0)
{
  land_units_original[1,]<-c(NA,NA)
}
unitland<- data.frame(lapply(land_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
unitland[1,grep("\\?",as.character(unlist(unitland[1,], use.names = F)))]<-NA
unitland<- data.frame(lapply(unitland, as.character), stringsAsFactors=FALSE)
colnames(unitland)<-c(land_units_original[,1])
land_units<-unitland

#SEARCH_Milk_Amount 
milk_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/milk_amount_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
milk_units_original<-milk_units_original[grep(unique(dat_all$country),  milk_units_original$milk_amount_units.Country, ignore.case = T),]
milk_units_original$milk_amount_units.Country<-NULL
if (nrow(milk_units_original)==0)
{
  milk_units_original[1,]<-c(NA,NA)
}
milk_units_column<- data.frame(lapply(milk_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
milk_units_column[1,grep("\\?",as.character(unlist(milk_units_column[1,], use.names = F)))]<-NA
milk_units_column<- data.frame(lapply(milk_units_column, as.character), stringsAsFactors=FALSE)
colnames(milk_units_column)<-c(milk_units_original[,1])

#SEARCH_Milk_Income
milk_sold_price_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/milk_price_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
milk_sold_price_units_original<-milk_sold_price_units_original[grep(unique(dat_all$country),  milk_sold_price_units_original$milk_price_units.Country, ignore.case = T),]
milk_sold_price_units_original$milk_price_units.Country<-NULL
if (nrow(milk_sold_price_units_original)==0)
{
  milk_sold_price_units_original[1,]<-c(NA,NA)
}
milk_sold_price_units<- data.frame(lapply(milk_sold_price_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
milk_sold_price_units[1,grep("\\?",as.character(unlist(milk_sold_price_units[1,], use.names = F)))]<-NA
milk_sold_price_units<- data.frame(lapply(milk_sold_price_units, as.character), stringsAsFactors=FALSE)
colnames(milk_sold_price_units)<-c(milk_sold_price_units_original[,1])


butter_price_time_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/butter_price_time_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
butter_price_time_units_original<-butter_price_time_units_original[grep(unique(dat_all$country),  butter_price_time_units_original$butter_price_time_units.Country, ignore.case = T),]
butter_price_time_units_original$butter_price_time_units.Country<-NULL
butter_price_time_units_original$butter_price_time_units.Country<-NULL
if (nrow(butter_price_time_units_original)==0)
{
  butter_price_time_units_original[1,]<-c(NA,NA)
}
butter_price_time_units<- data.frame(lapply(butter_price_time_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
butter_price_time_units[1,grep("\\?",as.character(unlist(butter_price_time_units[1,], use.names = F)))]<-NA
butter_price_time_units<- data.frame(lapply(butter_price_time_units, as.character), stringsAsFactors=FALSE)
colnames(butter_price_time_units)<-c(butter_price_time_units_original[,1])


butter_time_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/butter_time_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
butter_time_units_original<-butter_time_units_original[grep(unique(dat_all$country),  butter_time_units_original$butter_time_units.Country, ignore.case = T),]
butter_time_units_original$butter_time_units.Country<-NULL
if (nrow(butter_time_units_original)==0)
{
  butter_time_units_original[1,]<-c(NA,NA)
}
butter_time_units<- data.frame(lapply(butter_time_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
butter_time_units[1,grep("\\?",as.character(unlist(butter_time_units[1,], use.names = F)))]<-NA
butter_time_units<- data.frame(lapply(butter_time_units, as.character), stringsAsFactors=FALSE)
colnames(butter_time_units)<-c(butter_time_units_original[,1])


butter_units_amount_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/butter_amount_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
butter_units_amount_original<-butter_units_amount_original[grep(unique(dat_all$country),  butter_units_amount_original$butter_amount_units.Country, ignore.case = T),]
butter_units_amount_original$butter_amount_units.Country<-NULL
if (nrow(butter_units_amount_original)==0)
{
  butter_units_amount_original[1,]<-c(NA,NA)
}
butter_units_column<- data.frame(lapply(butter_units_amount_original[,2], function(x) t(data.frame(x))), row.names = c(1))
butter_units_column[1,grep("\\?",as.character(unlist(butter_units_column[1,], use.names = F)))]<-NA
butter_units_column<- data.frame(lapply(butter_units_column, as.character), stringsAsFactors=FALSE)
colnames(butter_units_column)<-c(butter_units_amount_original[,1])


cheese_price_time_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/cheese_price_time_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
cheese_price_time_units_original<-cheese_price_time_units_original[grep(unique(dat_all$country),  cheese_price_time_units_original$cheese_price_time_units.Country, ignore.case = T),]
cheese_price_time_units_original$cheese_price_time_units.Country<-NULL
if (nrow(cheese_price_time_units_original)==0)
{
  cheese_price_time_units_original[1,]<-c(NA,NA)
}
cheese_price_time_units<- data.frame(lapply(cheese_price_time_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
cheese_price_time_units[1,grep("\\?",as.character(unlist(cheese_price_time_units[1,], use.names = F)))]<-NA
cheese_price_time_units<- data.frame(lapply(cheese_price_time_units, as.character), stringsAsFactors=FALSE)
colnames(cheese_price_time_units)<-c(cheese_price_time_units_original[,1])


cheese_time_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/cheese_time_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
cheese_time_units_original<-cheese_time_units_original[grep(unique(dat_all$country),  cheese_time_units_original$cheese_time_units.Country, ignore.case = T),]
cheese_time_units_original$cheese_time_units.Country<-NULL
if (nrow(cheese_time_units_original)==0)
{
  cheese_time_units_original[1,]<-c(NA,NA)
}
cheese_time_units<- data.frame(lapply(cheese_time_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
cheese_time_units[1,grep("\\?",as.character(unlist(cheese_time_units[1,], use.names = F)))]<-NA
cheese_time_units<- data.frame(lapply(cheese_time_units, as.character), stringsAsFactors=FALSE)
colnames(cheese_time_units)<-c(cheese_time_units_original[,1])


cheese_units_amount_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/cheese_amount_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
cheese_units_amount_original<-cheese_units_amount_original[grep(unique(dat_all$country),  cheese_time_units_original$cheese_time_units.Country, ignore.case = T),]
cheese_units_amount_original$cheese_amount_units.Country<-NULL
if (nrow(cheese_units_amount_original)==0)
{
  cheese_units_amount_original[1,]<-c(NA,NA)
}
cheese_units<- data.frame(lapply(cheese_units_amount_original[,2], function(x) t(data.frame(x))), row.names = c(1))
cheese_units[1,grep("\\?",as.character(unlist(cheese_units[1,], use.names = F)))]<-NA
cheese_units<- data.frame(lapply(cheese_units, as.character), stringsAsFactors=FALSE)
colnames(cheese_units)<-c(cheese_units_amount_original[,1])


bees_honey_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/honey_amount_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
bees_honey_units_original<-bees_honey_units_original[grep(unique(dat_all$country),  bees_honey_units_original$honey_amount_units.Country, ignore.case = T),]
bees_honey_units_original$honey_amount_units.Country<-NULL
if (nrow(bees_honey_units_original)==0)
{
  bees_honey_units_original[1,]<-c(NA,NA)
}
bees_honey_units_column<- data.frame(lapply(bees_honey_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
bees_honey_units_column[1,grep("\\?",as.character(unlist(bees_honey_units_column[1,], use.names = F)))]<-NA
bees_honey_units_column<- data.frame(lapply(bees_honey_units_column, as.character), stringsAsFactors=FALSE)
colnames(bees_honey_units_column)<-c(bees_honey_units_original[,1])

#SEARCH_Eggs_Collected
eggs_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/eggs_amount_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
eggs_units_original<-eggs_units_original[grep(unique(dat_all$country),  eggs_units_original$eggs_amount_units.Country, ignore.case = T),]
eggs_units_original$eggs_amount_units.Country<-NULL
if (nrow(eggs_units_original)==0)
{
  eggs_units_original[1,]<-c(NA,NA)
}
eggs_units_column<- data.frame(lapply(eggs_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
eggs_units_column[1,grep("\\?",as.character(unlist(eggs_units_column[1,], use.names = F)))]<-NA
eggs_units_column<- data.frame(lapply(eggs_units_column, as.character), stringsAsFactors=FALSE)
colnames(eggs_units_column)<-c(eggs_units_original[,1])

#SEARCH_Eggs_Income
eggs_price_units_original<-data.frame(read_csv("../../Units_and_Cleaning_Data/Units/eggs_price_units.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
eggs_price_units_original<-eggs_price_units_original[grep(unique(dat_all$country),  eggs_price_units_original$eggs_price_units.Country, ignore.case = T),]
eggs_price_units_original$eggs_price_units.Country<-NULL
if (nrow(eggs_price_units_original)==0)
{
  eggs_price_units_original[1,]<-c(NA,NA)
}
eggs_sold_price_unit<- data.frame(lapply(eggs_price_units_original[,2], function(x) t(data.frame(x))), row.names = c(1))
eggs_sold_price_unit[1,grep("\\?",as.character(unlist(eggs_sold_price_unit[1,], use.names = F)))]<-NA
eggs_sold_price_unit<- data.frame(lapply(eggs_sold_price_unit, as.character), stringsAsFactors=FALSE)
colnames(eggs_sold_price_unit)<-c(eggs_price_units_original[,1])





#-------------------------------------------------------------------------------------------------
#### Male Adult Equivalent (MAE) Coefficients ####
#SEARCH_HHsizeMAE
coeff_children_0_to_4<-0.5
coeff_children_4_to_10<-0.75
coeff_male_11_to_24<-0.85
coeff_female_11_to_24<-0.75
coeff_male_25_to_50<-1
coeff_female_25_to_50<-0.86
coeff_male_50_plus<-0.73
coeff_female_50_plus<-0.6

MAE_coeff<- cbind(c("children_0_to_4", "children_4_to_10", "male_11_to_24", "female_11_to_24", "male_25_to_50", "female_25_to_50", "male_50_plus", "female_50_plus"),
                  c(      0.5,               0.75,                0.85,              0.75,             1,              0.86,              0.73,              0.6))
#-------------------------------------------------------------------------------------------------
#### Coefficient to convert livestock into tropical livestock units ####
# SEARCH_LivestockHoldings

TLU_Coeff_df<-data.frame(read_csv('../../Indicator_Calculations/Literature_Values_and_Resources/TLU_Conversion.csv'),check.names = F)

coeff_donkeys_horses<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='donkeys horses']
coeff_other_poultry<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='otherpoultry']
coeff_chicken_poultry<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='chicken']
coeff_cattle<-  TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='cattle']
coeff_ducks<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='ducks']
coeff_goats<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='goats']
coeff_turkey<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='turkey']
coeff_pigs<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='pigs']
coeff_rabbits<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='goats']
coeff_sheep<- TLU_Coeff_df$TLU_Conversion[TLU_Coeff_df$Animal=='sheep']
coeff_other1<- 0
coeff_other2<- 0
coeff_other3<- 0
coeff_bees<- 0
coeff_fish<- 0

#-------------------------------------------------------------------------------------------------
#### offfarm income proportions ####


prop_switch<- data.frame("all"=0.9,
                         "most"=0.7,
                         "half"=0.5,
                         "underhalf"=0.2,
                         "little"=0.1,
                         "none"=0,
                         "NA"=NA)
colnames(prop_switch)<- gsub("NA.", "NA", colnames(prop_switch), fixed=TRUE)
coeff_All<- 0.9
coeff_most<- 0.7
coeff_Half<- 0.5
coeff_Underhalf<-0.2
coeff_Little<-0.1
coeff_None<-0

#-------------------------------------------------------------------------------------------------
#### Energy coefficients ####
#SEARCH_Food_Availability_kCal_MAE_day
#SEARCH_Food_Self_Sufficiency_kCal_MAE_day

Livestock_prod_energies<-data.frame(read_csv('../../Indicator_Calculations/Literature_Values_and_Resources/Livestock_Product_Energy.csv'),check.names = F)



coeff_energy_eggs<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='eggs']
coeff_energy_milk<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='milk']
coeff_energy_cheese<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='cheese']
coeff_energy_cream<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='cream']
coeff_energy_butter<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='butter']
coeff_energy_bees_honey<-Livestock_prod_energies$Energy_Coefficient[Livestock_prod_energies$Livestock_Product=='honey']
#-------------------------------------------------------------------------------------------------
#### Weight per animal coefficients ####
#SEARCH_Meat_Amount 
# weight_killed_animals_set<-cbind(c('fish','pigs','chicken','cattle','ducks','goats','horses','horse','turkey','rabbits', 'crocodile', 'otherpoultry', 'geese', 'buffalo', 'guinea pigs', 'doves', 'donkeys_horses', 'camel', 'llama', 'alpaca'),
#                                  c( 1,     150,      1,       250,     1,      25,     250,    250,     2,        1,         55,           1,           2,        250,          1,          1,           250,         250,      250,      250),
#                                  c(1290,  2197,    1290,      2197,   2197,   1075,    2197,   2197,   2197,     1290,      2197,        1290,        1290,       2197,       1290,        1290,         2197,        2197,     2197,    2197))

weight_killed_animals_set <-data.frame(read_csv('../../Indicator_Calculations/Literature_Values_and_Resources/MeatEnergy.csv'),check.names = F)
colnames(weight_killed_animals_set)<-NULL

weight_killed_animals_set<- as.matrix(weight_killed_animals_set)
#-------------------------------------------------------------------------------------------------
#### Emmissions Coefficients ####
#manure management CH4
#three T classes, cool, temperate, warm
#cattle, Table 10.14, uncertainty +/- 30% 
#SEARCH_GHGEmissions

dairy_cattle_CH4MM<-c(1,1,2)
other_cattle_CH4MM<-c(1,1,1)
Buffalo_CH4MM<-c(1,1,1)
Pigs_CH4MM<-c(1,1,1)
#other animals, Table 10.15, uncertainty +/- 30% 
sheep_CH4MM<-c(0.10,0.15,0.20)
goats_CH4MM<-c(0.11,0.17,0.22)
camel_CH4MM<-c(1.28,1.92,2.56)
horses_CH4MM<-c(1.09,1.64,2.19)
donkeys_CH4MM<-c(0.60,0.90,1.20)
poultry_CH4MM<-c(0.01,0.02,0.02)

#N2O emissions manure
#E<-A*EF
#A is toal manure treated in certain option
#is manure produced * allocation
#allocation comes from survey
#EF values:
cattle_bedding_mixing_NMM<-0.07
cattle_bedding_nomixing_NMM<-0.01
pigs_bedding_mixing_NMM<-0.07
pigs_bedding_nomixing_NMM<-0.01
cattle_pile_NMM<-0.006
poultry_NMM<-0.001

#manure production
#total amount of manure is number of heads times TAM (typical animal mass) times  N excretion coefficient (Nex)
#DairyCattle_Nex<-0.48;#OtherCattle_Nex<-0.36;#Pigs_Nex<-1.64;#Poultry_Nex<-0.82;#Turkeys_Nex<-0.74;#Ducks_Nex<-0.83;#Sheep_Nex<-1.17;#Goats_Nex<-1.37;#Horses_Nex<-0.46;#Donkeys_Nex<-0.46;#Camels_Nex<-0.46;#Buffalo_Nex<-0.32
#DairyCattle_TAM<-400;#OtherCattle_TAM<-305;#Pigs_TAM<-28;#Poultry_TAM<-1.8;#Turkeys_TAM<-6.8;#Ducks_TAM<-2.7;#Sheep_TAM<-28;#Goats_TAM<-30;#Horses_TAM<-238;#Donkeys_TAM<-130;#Camels_TAM<-217;#Buffalo_TAM<-380

#N2O from managed soils
#is input mineral fert N + manure N + crop residue N times soils EF1 factor
Soils_EF1<-0.01 #for all mineral N and organic N inputs!
# plus deposition by animals 
Urine_EF_PRP<-0.02 #emissions from urine and faeces in field
#the latter multiplied by number of heads times Nex times fraction of time in the field

#residue calculation: fraction that stays in field plus produced residues: uses harvest index, shoot/root ratio, aboveground N value, and belowground N value 
Grains<-c(0.4,0.22,0.006,0.009)
Grains_crops<-c('maize','millet','sorghum')
Beanspulses<-c(0.4,0.19,0.008,0.008)
Beanspulses_crops<-c('beans','soyabean','groundnut')
Tubers<-c(0.6,0.2,0.019,0.014)
Tubers_crops<-c('potatoes','sweet potato')
Rootcropsother<-c(0.6,0.2,0.016,0.014)
Rootcropsother_crops<-c('yucacasava','cassava')
#Nfixingforages<-c(0.5,0.4,0.027,0.022);#NonNfixingforages<-c(0.5,0.54,0.015,0.012);#Perennialgrasses<-c(0.5,0.8,0.015,0.012);#Grassclovermixtures<-c(0.5,0.8,0.025,0.016)

#N indirect: atm. deposition
#is min fert N * volatilization min fert plus manure N plus urine dep N times organic vol., all multiplied by EF4
#SEARCH_GHGEmissions
EF4<-0.04
EF5<-0.0075
VolminfertN<-0.1
VolorgNinput<-0.2

#N indirect 2: leaching
#is all N inputs (N min fert, manure, crop residues, urine dep) times EF5 times leaching fraction
LeachingFrac<-0

#CO2 from urea:
CO2_EM_urea<-0.2

#trees & crops
Tree_growth_rate<-9 #tons of C per ha per year
#and this minus harvested/cleared
#relative Stock changes in C over 20 years; correct to years and multiply rates
Longtermcultivated_moistwet<-0.48
Longtermcultivated_dry<-0.58
Longtermperennial<-1.00
Reducedtillage_dry<-1.09
Reducedtillage_wet<-1.15
Organicinputlow_dry<-0.95
Organicinputlow_wet<-0.92
Organicinputmedium_dry<-1
Organicinputmedium_wet<-1
Organicinputhighwithoutmanure_dry<-1.04
Organicinputhighwithoutmanure_wet<-1.11
Organicinputhighwithmanure_dry<-1.37
Organicinputhighwithmanure_wet<-1.44

#burnt residues
#is amount times combustion factor times emission factor for each gass;
#Emission factors for agricultural residues for CO2, CO,CH4, N2) and NOx 
#Agricultural residues<-c(1515,92,2.7,0.07,2.5)
#Cf<-0.8

lvst_categories<-c('chicken','cows','ducks','goats','horses','turkey','pigs')

TAM_values<-c(1.8,400,2.7,30,238,6.8,28)
Nex_values<-c(0.82,0.48,0.83,1.37,0.46,0.74,1.46)
ind_CH4man<-3 #warm climate values!!
CH4manure_factors<-c(poultry_CH4MM[ind_CH4man],dairy_cattle_CH4MM[ind_CH4man],poultry_CH4MM[ind_CH4man],goats_CH4MM[ind_CH4man],horses_CH4MM[ind_CH4man],poultry_CH4MM[ind_CH4man],Pigs_CH4MM[ind_CH4man])

Entferm<-c(0,72,0,5,18,0,0)

housing_factor<-cbind(c('in_always','in_night','in_night_and_half_day','n/a','out_all_time','empty', 'most', 'half', 'NA', 'none', 'underhalf', 'all', 'little'),c(1,0.6,0.75,0.5,0,0.5, 0.7, 0.5, NA, 0, 0.2, 0.9, 0.1))

crop_residue_factors<-c('soil', 'soil feed', 'feed','soil burn','burn','soil fuel','sell','fuel')
CR_soil<-c(1,0.5,0,0.5,0,0.5,0,0)
CR_feed<-c(0,0.5,1,0,0,0,0,0)
CR_burn<-c(0,0,0,0.5,1,0.5,0,1)

#Male_Female_control_cleaning

#SEARCH_Gender_MaleControl
#SEARCH_Gender_FemaleControl
Male_female_control_switch<- data.frame("NA"=NA,
                                        "male_head"="male_adult",
                                        "female_head"="female_adult",
                                        "male_youth_or_child"="male_youth",
                                        "female_youth_or_child"="female_youth",
                                        "null"=NA,
                                        "c(\"male\","="male_adult",
                                        "\"child\")"=NA,
                                        "male_adult"="male_adult",
                                        "female_adult"="female_adult",
                                        "c(\"female\","="female_adult",
                                        "\"youth\","=NA,
                                        "0"=NA)
colnames(Male_female_control_switch)<- c("NA",
                                         "male_head",
                                         "female_head",
                                         "male_youth_or_child",
                                         "female_youth_or_child",
                                         "null",
                                         "c(\"male\","="male_adult",
                                         "\"child\")",
                                         "male_adult",
                                         "female_adult",
                                         "c(\"female\",",
                                         "\"youth\",",
                                         "0")
colnames(Male_female_control_switch)<- gsub("\\","", colnames(Male_female_control_switch),fixed=TRUE )
# 
# milk_units_column<-  data.frame(
#                           "l//day"=1, 
#                           "other"="other",
#                           "l_day"=1,
#                           "0.3l//day"=0.3,
#                           "total"=NA,
#                           "cups_half_l//day"=0.5,
#                           "bottles//day"=NA,
#                           "yourgurt"=NA,
#                           "i_bucket"=NA,
#                           "cup"=NA,
#                           "litres_de_lait_par_semaine"=(1/7),
#                           "litres_pour_six_mois"=(1/6)*(1/28),
#                           "litres"=1,
#                           "por_semana"=1/7,
#                           "10_litros_por_semana"=10/7,
#                           "semana"=1/7,
#                           "par_mois"=1/28,
#                           "1_calebasse_chaque_2_jour"=NA,
#                           "calebasse_par_jour"=NA,
#                           "per_animal_per_week_i_bucket"=NA)
# colnames(milk_units_column)<- gsub("..","/",colnames(milk_units_column), fixed=TRUE )
# colnames(milk_units_column)<- gsub("_"," ",colnames(milk_units_column), fixed=TRUE )
# colnames(milk_units_column)<- gsub("X","",colnames(milk_units_column), fixed=TRUE )
# colnames(milk_units_column)<- gsub("l day","l_day",colnames(milk_units_column), fixed=TRUE )
# colnames(milk_units_column)<- gsub("cups half l/day","cups_half_l/day",colnames(milk_units_column), fixed=TRUE )
# 
# 
# ####---------------------------------------
# 
# 
# land_units<- data.frame("acres"= 0.04,
# "other"= NA,
# "timad"=0.25,
# "lima"=NA,
# "kg_maize_seed_80_equal_one_ha"=NA,
# "0.25_ha"=0.25,
# "carre_25x25m"=0.06,
# "are_25x25m"=0.06,
# "fields"=NA,
# "katha"=NA,
# "bigha"=NA,
# "acre"=0.04,
# "mz"=NA,
# "mraba"=NA,
# "manzanas"=NA,
# "tareas"=NA,
# "igito_60x60m"=0.36,
# "hectare"=1, 
# "mide_0.167ha"= 0.167,
# "NA"=NA,
# "timad_0.25ha"=0.25)
