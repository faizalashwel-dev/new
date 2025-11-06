

#Household_Basics Script

##### A script for household basics: Household ID, Household size variables, Education Levels and land related variables ####

#### HouseholdID ####
HHid<- dat_all$HouseholdID
#-------------------------------------------------------------------------------------------------
#### Household Size Variables ####

# Number of members in the household
# SEARCH_HHsizemembers
HHsizemembers<-     
  data.frame(dat_all$children_under_4,
             dat_all$children_4to10,
             dat_all$males11to24,
             dat_all$females11to24,
             dat_all$males25to50,
             dat_all$females25to50,
             dat_all$malesover50,
             dat_all$femalesover50)
HHsizemembers <- data.frame(lapply(HHsizemembers, as.character), stringsAsFactors=FALSE)

HHsizemembers<- sapply(HHsizemembers , function (x) as.numeric(x))
HHsizemembers<-rowSums(HHsizemembers, na.rm = TRUE)

# Number of members in the household in terms of male adult equivalent (MAE)
#SEARCH_HHsizeMAE
HHsizeMAE<- data.frame(coeff_children_0_to_4*as.numeric(dat_all$children_under_4),
                       coeff_children_4_to_10*as.numeric(dat_all$children_4to10),
                       coeff_male_11_to_24*as.numeric(dat_all$males11to24),
                       coeff_female_11_to_24*as.numeric(dat_all$females11to24),
                       coeff_male_25_to_50*as.numeric(dat_all$males25to50),
                       coeff_female_25_to_50*as.numeric(dat_all$females25to50),
                       coeff_male_50_plus*as.numeric(dat_all$malesover50),
                       coeff_female_50_plus*as.numeric(dat_all$femalesover50))
HHsizeMAE <- data.frame(lapply(HHsizeMAE, as.character), stringsAsFactors=FALSE)

HHsizeMAE<- sapply(HHsizeMAE , function (x) as.numeric(x))
HHsizeMAE<-rowSums(HHsizeMAE, na.rm = TRUE)
HHsizeMAE[HHsizeMAE==0]<-NA
#-------------------------------------------------------------------------------------------------
#### Education Level of Household Head ####
# SEARCH_Head_EducationLevel
Head_EducationLevel<- dat_all$education_level
#-------------------------------------------------------------------------------------------------
#### Land Related Variables (All numerical values measured in Hectares) ####
# SEARCH_LandOwned

unitland_owned<- dat_all$unitland_owned

for(i in 1:length(unitland_owned))
{
  if(unitland_owned[i] %in% colnames(land_units)==TRUE)
  {unitland_owned[i]<- land_units[1,colnames(land_units)==unitland_owned[i]]}
}


LandOwned<- as.numeric(unitland_owned)*as.numeric(dat_all$landowned)


unitland<- dat_all$unitland
for(i in 1:length(unitland))
{
  if(unitland[i] %in% colnames(land_units)==TRUE)
  {unitland[i]<- land_units[1,colnames(land_units)==unitland[i]]}
}


# Land cultivated calculated using 'unitland'
#SEARCH_LandCultivated
LandCultivated<- as.numeric(unitland)*as.numeric(dat_all$landcultivated)
