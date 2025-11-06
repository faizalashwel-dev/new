# 'TZ_CFM_2015',
# 'GT_S4N_2015', 
# 'SV_S4N_2015', 
# 'HN_S4N_2015',
# 'ML_CW1_2015',
# 'BF_CW2_2015',
# 'MW_CFG_2015',
# 'KE_CM1_2016',
# 'KE_CM2_2016',
# 'BF_SIL_2016',
# 'IN_CM3_2016',
# 'KH_HAM_2016',
# 'VM_HAM_2016',
# 'LA_HAM_2016',
# 'TZ_SRL_2016',
# 'ET_SRL_2016',
# 'KE_SRL_2016',
# 'TZ_GLV_2017',
# 'CD_FRT_2017',
# 'KE_SCN_2017',
# 'ZM_SCN_2017',
# 'CD_CLP_2017',
# 'ET_TA1_2017',
# 'GH_TA2_2017',
# 'ML_TA3_2017',
# 'UG_CWU_2017',
# 'KE_VCD_2017',
# 'CR_IND_2017',
# 'BI_CLP_2018',
# 'BF_TA4_2018',
# 'TZ_CRA_2018',
# 'PE_MKP_2018',
# 'NI_CFN_2018',
# 'BF_TA5_2018',
# 'ET_TA6_2018',
# 'GH_TA7_2018',


# 'ET_ARI_2018',
#'KE_GLT_2019'
#'RW_OAF_2018'
#'KE_ARI_2019'
# Select which project you are processing

#project_ID<-'RW_OAF_2018'

library(readr)

# if (length(grep(project_ID, getwd()))==0)
# {
#   setwd(paste0('Projects/',project_ID))
# }
dat_all<- data.frame(read_csv("Data/Raw_Data.csv", na = c("n/a","<NA>", "999", 'NA')))

if (project_ID=='KE_GLT_2019')
{
glten_extras<-dat_all[,grep('glten|long_rains|short_rains', colnames(dat_all))]
glten_extras$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other1<- dat_all$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other1
glten_extras$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other2<- dat_all$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other2
glten_extras$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other3<- dat_all$survey_grp.SECTION_Crop_Productivity.crops_other_specify.crops_other3

dat_all<-dat_all[,-grep('glten|long_rains|short_rains', colnames(dat_all))]
write_csv(glten_extras,"Data/GLTEN_Extra_Crop_Loops.csv")

}

source("../../Most_Recent_Script_Update/Cleaning/Shorten_Column_Names.R")
short_column_names<- shortened_column_names(dat_all)
colnames(dat_all)<- short_column_names


if (project_ID=='ET_ARI_2018')
{
dat_all$livestock_heads_cattle<-   rowSums(data.frame(dat_all$livestock_heads_oxen_bulls,dat_all$livestock_heads_cows), na.rm=T)
dat_all$livestock_heads_oxen_bulls<-NULL
dat_all$livestock_heads_cows<-NULL
}

if (length(unique(dat_all$country))==1)
{
if (length(grep('burkina',dat_all$country))>1)
{
  dat_all$country<- 'Burkina_Faso'
}
}
#source('Cleaning/Merging_Versions.R')

Ten_groups_true_false<-0
Fourteen_groups_true_false<-0

Ten_groups<-c("GrainsRootsTubers",
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

Fourteen_Groups<-c("grains",
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
for (i in 1:length(Ten_groups))
{
  if (!is.null(grep(Ten_groups[i], colnames(dat_all))))
  {
    if (length(grep(Ten_groups[i], colnames(dat_all)))!=0)
    { 
      Ten_groups_true_false<-Ten_groups_true_false+1
    }
  }
}

for (j in 1:length(Fourteen_Groups))
{
  if (!is.null(grep(Fourteen_Groups[i], colnames(dat_all))))
  {
    if (length(grep(Fourteen_Groups[i], colnames(dat_all)))!=0)
    { 
      Fourteen_groups_true_false<-Fourteen_groups_true_false+1
    }
  }
}


if (Fourteen_groups_true_false==14){HDDS_type<-'Fourteen_Groups'}
if (Ten_groups_true_false==10){HDDS_type<-'Ten_Groups'}


dat_all$HDDS_type<-HDDS_type

#dat_all<-Merged


YEAR<- gsub('.*_','', project_ID)
COUNTRY_CODE<- gsub('_.*','', project_ID)
PROJECT<- gsub(paste0('_',unique(YEAR)), '', project_ID)
PROJECT<- gsub(paste0(unique(COUNTRY_CODE),'_'), '', PROJECT)
ITERATION<-'1'

ID_HH<- paste0(COUNTRY_CODE,'_', YEAR,'_',PROJECT,'_', row.names(dat_all), '_',ITERATION)


country_names_list<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Country_Names.csv",na=c('n/a','<NA>')), check.names=F)#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat_all$country<- country_names_list$Country_Name[country_names_list$ISO_Country_Code==COUNTRY_CODE]


source("../../Most_Recent_Script_Update/Cleaning/Cleanmotherdatabase.R")


#### IDs should be added here ####


#####Dahari_Specifics #####
#source("Indicator_Calculations/Scripts/Parcelles_Land_Area.R")


if (!is.null(grep('children_under_4|children_4to10|males11to24|males25to50|females25to50|malesover50|femalesover50',colnames(dat_all))))
    {
      if (length(grep('children_under_4|children_4to10|males11to24|males25to50|females25to50|malesover50|femalesover50',colnames(dat_all)))<1)
      {
        source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Age_Loop_Calculation.R")
      }
}



source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Extract_New_Units_per_Country.R")
source("../../Most_Recent_Script_Update/Cleaning/Checking_New_Spellings.R")
write_csv(dat_all, "Data/Cleaned_Data.csv")
source("../../Most_Recent_Script_Update/Cleaning/Extracting_Core_Data.R")





