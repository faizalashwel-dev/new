#detach("package:plyr")
#detach("package:dplyr")

# if("ggplot2" %in% (.packages())){
#   detach("package:ggplot2", unload=TRUE) 
# }
# if("plyr" %in% (.packages())){
#   detach("package:plyr", unload=TRUE) 
# }
# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE) 
# }


project_ID<-'KE_CM2_2016'


Organisation<-"Unknown"
Country<- "Kenya"
Year <- "2016"
cal_line = 2500
pov_line = 1.9



if (length(grep(project_ID, getwd()))==0)
{
  setwd(paste0('Projects/',project_ID))
}

if (sub('.*/','',getwd())==project_ID)
{
  dir.create('Reporting')
  dir.create('Reporting/English')
  
  setwd('Reporting/English')
}




if("ggplot2" %in% (.packages())==F){
  library(ggplot2)
}
if("plyr" %in% (.packages())==F){
  library(plyr)
}
if("dplyr" %in% (.packages())==F){
  library(dplyr)
}

if("readr" %in% (.packages())==F){
  library(readr)
}



dat_all<-data.frame(read_csv("../../Data/Core_Data.csv",na=c('n/a','999','<NA>')))#na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat<-data.frame(read_csv("../../Data/indicator_sheet.csv",na=c('n/a','999','<NA>', 'Inf')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)

dat_crop<-data.frame(read_csv("../../Data/Extra_Outputs/crop_details.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat_lvstk<-data.frame(read_csv("../../Data/Extra_Outputs/livestock_details.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)

#### TreeAID and ADUNA specifics ####

dat_crop_use<-data.frame(read_csv("../../Data/Extra_Outputs/crop_use.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat_crop_residue_use<-data.frame(read_csv("../../Data/Extra_Outputs/crop_residue_use.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat_Crops_Intercropped<-data.frame(read_csv("../../Data/Extra_Outputs/Crops_Intercropped.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
dat_Crops_Monocropped<-data.frame(read_csv("../../Data/Extra_Outputs/Crops_Monocropped.csv",na=c('n/a','999','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)



# dat_value_NTFP_consumed<-read.csv("Input_Data/Value_NTFP_Consumed.csv",na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
# dat_NTFP_incomes<-read.csv("Input_Data/NTFP_Incomes.csv",na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
# dat_NTFP_energies<-read.csv("Input_Data/NTFP_Energies.csv",na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
# 
# Existing_Villages<-unique(dat$Village[dat$Existing_or_New_Village=="Existing Village"])




dir.create("Word_Outputs")
dir.create("Word_Outputs/1.Responses_Summary")
dir.create("Word_Outputs/2.Data_Summary")
dir.create("Word_Outputs/3.Household_Incomes_and_Productivity")
dir.create("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty")
dir.create("Word_Outputs/5.Farm_and_Household_Characteristics")
#dir.create("Word_Outputs/6.Natural_Resource_Management")
dir.create("Word_Outputs/6.Agricultural_Practices")
#dir.create("Word_Outputs/8.NTFPs")
#dir.create("Word_Outputs/9.NTFP_Specifics")

dir.create("Word_Outputs/7.Off_Farm_Sources_Income")
dir.create("Word_Outputs/8.Household_Heads")
#dir.create("Word_Outputs/12.Forest_Governance")
dir.create("Word_Outputs/9.Gender")

source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/TRUE_FALSE_SCRIPT.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/Numeric_Variables.R")

#### A function used to capitalise the first letter of every word in a character string
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
} # A function found online at: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case

dat$Village<- 'NA'
dat_all$village<- 'NA'



source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/1.Responses_Summary.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/2.Data_Summary.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/3.Household_Incomes_and_Productivity.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/4.Indicators_of_Food_Security_and_Poverty.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/5.Farm_and_Household_Characteristics.R")
#source("Word_Scripts/6.Natural_Resource_Management.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/6.Agricultural_Practices.R")
#source("Word_Scripts/8.NTFPs.R")
#source("Word_Scripts/9.NTFP_Specifics.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/7.Off_Farm_Sources_Income.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/8.Household_Heads.R")
#source("Word_Scripts/12.Forest_Governance.R")
source("../../../../Most_Recent_Script_Update/Reporting/English/Word_Scripts/9.Gender.R")


setwd('../../')




