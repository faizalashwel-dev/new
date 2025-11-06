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

project_ID<-'ZM_FAW_2019'

if (sub('.*/','',getwd())==project_ID)
{
  setwd('Reporting/French')
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

 project_ID<-'KE_CM2_2016'
 
 
 Organisation<-"Unknown"
 Country<- "Kenya"
 Year <- "2016"
 cal_line = 2500
 pov_line = 1.9




dir.create("Word_Outputs")
dir.create("Word_Outputs/1.Resume_des_Enquetes")
dir.create("Word_Outputs/2.Resume_des_donnees")
dir.create("Word_Outputs/3.Revenu_et_Productivite_des_Menages")
dir.create("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire")
dir.create("Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage")
#dir.create("Word_Outputs/6.Natural_Resource_Management")
dir.create("Word_Outputs/6.Pratiques_Agricole")
#dir.create("Word_Outputs/8.NTFPs")
#dir.create("Word_Outputs/9.NTFP_Specifics")

dir.create("Word_Outputs/7.Revenu_Hors_d_Exploitation")
dir.create("Word_Outputs/8.Chef_de_Menage")
#dir.create("Word_Outputs/12.Forest_Governance")
dir.create("Word_Outputs/9.Genre")

source("Word_Scripts/TRUE_FALSE_SCRIPT.R")
source("Word_Scripts/Numeric_Variables.R")

#### A function used to capitalise the first letter of every word in a character string
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
} # A function found online at: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case

dat$Village<- firstup(dat$Village)
dat_all$village<- firstup(dat_all$village)



source("Word_Scripts/1.Responses_Summary.R", encoding = 'UTF-8')
source("Word_Scripts/2.Data_Summary.R", encoding = 'UTF-8')
source("Word_Scripts/3.Household_Incomes_and_Productivity.R", encoding = 'UTF-8')
source("Word_Scripts/4.Indicators_of_Food_Security_and_Poverty.R", encoding = 'UTF-8')
source("Word_Scripts/5.Farm_and_Household_Characteristics.R", encoding = 'UTF-8')
#source("Word_Scripts/6.Natural_Resource_Management.R")
source("Word_Scripts/6.Agricultural_Practices.R", encoding = 'UTF-8')
#source("Word_Scripts/8.NTFPs.R")
#source("Word_Scripts/9.NTFP_Specifics.R")
source("Word_Scripts/7.Off_Farm_Sources_Income.R", encoding = 'UTF-8')
source("Word_Scripts/8.Household_Heads.R", encoding = 'UTF-8')
#source("Word_Scripts/12.Forest_Governance.R")
source("Word_Scripts/9.Gender.R", encoding = 'UTF-8')






