library(plyr)

dat_all<- data.frame(read_csv("Data/Cleaned_Data.csv", na = c("n/a","<NA>")))#, encoding='UTF-8')

core_cols<- data.frame(read_csv("../../Most_Recent_Script_Update/Cleaning/Core_Column_Names.csv"))#, stringsAsFactors = F, encoding = 'UTF-8')



#### Shows the core columns which do not exist in the new data set ####
core_cols[which(core_cols[,1] %in% colnames(dat_all) ==F),1]

#'Make any necessary changes to column names to get them to match "core column names". 
#'Can keep adding to these changes so as not to not make them every time

# colnames(dat_all)<- gsub("householdID","HouseholdID", colnames(dat_all))
# colnames(dat_all)<- gsub("beneficiary", "beneficiary_TA", colnames(dat_all))
# colnames(dat_all)<- gsub("tillage_power_old", "tillage_power", colnames(dat_all))
# colnames(dat_all)<- gsub("agroforestry_old", "agroforestry", colnames(dat_all))


#' merging the new data set with the column name format

core_data<- data.frame(matrix(ncol = length(core_cols[,1]), nrow = 0))

colnames(core_data)<- core_cols[,1]

core_data<-rbind.fill(core_data, dat_all[,which(colnames(dat_all) %in%  core_cols[,1]==T)])

write_csv(core_data, "Data/Core_Data.csv")


