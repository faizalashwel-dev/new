library(readr)

source("Cleaning/Shorten_Column_Names.R")


Correct_Version<- data.frame(read_csv("Data/Different_Versions/RHoMIS_CIRAD_Morocco_draftv6_2019_05_15_10_49_03_306291.csv", na = c("n/a","<NA>", "999", 'NA')))
Version_1<-data.frame(read_csv("Data/Different_Versions/RHoMIS_CIRAD_Morocco_draftv2_2019_05_15_10_47_16_295190.csv", na = c("n/a","<NA>", "999", 'NA')))
Version_2<-data.frame(read_csv("Data/Different_Versions/RHoMIS_CIRAD_Morocco_final_2019_05_15_10_50_12_261945.csv", na = c("n/a","<NA>", "999", 'NA')))
Version_3<-data.frame(read_csv("Data/Different_Versions/RHoMIS_CIRAD_Morocco_final2_2019_05_15_10_59_36_734860.csv", na = c("n/a","<NA>", "999", 'NA')))


#Older_Version<- Older_Version[,colnames(Older_Version) %in%colnames(Correct_Version)]


short_column_names<- shortened_column_names(Correct_Version)
colnames(Correct_Version)<- short_column_names
short_column_names<- shortened_column_names(Version_1)
colnames(Version_1)<- short_column_names
short_column_names<- shortened_column_names(Version_2)
colnames(Version_2)<- short_column_names
short_column_names<- shortened_column_names(Version_3)
colnames(Version_3)<- short_column_names



ncol(Correct_Version)
ncol(Version_1)
ncol(Version_2)
ncol(Version_3)




table(colnames(Version_1)%in%colnames(Correct_Version))
colnames(Version_1)[colnames(Version_1)%in%colnames(Correct_Version)==F]

table(colnames(Version_2)%in%colnames(Correct_Version))
colnames(Version_2)[colnames(Version_2)%in%colnames(Correct_Version)==F]



table(colnames(Version_3)%in%colnames(Correct_Version))
colnames(Version_3)[colnames(Version_3)%in%colnames(Correct_Version)==F]




Merged<-rbind.fill(Correct_Version, Version_1, Version_2, Version_3)






# length(colnames(Correct_Version))
# 
# length(colnames(Older_Version))
# 
# 
# table(colnames(Correct_Version) %in%colnames(Older_Version))
# 
# table(colnames(Older_Version) %in%colnames(Correct_Version))
# 
# 
# colnames(Correct_Version)[which(colnames(Correct_Version) %in%colnames(Older_Version)==F)]
