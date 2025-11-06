#clean

totdat<-dat_all
cropnames_overview<-unique(c(totdat$crop_name_1,totdat$crop_name_2,totdat$crop_name_3,totdat$crop_name_4,totdat$crop_name_5,totdat$crop_name_6,totdat$crop_name_7,totdat$crop_name_8,totdat$crops_other1,totdat$crops_other2,totdat$crops_other3))
length(cropnames_overview)


#### Crop Names####

tocorrect<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv", na = c('NA', 'N/A')))#,stringsAsFactors = F, na.strings = c('na', 'NA', 'N/A'))

for (j in 1:length(tocorrect[,1])) {
  
  if (length(grep('crops_other', colnames(dat_all)))==3)
    for (i in 1:3) {
      index<-as.character(totdat[,paste0('crops_other',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('crops_other',as.character(i))])
      totdat[,paste0('crops_other',as.character(i))][index]<-tocorrect[j,2]
    }
  for (i in 1:length(grep('crop_name_', colnames(totdat)))) {
    index<-as.character(totdat[,paste0('crop_name_',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('crop_name_',as.character(i))])
    totdat[,paste0('crop_name_',as.character(i))][index]<-tocorrect[j,2]
  }
  
 
}

cropnames_overview<-unique(c(totdat$crop_name_1,totdat$crop_name_2,totdat$crop_name_3,totdat$crop_name_4,totdat$crop_name_5,totdat$crop_name_6,totdat$crop_name_7,totdat$crop_name_8,totdat$crops_other1,totdat$crops_other2,totdat$crops_other3))
length(cropnames_overview)
#------------------------------------------------------------------------
#### Livestock Names ####
lvstnames_overview<-unique(c(totdat$livestock_name_1,totdat$livestock_name_2,totdat$livestock_name_3,totdat$livestock_name_4,totdat$livestock_name_5,totdat$livestock_other1,totdat$livestock_other2,totdat$livestock_other3))
length(lvstnames_overview)

tocorrect<- data.frame(read.csv("../../Units_and_Cleaning_Data/Cleaning/Livestock_Spellings.csv", na = c( 'NA', 'N/A')))#, stringsAsFactors = F, na.strings = c('na', 'NA', 'N/A'))


for (j in 1:length(tocorrect[,1])) {
  for (i in 1:length(grep("livestock_name_", colnames(totdat)))) {
    index<-as.character(totdat[,paste0('livestock_name_',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('livestock_name_',as.character(i))])
    totdat[,paste0('livestock_name_',as.character(i))][index]<-as.character(tocorrect[j,2])
  }
  for (i in 1:3) {
    index<-as.character(totdat[,paste0('livestock_other',as.character(i))])==tocorrect[j,1]&!is.na(totdat[,paste0('livestock_other',as.character(i))])
    totdat[,paste0('livestock_other',as.character(i))][index]<-as.character(tocorrect[j,2])
  }
}

# totdat$livestock_other1[6106]<-'NA'
# totdat$livestock_other2[5477]<-'fish'
# totdat$livestock_other1[12752]<-'donkeys_horses'
# totdat$livestock_other1[12753]<-'donkeys_horses'
# totdat$livestock_other1[12748]<-'donkeys_horses'

lvstnames_overview<-unique(c(totdat$livestock_name_1,totdat$livestock_name_2,totdat$livestock_name_3,totdat$livestock_name_4,totdat$livestock_name_5,totdat$livestock_other1,totdat$livestock_other2,totdat$livestock_other3))
length(lvstnames_overview)

#------------------------------------------------------------------------

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------


totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) as.character(tolower(x))))
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) as.character(gsub('female_child', 'female_youth_or_child', x))))
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) as.character(gsub('male_child', 'male_youth_or_child', x))))
#totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) as.character(x)))

for (i in grep('control|who|ownership', colnames(totdat)))
{
  totdat[,i]<-as.character(totdat[,i])
  
}



#########harmonize gender info ####

if (length(totdat$crop_consume_control_1)>0)
{
  for (i in 1:length(totdat$crop_consume_control_1)) {
    totdat$crop_consume_control_1[i]<-trimws(totdat$crop_consume_control_1[i])
  }
  
  for (i in 1:length(totdat$crop_consume_control_1)) {
    if (totdat$crop_consume_control_1[i]=='male'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head'
    }
    if (totdat$crop_consume_control_1[i]=='female'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'female_head'
    }
    if (totdat$crop_consume_control_1[i]=='male female'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_1[i]=='female_youth'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='male_youth'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='child'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_1[i]=='null)'&!is.na(totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-'NA'
    }
  }
  
  #first female then male!!!
  totdat$crop_consume_control_1<-gsub('woman_single','female_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('man_single','male_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub(' child','male_youth_or_child',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('other_family_female','female_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('other_family_male','male_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub(' female_child','female_youth_or_child',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_1)
  
  totdat$crop_consume_control_1<-gsub('female ','female_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('male ','male_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('female_head ','female_adult',totdat$crop_consume_control_1)
  totdat$crop_consume_control_1<-gsub('male_head ','male_adult',totdat$crop_consume_control_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_1)) {
    if (grepl('male_youth',totdat$crop_consume_control_1[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-gsub(' male_youth','male_youth_or_child',totdat$crop_consume_control_1[i])
    }
    if (grepl('female_youth',totdat$crop_consume_control_1[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_1[i])) {
      totdat$crop_consume_control_1[i]<-gsub('female_youth','female_youth_or_child',totdat$crop_consume_control_1[i])
    }
  }      
}
#------------------------------------------------------------------------
########harmonize gender info  ####
if (length(totdat$crop_consume_control_2)>0)
{
  for (i in 1:length(totdat$crop_consume_control_2)) {
    totdat$crop_consume_control_2[i]<-trimws(totdat$crop_consume_control_2[i])
  }
  
  for (i in 1:length(totdat$crop_consume_control_2)) {
    if (totdat$crop_consume_control_2[i]=='male'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head'
    }
    if (totdat$crop_consume_control_2[i]=='female'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'female_head'
    }
    if (totdat$crop_consume_control_2[i]=='male female'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_2[i]=='female_youth'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='male_youth'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='child'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_2[i]=='null)'&!is.na(totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_consume_control_2<-gsub('woman_single','female_head',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('man_single','male_head',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('other_family_female','female_adult',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('other_family_male','male_adult',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('female ','female_head ',totdat$crop_consume_control_2)
  totdat$crop_consume_control_2<-gsub('male ','male_head ',totdat$crop_consume_control_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_2)) {
    if (grepl(' male_youth',totdat$crop_consume_control_2[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_2[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_2[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_2[i])) {
      totdat$crop_consume_control_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_2[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_3)>0)
{
  for (i in 1:length(totdat$crop_consume_control_3)) {
    totdat$crop_consume_control_3[i]<-trimws(totdat$crop_consume_control_3[i])
  }
  
  for (i in 1:length(totdat$crop_consume_control_3)) {
    if (totdat$crop_consume_control_3[i]=='male'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head'
    }
    if (totdat$crop_consume_control_3[i]=='female'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'female_head'
    }
    if (totdat$crop_consume_control_3[i]=='male female'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_3[i]=='female_youth'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='male_youth'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='child'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='youth'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_3[i]=='null)'&!is.na(totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_consume_control_3<-gsub('woman_single','female_head',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('man_single','male_head',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('other_family_female','female_adult',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('other_family_male','male_adult',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_3)
  
  totdat$crop_consume_control_3<-gsub('female ','female_head ',totdat$crop_consume_control_3)
  totdat$crop_consume_control_3<-gsub('male ','male_head ',totdat$crop_consume_control_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_3)) {
    if (grepl(' male_youth',totdat$crop_consume_control_3[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_3[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_3[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_3[i])) {
      totdat$crop_consume_control_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_3[i])
    }
  } 
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_4)>0)
{
  for (i in 1:length(totdat$crop_consume_control_4)) {
    totdat$crop_consume_control_4[i]<-trimws(totdat$crop_consume_control_4[i])
  }
  
  index<-totdat$crop_consume_control_4=='FALSE'
  totdat$crop_consume_control_4[index]<-'NA'
  
  for (i in 1:length(totdat$crop_consume_control_4)) {
    if (totdat$crop_consume_control_4[i]=='male'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head'
    }
    if (totdat$crop_consume_control_4[i]=='female'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'female_head'
    }
    if (totdat$crop_consume_control_4[i]=='male female'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_4[i]=='female_youth'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='male_youth'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='child'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='youth'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_4[i]=='null)'&!is.na(totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-'NA'
    }
  }
  
  #first female then male!!!
  totdat$crop_consume_control_4<-gsub('woman_single','female_head',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('man_single','male_head',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('other_family_female','female_adult',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('other_family_male','male_adult',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_4)
  
  totdat$crop_consume_control_4<-gsub('female ','female_head ',totdat$crop_consume_control_4)
  totdat$crop_consume_control_4<-gsub('male ','male_head ',totdat$crop_consume_control_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_4)) {
    if (grepl(' male_youth',totdat$crop_consume_control_4[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_4[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_4[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_4[i])) {
      totdat$crop_consume_control_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_4[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_5)>0)
{
  for (i in 1:length(totdat$crop_consume_control_5)) {
    totdat$crop_consume_control_5[i]<-trimws(totdat$crop_consume_control_5[i])
  }
  
  index<-totdat$crop_consume_control_5=='FALSE'
  totdat$crop_consume_control_5[index]<-'NA'
  
  for (i in 1:length(totdat$crop_consume_control_5)) {
    if (totdat$crop_consume_control_5[i]=='male'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head'
    }
    if (totdat$crop_consume_control_5[i]=='female'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'female_head'
    }
    if (totdat$crop_consume_control_5[i]=='male female'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_5[i]=='female_youth'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='male_youth'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='child'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='youth'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_5[i]=='null)'&!is.na(totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_consume_control_5<-gsub('woman_single','female_head',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('man_single','male_head',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('other_family_female','female_adult',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('other_family_male','male_adult',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_5)
  
  totdat$crop_consume_control_5<-gsub('female ','female_head ',totdat$crop_consume_control_5)
  totdat$crop_consume_control_5<-gsub('male ','male_head ',totdat$crop_consume_control_5)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_5)) {
    if (grepl(' male_youth',totdat$crop_consume_control_5[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_5[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_5[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_5[i])) {
      totdat$crop_consume_control_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_5[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_6)>0)
{
  if (!is.null(totdat$crop_consume_control_6))
  {
    for (i in 1:length(totdat$crop_consume_control_6)) {
      totdat$crop_consume_control_6[i]<-trimws(totdat$crop_consume_control_6[i])
    }
    
    index<-totdat$crop_consume_control_6=='FALSE'
    totdat$crop_consume_control_6[index]<-'NA'
    
    for (i in 1:length(totdat$crop_consume_control_6)) {
      if (totdat$crop_consume_control_6[i]=='male'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head'
      }
      if (totdat$crop_consume_control_6[i]=='female'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'female_head'
      }
      if (totdat$crop_consume_control_6[i]=='male female'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head female_head'
      }
      if (totdat$crop_consume_control_6[i]=='female_youth'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'female_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='male_youth'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='child'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_consume_control_6[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'male_head female_head'
      }
      if (totdat$crop_consume_control_6[i]=='null)'&!is.na(totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-'NA'
      }
    }
    #first female then male!!!
    totdat$crop_consume_control_6<-gsub('woman_single','female_head',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('man_single','male_head',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('other_family_female','female_adult',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('other_family_male','male_adult',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_6)
    
    totdat$crop_consume_control_6<-gsub('female ','female_head ',totdat$crop_consume_control_6)
    totdat$crop_consume_control_6<-gsub('male ','male_head ',totdat$crop_consume_control_6)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$crop_consume_control_6)) {
      if (grepl(' male_youth',totdat$crop_consume_control_6[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_6[i])
      }
      if (grepl(' female_youth',totdat$crop_consume_control_6[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_6[i])) {
        totdat$crop_consume_control_6[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_6[i])
      }
    }      
  }
}

#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_7)>0)
{
  for (i in 1:length(totdat$crop_consume_control_7)) {
    totdat$crop_consume_control_7[i]<-trimws(totdat$crop_consume_control_7[i])
  }
  
  index<-totdat$crop_consume_control_7=='FALSE'
  totdat$crop_consume_control_7[index]<-'NA'
  
  for (i in 1:length(totdat$crop_consume_control_7)) {
    if (totdat$crop_consume_control_7[i]=='male'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head'
    }
    if (totdat$crop_consume_control_7[i]=='female'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'female_head'
    }
    if (totdat$crop_consume_control_7[i]=='male female'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_7[i]=='female_youth'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='male_youth'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='child'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_7[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_7[i]=='null)'&!is.na(totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_consume_control_7<-gsub('woman_single','female_head',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('man_single','male_head',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('other_family_female','female_adult',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('other_family_male','male_adult',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_7)
  
  totdat$crop_consume_control_7<-gsub('female ','female_head ',totdat$crop_consume_control_7)
  totdat$crop_consume_control_7<-gsub('male ','male_head ',totdat$crop_consume_control_7)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_7)) {
    if (grepl(' male_youth',totdat$crop_consume_control_7[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_7[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_7[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_7[i])) {
      totdat$crop_consume_control_7[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_7[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_consume_control_8)>0)
{
  for (i in 1:length(totdat$crop_consume_control_8)) {
    totdat$crop_consume_control_8[i]<-trimws(totdat$crop_consume_control_8[i])
  }
  
  index<-totdat$crop_consume_control_8=='FALSE'
  totdat$crop_consume_control_8[index]<-'NA'
  
  for (i in 1:length(totdat$crop_consume_control_8)) {
    if (totdat$crop_consume_control_8[i]=='male'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head'
    }
    if (totdat$crop_consume_control_8[i]=='female'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'female_head'
    }
    if (totdat$crop_consume_control_8[i]=='male female'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_8[i]=='female_youth'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'female_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='male_youth'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='child'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_consume_control_8[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'male_head female_head'
    }
    if (totdat$crop_consume_control_8[i]=='null)'&!is.na(totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_consume_control_8<-gsub('woman_single','female_head',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('man_single','male_head',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub(' child',' male_youth_or_child',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('other_family_female','female_adult',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('other_family_male','male_adult',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('female_youth ','female_youth_or_child ',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('male_youth ','male_youth_or_child ',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('female_child ','female_youth_or_child ',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub(' female_child',' female_youth_or_child',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('male_child ','male_youth_or_child ',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub(' male_child',' male_youth_or_child',totdat$crop_consume_control_8)
  
  totdat$crop_consume_control_8<-gsub('female ','female_head ',totdat$crop_consume_control_8)
  totdat$crop_consume_control_8<-gsub('male ','male_head ',totdat$crop_consume_control_8)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_consume_control_8)) {
    if (grepl(' male_youth',totdat$crop_consume_control_8[i])&!grepl('male_youth_or_child',totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_consume_control_8[i])
    }
    if (grepl(' female_youth',totdat$crop_consume_control_8[i])&!grepl('female_youth_or_child',totdat$crop_consume_control_8[i])) {
      totdat$crop_consume_control_8[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_consume_control_8[i])
    }
  }      
  
}
#------------------------------------------------------------------------
#########harmonize gender info #####

if (length(totdat$crop_who_control_revenue_1)>0)
{
  for (i in 1:length(totdat$crop_who_control_revenue_1)) {
    totdat$crop_who_control_revenue_1[i]<-trimws(totdat$crop_who_control_revenue_1[i])
  }
  
  for (i in 1:length(totdat$crop_who_control_revenue_1)) {
    if (totdat$crop_who_control_revenue_1[i]=='male'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head'
    }
    if (totdat$crop_who_control_revenue_1[i]=='female'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'female_head'
    }
    if (totdat$crop_who_control_revenue_1[i]=='male female'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_1[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'female_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='child'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='youth'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_1[i]=='null)'&!is.na(totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_who_control_revenue_1<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('man_single','male_head',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_1)
  
  totdat$crop_who_control_revenue_1<-gsub('female ','female_head ',totdat$crop_who_control_revenue_1)
  totdat$crop_who_control_revenue_1<-gsub('male ','male_head ',totdat$crop_who_control_revenue_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_who_control_revenue_1)) {
    if (grepl(' male_youth',totdat$crop_who_control_revenue_1[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_1[i])
    }
    if (grepl(' female_youth',totdat$crop_who_control_revenue_1[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_1[i])) {
      totdat$crop_who_control_revenue_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_1[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_who_control_revenue_2)>0)
{
  for (i in 1:length(totdat$crop_who_control_revenue_2)) {
    totdat$crop_who_control_revenue_2[i]<-trimws(totdat$crop_who_control_revenue_2[i])
  }
  
  for (i in 1:length(totdat$crop_who_control_revenue_2)) {
    if (totdat$crop_who_control_revenue_2[i]=='male'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head'
    }
    if (totdat$crop_who_control_revenue_2[i]=='female'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'female_head'
    }
    if (totdat$crop_who_control_revenue_2[i]=='male female'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_2[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'female_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='child'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='youth'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_2[i]=='null)'&!is.na(totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_who_control_revenue_2<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('man_single','male_head',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_2)
  
  totdat$crop_who_control_revenue_2<-gsub('female ','female_head ',totdat$crop_who_control_revenue_2)
  totdat$crop_who_control_revenue_2<-gsub('male ','male_head ',totdat$crop_who_control_revenue_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_who_control_revenue_2)) {
    if (grepl(' male_youth',totdat$crop_who_control_revenue_2[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_2[i])
    }
    if (grepl(' female_youth',totdat$crop_who_control_revenue_2[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_2[i])) {
      totdat$crop_who_control_revenue_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_2[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_who_control_revenue_3)>0)
{
  for (i in 1:length(totdat$crop_who_control_revenue_3)) {
    totdat$crop_who_control_revenue_3[i]<-trimws(totdat$crop_who_control_revenue_3[i])
  }
  
  for (i in 1:length(totdat$crop_who_control_revenue_3)) {
    if (totdat$crop_who_control_revenue_3[i]=='male'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head'
    }
    if (totdat$crop_who_control_revenue_3[i]=='female'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'female_head'
    }
    if (totdat$crop_who_control_revenue_3[i]=='male female'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_3[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'female_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='child'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='youth'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_3[i]=='null)'&!is.na(totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_who_control_revenue_3<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('man_single','male_head',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_3)
  
  totdat$crop_who_control_revenue_3<-gsub('female ','female_head ',totdat$crop_who_control_revenue_3)
  totdat$crop_who_control_revenue_3<-gsub('male ','male_head ',totdat$crop_who_control_revenue_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_who_control_revenue_3)) {
    if (grepl(' male_youth',totdat$crop_who_control_revenue_3[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_3[i])
    }
    if (grepl(' female_youth',totdat$crop_who_control_revenue_3[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_3[i])) {
      totdat$crop_who_control_revenue_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_3[i])
    }
  }    
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_who_control_revenue_4)>0)
{
  for (i in 1:length(totdat$crop_who_control_revenue_4)) {
    totdat$crop_who_control_revenue_4[i]<-trimws(totdat$crop_who_control_revenue_4[i])
  }
  
  index<-totdat$crop_who_control_revenue_4=='FALSE'
  totdat$crop_who_control_revenue_4[index]<-'NA'
  
  for (i in 1:length(totdat$crop_who_control_revenue_4)) {
    if (totdat$crop_who_control_revenue_4[i]=='male'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head'
    }
    if (totdat$crop_who_control_revenue_4[i]=='female'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'female_head'
    }
    if (totdat$crop_who_control_revenue_4[i]=='male female'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_4[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'female_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='child'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='youth'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"c(\"female\", \"youth\", \"child\")")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_4[i]=='null)'&!is.na(totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_who_control_revenue_4<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('man_single','male_head',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('female ','female_head ',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_4)
  totdat$crop_who_control_revenue_4<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_4)
  
  totdat$crop_who_control_revenue_4<-gsub('male ','male_head ',totdat$crop_who_control_revenue_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_who_control_revenue_4)) {
    if (grepl(' male_youth',totdat$crop_who_control_revenue_4[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_4[i])
    }
    if (grepl(' female_youth',totdat$crop_who_control_revenue_4[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_4[i])) {
      totdat$crop_who_control_revenue_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_4[i])
    }
  }  
}

#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_who_control_revenue_5)>0)
{
  for (i in 1:length(totdat$crop_who_control_revenue_5)) {
    totdat$crop_who_control_revenue_5[i]<-trimws(totdat$crop_who_control_revenue_5[i])
  }
  
  index<-totdat$crop_who_control_revenue_5=='FALSE'
  totdat$crop_who_control_revenue_5[index]<-'NA'
  
  for (i in 1:length(totdat$crop_who_control_revenue_5)) {
    if (totdat$crop_who_control_revenue_5[i]=='male'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head'
    }
    if (totdat$crop_who_control_revenue_5[i]=='female'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'female_head'
    }
    if (totdat$crop_who_control_revenue_5[i]=='male female'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_5[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'female_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='child'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_who_control_revenue_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'male_head female_head'
    }
    if (totdat$crop_who_control_revenue_5[i]=='null)'&!is.na(totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-'NA'
    }
  }
  #first female then male!!!
  totdat$crop_who_control_revenue_5<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('man_single','male_head',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_5)
  
  totdat$crop_who_control_revenue_5<-gsub('female ','female_head ',totdat$crop_who_control_revenue_5)
  totdat$crop_who_control_revenue_5<-gsub('male ','male_head ',totdat$crop_who_control_revenue_5)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_who_control_revenue_5)) {
    if (grepl(' male_youth',totdat$crop_who_control_revenue_5[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_5[i])
    }
    if (grepl(' female_youth',totdat$crop_who_control_revenue_5[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_5[i])) {
      totdat$crop_who_control_revenue_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_5[i])
    }
  }      
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$crop_who_control_revenue_6)>0)
{
  if (!is.null(totdat$crop_who_control_revenue_6))
  {
    for (i in 1:length(totdat$crop_who_control_revenue_6)) {
      totdat$crop_who_control_revenue_6[i]<-trimws(totdat$crop_who_control_revenue_6[i])
    }
    
    index<-totdat$crop_who_control_revenue_6=='FALSE'
    totdat$crop_who_control_revenue_6[index]<-'NA'
    
    for (i in 1:length(totdat$crop_who_control_revenue_6)) {
      if (totdat$crop_who_control_revenue_6[i]=='male'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head'
      }
      if (totdat$crop_who_control_revenue_6[i]=='female'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'female_head'
      }
      if (totdat$crop_who_control_revenue_6[i]=='male female'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_6[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'female_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='child'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_6[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_6[i]=='null)'&!is.na(totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-'NA'
      }
    }
    #first female then male!!!
    totdat$crop_who_control_revenue_6<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('man_single','male_head',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_6)
    
    totdat$crop_who_control_revenue_6<-gsub('female ','female_head ',totdat$crop_who_control_revenue_6)
    totdat$crop_who_control_revenue_6<-gsub('male ','male_head ',totdat$crop_who_control_revenue_6)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$crop_who_control_revenue_6)) {
      if (grepl(' male_youth',totdat$crop_who_control_revenue_6[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_6[i])
      }
      if (grepl(' female_youth',totdat$crop_who_control_revenue_6[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_6[i])) {
        totdat$crop_who_control_revenue_6[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_6[i])
      }
    }      
  }
  #------------------------------------------------------------------------
  #########harmonize gender info #####
  if (length(totdat$crop_who_control_revenue_7)>0)
  {
    for (i in 1:length(totdat$crop_who_control_revenue_7)) {
      totdat$crop_who_control_revenue_7[i]<-trimws(totdat$crop_who_control_revenue_7[i])
    }
    
    index<-totdat$crop_who_control_revenue_7=='FALSE'
    totdat$crop_who_control_revenue_7[index]<-'NA'
    
    for (i in 1:length(totdat$crop_who_control_revenue_7)) {
      if (totdat$crop_who_control_revenue_7[i]=='male'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head'
      }
      if (totdat$crop_who_control_revenue_7[i]=='female'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'female_head'
      }
      if (totdat$crop_who_control_revenue_7[i]=='male female'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_7[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'female_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='child'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_7[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_7[i]=='null)'&!is.na(totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-'NA'
      }
    }
    #first female then male!!!
    totdat$crop_who_control_revenue_7<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('man_single','male_head',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_7)
    
    totdat$crop_who_control_revenue_7<-gsub('female ','female_head ',totdat$crop_who_control_revenue_7)
    totdat$crop_who_control_revenue_7<-gsub('male ','male_head ',totdat$crop_who_control_revenue_7)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$crop_who_control_revenue_7)) {
      if (grepl(' male_youth',totdat$crop_who_control_revenue_7[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_7[i])
      }
      if (grepl(' female_youth',totdat$crop_who_control_revenue_7[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_7[i])) {
        totdat$crop_who_control_revenue_7[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_7[i])
      }
    }      
  }
  #------------------------------------------------------------------------
  #########harmonize gender info #####
  if (length(totdat$crop_who_control_revenue_8)>0)
  {
    for (i in 1:length(totdat$crop_who_control_revenue_8)) {
      totdat$crop_who_control_revenue_8[i]<-trimws(totdat$crop_who_control_revenue_8[i])
    }
    
    index<-totdat$crop_who_control_revenue_8=='FALSE'
    totdat$crop_who_control_revenue_8[index]<-'NA'
    
    for (i in 1:length(totdat$crop_who_control_revenue_8)) {
      if (totdat$crop_who_control_revenue_8[i]=='male'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head'
      }
      if (totdat$crop_who_control_revenue_8[i]=='female'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'female_head'
      }
      if (totdat$crop_who_control_revenue_8[i]=='male female'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_8[i]=='female_youth'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'female_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='male_youth'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='child'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head female_head male_youth_or_child'
      }
      if (totdat$crop_who_control_revenue_8[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'male_head female_head'
      }
      if (totdat$crop_who_control_revenue_8[i]=='null)'&!is.na(totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-'NA'
      }
    }
    #first female then male!!!
    totdat$crop_who_control_revenue_8<-gsub('woman_single','female_head',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('man_single','male_head',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub(' child',' male_youth_or_child',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('other_family_female','female_adult',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('other_family_male','male_adult',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('female_youth ','female_youth_or_child ',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('male_youth ','male_youth_or_child ',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('female_child ','female_youth_or_child ',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub(' female_child',' female_youth_or_child',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('male_child ','male_youth_or_child ',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub(' male_child',' male_youth_or_child',totdat$crop_who_control_revenue_8)
    
    totdat$crop_who_control_revenue_8<-gsub('female ','female_head ',totdat$crop_who_control_revenue_8)
    totdat$crop_who_control_revenue_8<-gsub('male ','male_head ',totdat$crop_who_control_revenue_8)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$crop_who_control_revenue_8)) {
      if (grepl(' male_youth',totdat$crop_who_control_revenue_8[i])&!grepl('male_youth_or_child',totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_who_control_revenue_8[i])
      }
      if (grepl(' female_youth',totdat$crop_who_control_revenue_8[i])&!grepl('female_youth_or_child',totdat$crop_who_control_revenue_8[i])) {
        totdat$crop_who_control_revenue_8[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_who_control_revenue_8[i])
      }
    }      
  }
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (length(totdat$land_ownership)>0)
{
  for (i in 1:length(totdat$land_ownership)) {
    totdat$land_ownership[i]<-trimws(totdat$land_ownership[i])
  }
  
  index<-totdat$land_ownership=='FALSE'
  totdat$land_ownership[index]<-'NA'
  
  for (i in 1:length(totdat$land_ownership)) {
    if (totdat$land_ownership[i]=='male'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head'
    }
    if (totdat$land_ownership[i]=='female'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'female_head'
    }
    if (totdat$land_ownership[i]=='male female'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head'
    }
    if (totdat$land_ownership[i]=='female_youth'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'female_youth_or_child'
    }
    if (totdat$land_ownership[i]=='male_youth'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='child'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"female\", \"child\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'female_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"female\", \"youth\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'female_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"male\", \"youth\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$land_ownership[i]=='c(\"male\", \"female\")'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head'
    }
    if (totdat$land_ownership[i]=='null)'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'NA'
    }
    if (totdat$land_ownership[i]=='joint)'&!is.na(totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$land_ownership<-gsub('woman_single','female_head',totdat$land_ownership)
  totdat$land_ownership<-gsub('man_single','male_head',totdat$land_ownership)
  totdat$land_ownership<-gsub(' child',' male_youth_or_child',totdat$land_ownership)
  totdat$land_ownership<-gsub('other_family_female','female_adult',totdat$land_ownership)
  totdat$land_ownership<-gsub('other_family_male','male_adult',totdat$land_ownership)
  totdat$land_ownership<-gsub('female_youth ','female_youth_or_child ',totdat$land_ownership)
  totdat$land_ownership<-gsub('male_youth ','male_youth_or_child ',totdat$land_ownership)
  totdat$land_ownership<-gsub('female_child ','female_youth_or_child ',totdat$land_ownership)
  totdat$land_ownership<-gsub(' female_child',' female_youth_or_child',totdat$land_ownership)
  totdat$land_ownership<-gsub('male_child ','male_youth_or_child ',totdat$land_ownership)
  totdat$land_ownership<-gsub(' male_child',' male_youth_or_child',totdat$land_ownership)
  
  totdat$land_ownership<-gsub('female ','female_head ',totdat$land_ownership)
  totdat$land_ownership<-gsub('male ','male_head ',totdat$land_ownership)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$land_ownership)) {
    if (grepl(' male_youth',totdat$land_ownership[i])&!grepl('male_youth_or_child',totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-gsub(' male_youth',' male_youth_or_child',totdat$land_ownership[i])
    }
    if (grepl(' female_youth',totdat$land_ownership[i])&!grepl('female_youth_or_child',totdat$land_ownership[i])) {
      totdat$land_ownership[i]<-gsub(' female_youth',' female_youth_or_child',totdat$land_ownership[i])
    }
  }      
  
  index<-totdat$land_ownership=='joint'
  totdat$land_ownership[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$crop_products_consume_control))
{
  for (i in 1:length(totdat$crop_products_consume_control)) {
    totdat$crop_products_consume_control[i]<-trimws(totdat$crop_products_consume_control[i])
  }
  
  index<-totdat$crop_products_consume_control=='FALSE'
  totdat$crop_products_consume_control[index]<-'NA'
  
  for (i in 1:length(totdat$crop_products_consume_control)) {
    if (totdat$crop_products_consume_control[i]=='male'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head'
    }
    if (totdat$crop_products_consume_control[i]=='female'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'female_head'
    }
    if (totdat$crop_products_consume_control[i]=='male female'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head'
    }
    if (totdat$crop_products_consume_control[i]=='female_youth'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'female_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='male_youth'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='child'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_consume_control[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head'
    }
    if (totdat$crop_products_consume_control[i]=='null)'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'NA'
    }
    if (totdat$crop_products_consume_control[i]=='joint)'&!is.na(totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$crop_products_consume_control<-gsub('woman_single','female_head',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('man_single','male_head',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub(' child',' male_youth_or_child',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('other_family_female','female_adult',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('other_family_male','male_adult',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('female_youth ','female_youth_or_child ',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('male_youth ','male_youth_or_child ',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('female_child ','female_youth_or_child ',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub(' female_child',' female_youth_or_child',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('male_child ','male_youth_or_child ',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub(' male_child',' male_youth_or_child',totdat$crop_products_consume_control)
  
  totdat$crop_products_consume_control<-gsub('female ','female_head ',totdat$crop_products_consume_control)
  totdat$crop_products_consume_control<-gsub('male ','male_head ',totdat$crop_products_consume_control)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_products_consume_control)) {
    if (grepl(' male_youth',totdat$crop_products_consume_control[i])&!grepl('male_youth_or_child',totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_products_consume_control[i])
    }
    if (grepl(' female_youth',totdat$crop_products_consume_control[i])&!grepl('female_youth_or_child',totdat$crop_products_consume_control[i])) {
      totdat$crop_products_consume_control[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_products_consume_control[i])
    }
  }      
  
  index<-totdat$crop_products_consume_control=='joint'
  totdat$crop_products_consume_control[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #########harmonize gender info #####
  for (i in 1:length(totdat$crop_products_who_control_revenue)) {
    totdat$crop_products_who_control_revenue[i]<-trimws(totdat$crop_products_who_control_revenue[i])
  }
  
  index<-totdat$crop_products_who_control_revenue=='FALSE'
  totdat$crop_products_who_control_revenue[index]<-'NA'
  
  for (i in 1:length(totdat$crop_products_who_control_revenue)) {
    if (totdat$crop_products_who_control_revenue[i]=='male'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head'
    }
    if (totdat$crop_products_who_control_revenue[i]=='female'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'female_head'
    }
    if (totdat$crop_products_who_control_revenue[i]=='male female'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head'
    }
    if (totdat$crop_products_who_control_revenue[i]=='female_youth'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'female_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='male_youth'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='child'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"female\", \"child\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"female\", \"youth\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'female_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"male\", \"youth\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$crop_products_who_control_revenue[i]=='c(\"male\", \"female\")'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head'
    }
    if (totdat$crop_products_who_control_revenue[i]=='null)'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'NA'
    }
    if (totdat$crop_products_who_control_revenue[i]=='joint)'&!is.na(totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$crop_products_who_control_revenue<-gsub('woman_single','female_head',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('man_single','male_head',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub(' child',' male_youth_or_child',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('other_family_female','female_adult',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('other_family_male','male_adult',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('female_youth ','female_youth_or_child ',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('male_youth ','male_youth_or_child ',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('female_child ','female_youth_or_child ',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub(' female_child',' female_youth_or_child',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('male_child ','male_youth_or_child ',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub(' male_child',' male_youth_or_child',totdat$crop_products_who_control_revenue)
  
  totdat$crop_products_who_control_revenue<-gsub('female ','female_head ',totdat$crop_products_who_control_revenue)
  totdat$crop_products_who_control_revenue<-gsub('male ','male_head ',totdat$crop_products_who_control_revenue)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$crop_products_who_control_revenue)) {
    if (grepl(' male_youth',totdat$crop_products_who_control_revenue[i])&!grepl('male_youth_or_child',totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-gsub(' male_youth',' male_youth_or_child',totdat$crop_products_who_control_revenue[i])
    }
    if (grepl(' female_youth',totdat$crop_products_who_control_revenue[i])&!grepl('female_youth_or_child',totdat$crop_products_who_control_revenue[i])) {
      totdat$crop_products_who_control_revenue[i]<-gsub(' female_youth',' female_youth_or_child',totdat$crop_products_who_control_revenue[i])
    }
  }      
  
  index<-totdat$crop_products_who_control_revenue=='joint'
  totdat$crop_products_who_control_revenue[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$dairy_products_who_sells))
{
  for (i in 1:length(totdat$dairy_products_who_sells)) {
    totdat$dairy_products_who_sells[i]<-trimws(totdat$dairy_products_who_sells[i])
  }
  
  index<-totdat$dairy_products_who_sells=='FALSE'
  totdat$dairy_products_who_sells[index]<-'NA'
  
  for (i in 1:length(totdat$dairy_products_who_sells)) {
    if (totdat$dairy_products_who_sells[i]=='male'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head'
    }
    if (totdat$dairy_products_who_sells[i]=='female'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'female_head'
    }
    if (totdat$dairy_products_who_sells[i]=='male female'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head'
    }
    if (totdat$dairy_products_who_sells[i]=='female_youth'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'female_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='male_youth'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='child'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"female\", \"child\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"female\", \"youth\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"male\", \"youth\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_sells[i]=='c(\"male\", \"female\")'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head'
    }
    if (totdat$dairy_products_who_sells[i]=='null)'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'NA'
    }
    if (totdat$dairy_products_who_sells[i]=='joint)'&!is.na(totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$dairy_products_who_sells<-gsub('woman_single','female_head',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('man_single','male_head',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub(' child',' male_youth_or_child',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('other_family_female','female_adult',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('other_family_male','male_adult',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('female_youth ','female_youth_or_child ',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('male_youth ','male_youth_or_child ',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('female_child ','female_youth_or_child ',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub(' female_child',' female_youth_or_child',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('male_child ','male_youth_or_child ',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub(' male_child',' male_youth_or_child',totdat$dairy_products_who_sells)
  
  totdat$dairy_products_who_sells<-gsub('female ','female_head ',totdat$dairy_products_who_sells)
  totdat$dairy_products_who_sells<-gsub('male ','male_head ',totdat$dairy_products_who_sells)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$dairy_products_who_sells)) {
    if (grepl(' male_youth',totdat$dairy_products_who_sells[i])&!grepl('male_youth_or_child',totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-gsub(' male_youth',' male_youth_or_child',totdat$dairy_products_who_sells[i])
    }
    if (grepl(' female_youth',totdat$dairy_products_who_sells[i])&!grepl('female_youth_or_child',totdat$dairy_products_who_sells[i])) {
      totdat$dairy_products_who_sells[i]<-gsub(' female_youth',' female_youth_or_child',totdat$dairy_products_who_sells[i])
    }
  }      
  
  index<-totdat$dairy_products_who_sells=='joint'
  totdat$dairy_products_who_sells[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #########harmonize gender info #####
  for (i in 1:length(totdat$dairy_products_who_control_eating)) {
    totdat$dairy_products_who_control_eating[i]<-trimws(totdat$dairy_products_who_control_eating[i])
  }
  
  index<-totdat$dairy_products_who_control_eating=='FALSE'
  totdat$dairy_products_who_control_eating[index]<-'NA'
  
  for (i in 1:length(totdat$dairy_products_who_control_eating)) {
    if (totdat$dairy_products_who_control_eating[i]=='male'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head'
    }
    if (totdat$dairy_products_who_control_eating[i]=='female'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'female_head'
    }
    if (totdat$dairy_products_who_control_eating[i]=='male female'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head'
    }
    if (totdat$dairy_products_who_control_eating[i]=='female_youth'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'female_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='male_youth'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='child'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"female\", \"child\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"female\", \"youth\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"male\", \"youth\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$dairy_products_who_control_eating[i]=='c(\"male\", \"female\")'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head'
    }
    if (totdat$dairy_products_who_control_eating[i]=='null)'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'NA'
    }
    if (totdat$dairy_products_who_control_eating[i]=='joint)'&!is.na(totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$dairy_products_who_control_eating<-gsub('woman_single','female_head',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('man_single','male_head',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub(' child',' male_youth_or_child',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('other_family_female','female_adult',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('other_family_male','male_adult',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('female_youth ','female_youth_or_child ',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('male_youth ','male_youth_or_child ',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('female_child ','female_youth_or_child ',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub(' female_child',' female_youth_or_child',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('male_child ','male_youth_or_child ',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub(' male_child',' male_youth_or_child',totdat$dairy_products_who_control_eating)
  
  totdat$dairy_products_who_control_eating<-gsub('female ','female_head ',totdat$dairy_products_who_control_eating)
  totdat$dairy_products_who_control_eating<-gsub('male ','male_head ',totdat$dairy_products_who_control_eating)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$dairy_products_who_control_eating)) {
    if (grepl(' male_youth',totdat$dairy_products_who_control_eating[i])&!grepl('male_youth_or_child',totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-gsub(' male_youth',' male_youth_or_child',totdat$dairy_products_who_control_eating[i])
    }
    if (grepl(' female_youth',totdat$dairy_products_who_control_eating[i])&!grepl('female_youth_or_child',totdat$dairy_products_who_control_eating[i])) {
      totdat$dairy_products_who_control_eating[i]<-gsub(' female_youth',' female_youth_or_child',totdat$dairy_products_who_control_eating[i])
    }
  }      
  
  index<-totdat$dairy_products_who_control_eating=='joint'
  totdat$dairy_products_who_control_eating[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_wool_who_sells))
{
  for (i in 1:length(totdat$livestock_wool_who_sells)) {
    totdat$livestock_wool_who_sells[i]<-trimws(totdat$livestock_wool_who_sells[i])
  }
  
  index<-totdat$livestock_wool_who_sells=='FALSE'
  totdat$livestock_wool_who_sells[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_wool_who_sells)) {
    if (totdat$livestock_wool_who_sells[i]=='male'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head'
    }
    if (totdat$livestock_wool_who_sells[i]=='female'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'female_head'
    }
    if (totdat$livestock_wool_who_sells[i]=='male female'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head'
    }
    if (totdat$livestock_wool_who_sells[i]=='female_youth'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='male_youth'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='child'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"female\", \"youth\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"male\", \"youth\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$livestock_wool_who_sells[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head'
    }
    if (totdat$livestock_wool_who_sells[i]=='null)'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'NA'
    }
    if (totdat$livestock_wool_who_sells[i]=='joint)'&!is.na(totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$livestock_wool_who_sells<-gsub('woman_single','female_head',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('man_single','male_head',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub(' child',' male_youth_or_child',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('other_family_female','female_adult',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('other_family_male','male_adult',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('female_child ','female_youth_or_child ',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub(' female_child',' female_youth_or_child',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('male_child ','male_youth_or_child ',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub(' male_child',' male_youth_or_child',totdat$livestock_wool_who_sells)
  
  totdat$livestock_wool_who_sells<-gsub('female ','female_head ',totdat$livestock_wool_who_sells)
  totdat$livestock_wool_who_sells<-gsub('male ','male_head ',totdat$livestock_wool_who_sells)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_wool_who_sells)) {
    if (grepl(' male_youth',totdat$livestock_wool_who_sells[i])&!grepl('male_youth_or_child',totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_wool_who_sells[i])
    }
    if (grepl(' female_youth',totdat$livestock_wool_who_sells[i])&!grepl('female_youth_or_child',totdat$livestock_wool_who_sells[i])) {
      totdat$livestock_wool_who_sells[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_wool_who_sells[i])
    }
  }      
  
  index<-totdat$livestock_wool_who_sells=='joint'
  totdat$livestock_wool_who_sells[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$wildfoods_who_control_revenue))
{
  for (i in 1:length(totdat$wildfoods_who_control_revenue)) {
    totdat$wildfoods_who_control_revenue[i]<-trimws(totdat$wildfoods_who_control_revenue[i])
  }
  
  index<-totdat$wildfoods_who_control_revenue=='FALSE'
  totdat$wildfoods_who_control_revenue[index]<-'NA'
  
  for (i in 1:length(totdat$wildfoods_who_control_revenue)) {
    if (totdat$wildfoods_who_control_revenue[i]=='male'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='female'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'female_head'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='male female'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='female_youth'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'female_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='male_youth'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='child'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"female\", \"child\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'female_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"female\", \"youth\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'female_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"male\", \"youth\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"male\", \"female\", \"youth\", \"child\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='c(\"male\", \"female\")'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='null)'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'NA'
    }
    if (totdat$wildfoods_who_control_revenue[i]=='joint)'&!is.na(totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-'male_head female_head'
    }
  }
  #first female then male!!!
  totdat$wildfoods_who_control_revenue<-gsub('woman_single','female_head',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('man_single','male_head',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub(' child',' male_youth_or_child',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('other_family_female','female_adult',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('other_family_male','male_adult',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('female_youth ','female_youth_or_child ',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('male_youth ','male_youth_or_child ',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('female_child ','female_youth_or_child ',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub(' female_child',' female_youth_or_child',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('male_child ','male_youth_or_child ',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub(' male_child',' male_youth_or_child',totdat$wildfoods_who_control_revenue)
  
  totdat$wildfoods_who_control_revenue<-gsub('female ','female_head ',totdat$wildfoods_who_control_revenue)
  totdat$wildfoods_who_control_revenue<-gsub('male ','male_head ',totdat$wildfoods_who_control_revenue)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$wildfoods_who_control_revenue)) {
    if (grepl(' male_youth',totdat$wildfoods_who_control_revenue[i])&!grepl('male_youth_or_child',totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-gsub(' male_youth',' male_youth_or_child',totdat$wildfoods_who_control_revenue[i])
    }
    if (grepl(' female_youth',totdat$wildfoods_who_control_revenue[i])&!grepl('female_youth_or_child',totdat$wildfoods_who_control_revenue[i])) {
      totdat$wildfoods_who_control_revenue[i]<-gsub(' female_youth',' female_youth_or_child',totdat$wildfoods_who_control_revenue[i])
    }
  }      
  
  index<-totdat$wildfoods_who_control_revenue=='joint'
  totdat$wildfoods_who_control_revenue[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (!is.null(totdat$livestock_ownership_1))
{
  for (i in 1:length(totdat$livestock_ownership_1)) {
    totdat$livestock_ownership_1[i]<-trimws(totdat$livestock_ownership_1[i])
  }
  
  index<-totdat$livestock_ownership_1=='FALSE'
  totdat$livestock_ownership_1[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_ownership_1)) {
    if (totdat$livestock_ownership_1[i]=='male'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'male_head'
    }
    if (totdat$livestock_ownership_1[i]=='male_child'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_1[i]=='female_child'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_1[i]=='female'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'female_head'
    }
    if (totdat$livestock_ownership_1[i]=='male female'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_ownership_1[i]=='female_youth'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_1[i]=='male_youth'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_1[i]=='child'&!is.na(totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-'male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_ownership_1<-gsub('woman_single','female_head',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('man_single','male_head',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub(' child',' male_youth_or_child',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('other_family_female','female_adult',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('other_family_male','male_adult',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('female_child ','female_youth_or_child ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub(' female_child',' female_youth_or_child',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('male_child ','male_youth_or_child ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub(' male_child',' male_youth_or_child',totdat$livestock_ownership_1)
  
  totdat$livestock_ownership_1<-gsub('female ','female_head ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub('male ','male_head ',totdat$livestock_ownership_1)
  totdat$livestock_ownership_1<-gsub(' outside_person','',totdat$livestock_ownership_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_ownership_1)) {
    if (grepl(' male_youth',totdat$livestock_ownership_1[i])&!grepl('male_youth_or_child',totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_ownership_1[i])
    }
    if (grepl(' female_youth',totdat$livestock_ownership_1[i])&!grepl('female_youth_or_child',totdat$livestock_ownership_1[i])) {
      totdat$livestock_ownership_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_ownership_1[i])
    }
  }      
  
  index<-totdat$livestock_ownership_1=='joint'
  totdat$livestock_ownership_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if (!is.null(totdat$livestock_ownership_2))
{
  for (i in 1:length(totdat$livestock_ownership_2)) {
    totdat$livestock_ownership_2[i]<-trimws(totdat$livestock_ownership_2[i])
  }
  
  index<-totdat$livestock_ownership_2=='FALSE'
  totdat$livestock_ownership_2[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_ownership_2)) {
    if (totdat$livestock_ownership_2[i]=='male'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'male_head'
    }
    if (totdat$livestock_ownership_2[i]=='male_child'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_2[i]=='female'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'female_head'
    }
    if (totdat$livestock_ownership_2[i]=='male female'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_ownership_2[i]=='female_youth'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_2[i]=='male_youth'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_2[i]=='child'&!is.na(totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-'male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_ownership_2<-gsub('woman_single','female_head',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('man_single','male_head',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub(' child',' male_youth_or_child',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('other_family_female','female_adult',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('other_family_male','male_adult',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('female_child ','female_youth_or_child ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub(' female_child',' female_youth_or_child',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('male_child ','male_youth_or_child ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub(' male_child',' male_youth_or_child',totdat$livestock_ownership_2)
  
  totdat$livestock_ownership_2<-gsub('female ','female_head ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub('male ','male_head ',totdat$livestock_ownership_2)
  totdat$livestock_ownership_2<-gsub(' outside_person','',totdat$livestock_ownership_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_ownership_2)) {
    if (grepl(' male_youth',totdat$livestock_ownership_2[i])&!grepl('male_youth_or_child',totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_ownership_2[i])
    }
    if (grepl(' female_youth',totdat$livestock_ownership_2[i])&!grepl('female_youth_or_child',totdat$livestock_ownership_2[i])) {
      totdat$livestock_ownership_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_ownership_2[i])
    }
  }      
  
  index<-totdat$livestock_ownership_2=='joint'
  totdat$livestock_ownership_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####

if(!is.null(totdat$livestock_ownership_3))
{
  for (i in 1:length(totdat$livestock_ownership_3)) {
    totdat$livestock_ownership_3[i]<-trimws(totdat$livestock_ownership_3[i])
  }
  
  index<-totdat$livestock_ownership_3=='FALSE'
  totdat$livestock_ownership_3[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_ownership_3)) {
    if (totdat$livestock_ownership_3[i]=='male'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'male_head'
    }
    if (totdat$livestock_ownership_3[i]=='male_child'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_3[i]=='female'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'female_head'
    }
    if (totdat$livestock_ownership_3[i]=='male female'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_ownership_3[i]=='female_youth'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_3[i]=='male_youth'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_3[i]=='child'&!is.na(totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-'male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_ownership_3<-gsub('woman_single','female_head',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('man_single','male_head',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub(' child',' male_youth_or_child',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('other_family_female','female_adult',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('other_family_male','male_adult',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('female_child ','female_youth_or_child ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub(' female_child',' female_youth_or_child',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('male_child ','male_youth_or_child ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub(' male_child',' male_youth_or_child',totdat$livestock_ownership_3)
  
  totdat$livestock_ownership_3<-gsub('female ','female_head ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub('male ','male_head ',totdat$livestock_ownership_3)
  totdat$livestock_ownership_3<-gsub(' outside_person','',totdat$livestock_ownership_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_ownership_3)) {
    if (grepl(' male_youth',totdat$livestock_ownership_3[i])&!grepl('male_youth_or_child',totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_ownership_3[i])
    }
    if (grepl(' female_youth',totdat$livestock_ownership_3[i])&!grepl('female_youth_or_child',totdat$livestock_ownership_3[i])) {
      totdat$livestock_ownership_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_ownership_3[i])
    }
  }      
  
  index<-totdat$livestock_ownership_3=='joint'
  totdat$livestock_ownership_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_ownership_4))
{
  for (i in 1:length(totdat$livestock_ownership_4)) {
    totdat$livestock_ownership_4[i]<-trimws(totdat$livestock_ownership_4[i])
  }
  
  index<-totdat$livestock_ownership_4=='FALSE'
  totdat$livestock_ownership_4[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_ownership_4)) {
    if (totdat$livestock_ownership_4[i]=='male'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'male_head'
    }
    if (totdat$livestock_ownership_4[i]=='male_child'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_4[i]=='female'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'female_head'
    }
    if (totdat$livestock_ownership_4[i]=='male female'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_ownership_4[i]=='female_youth'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_4[i]=='male_youth'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_4[i]=='child'&!is.na(totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-'male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_ownership_4<-gsub('woman_single','female_head',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('man_single','male_head',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub(' child',' male_youth_or_child',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('other_family_female','female_adult',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('other_family_male','male_adult',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('female_child ','female_youth_or_child ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub(' female_child',' female_youth_or_child',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('male_child ','male_youth_or_child ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub(' male_child',' male_youth_or_child',totdat$livestock_ownership_4)
  
  totdat$livestock_ownership_4<-gsub('female ','female_head ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub('male ','male_head ',totdat$livestock_ownership_4)
  totdat$livestock_ownership_4<-gsub(' outside_person','',totdat$livestock_ownership_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_ownership_4)) {
    if (grepl(' male_youth',totdat$livestock_ownership_4[i])&!grepl('male_youth_or_child',totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_ownership_4[i])
    }
    if (grepl(' female_youth',totdat$livestock_ownership_4[i])&!grepl('female_youth_or_child',totdat$livestock_ownership_4[i])) {
      totdat$livestock_ownership_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_ownership_4[i])
    }
  }      
  
  index<-totdat$livestock_ownership_4=='joint'
  totdat$livestock_ownership_4[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_ownership_5))
{
  for (i in 1:length(totdat$livestock_ownership_5)) {
    totdat$livestock_ownership_5[i]<-trimws(totdat$livestock_ownership_5[i])
  }
  
  index<-totdat$livestock_ownership_5=='FALSE'
  totdat$livestock_ownership_5[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_ownership_5)) {
    if (totdat$livestock_ownership_5[i]=='male'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'male_head'
    }
    if (totdat$livestock_ownership_5[i]=='male_child'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_5[i]=='female'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'female_head'
    }
    if (totdat$livestock_ownership_5[i]=='male female'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'male_head female_head'
    }
    if (totdat$livestock_ownership_5[i]=='female_youth'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_ownership_5[i]=='male_youth'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_ownership_5[i]=='child'&!is.na(totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-'male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_ownership_5<-gsub('woman_single','female_head',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('man_single','male_head',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub(' child',' male_youth_or_child',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('other_family_female','female_adult',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('other_family_male','male_adult',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('female_child ','female_youth_or_child ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub(' female_child',' female_youth_or_child',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('male_child ','male_youth_or_child ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub(' male_child',' male_youth_or_child',totdat$livestock_ownership_5)
  
  totdat$livestock_ownership_5<-gsub('female ','female_head ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub('male ','male_head ',totdat$livestock_ownership_5)
  totdat$livestock_ownership_5<-gsub(' outside_person','',totdat$livestock_ownership_5)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_ownership_5)) {
    if (grepl(' male_youth',totdat$livestock_ownership_5[i])&!grepl('male_youth_or_child',totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_ownership_5[i])
    }
    if (grepl(' female_youth',totdat$livestock_ownership_5[i])&!grepl('female_youth_or_child',totdat$livestock_ownership_5[i])) {
      totdat$livestock_ownership_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_ownership_5[i])
    }
  }      
  
  index<-totdat$livestock_ownership_5=='joint'
  totdat$livestock_ownership_5[index]<-'male_head female_head'
}

#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_who_sells_1))
{
  for (i in 1:length(totdat$livestock_who_sells_1)) {
    totdat$livestock_who_sells_1[i]<-trimws(totdat$livestock_who_sells_1[i])
  }
  
  index<-totdat$livestock_who_sells_1=='FALSE'
  totdat$livestock_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_who_sells_1)) {
    if (totdat$livestock_who_sells_1[i]=='male'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_head'
    }
    if (totdat$livestock_who_sells_1[i]=='male_child'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='female_child'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='female'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'female_head'
    }
    if (totdat$livestock_who_sells_1[i]=='male female'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_1[i]=='female_youth'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='male_youth'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='child'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_who_sells_1<-gsub('woman_single','female_head',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('man_single','male_head',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('other_family_female','female_adult',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('other_family_male','male_adult',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$livestock_who_sells_1)
  
  totdat$livestock_who_sells_1<-gsub('female ','female_head ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub('male ','male_head ',totdat$livestock_who_sells_1)
  totdat$livestock_who_sells_1<-gsub(' outside_person','',totdat$livestock_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_who_sells_1)) {
    if (grepl(' male_youth',totdat$livestock_who_sells_1[i])&!grepl('male_youth_or_child',totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$livestock_who_sells_1[i])&!grepl('female_youth_or_child',totdat$livestock_who_sells_1[i])) {
      totdat$livestock_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_who_sells_1[i])
    }
  }      
  
  index<-totdat$livestock_who_sells_1=='joint'
  totdat$livestock_who_sells_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_who_sells_2))
{
  for (i in 1:length(totdat$livestock_who_sells_2)) {
    totdat$livestock_who_sells_2[i]<-trimws(totdat$livestock_who_sells_2[i])
  }
  
  index<-totdat$livestock_who_sells_2=='FALSE'
  totdat$livestock_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_who_sells_2)) {
    if (totdat$livestock_who_sells_2[i]=='male'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_head'
    }
    if (totdat$livestock_who_sells_2[i]=='male_child'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_2[i]=='female'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'female_head'
    }
    if (totdat$livestock_who_sells_2[i]=='male female'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_2[i]=='female_youth'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_who_sells_2[i]=='male_youth'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_2[i]=='child'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_who_sells_2<-gsub('woman_single','female_head',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('man_single','male_head',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('other_family_female','female_adult',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('other_family_male','male_adult',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$livestock_who_sells_2)
  
  totdat$livestock_who_sells_2<-gsub('female ','female_head ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub('male ','male_head ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub(' outside_person','',totdat$livestock_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_who_sells_2)) {
    if (grepl(' male_youth',totdat$livestock_who_sells_2[i])&!grepl('male_youth_or_child',totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$livestock_who_sells_2[i])&!grepl('female_youth_or_child',totdat$livestock_who_sells_2[i])) {
      totdat$livestock_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_who_sells_2[i])
    }
  }      
  
  index<-totdat$livestock_who_sells_2=='joint'
  totdat$livestock_who_sells_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_who_sells_3))
{
  for (i in 1:length(totdat$livestock_who_sells_3)) {
    totdat$livestock_who_sells_3[i]<-trimws(totdat$livestock_who_sells_3[i])
  }
  
  index<-totdat$livestock_who_sells_3=='FALSE'
  totdat$livestock_who_sells_3[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_who_sells_3)) {
    if (totdat$livestock_who_sells_3[i]=='male'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_head'
    }
    if (totdat$livestock_who_sells_3[i]=='male_child'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_3[i]=='female'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'female_head'
    }
    if (totdat$livestock_who_sells_3[i]=='male female'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_3[i]=='female_youth'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_who_sells_3[i]=='male_youth'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_3[i]=='child'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_who_sells_3<-gsub('woman_single','female_head',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('man_single','male_head',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('other_family_female','female_adult',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('other_family_male','male_adult',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_2)
  totdat$livestock_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('female ','female_head ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub('male ','male_head ',totdat$livestock_who_sells_3)
  totdat$livestock_who_sells_3<-gsub(' outside_person','',totdat$livestock_who_sells_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_who_sells_3)) {
    if (grepl(' male_youth',totdat$livestock_who_sells_3[i])&!grepl('male_youth_or_child',totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_who_sells_3[i])
    }
    if (grepl(' female_youth',totdat$livestock_who_sells_3[i])&!grepl('female_youth_or_child',totdat$livestock_who_sells_3[i])) {
      totdat$livestock_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_who_sells_3[i])
    }
  }      
  
  index<-totdat$livestock_who_sells_3=='joint'
  totdat$livestock_who_sells_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_who_sells_4))
{
  for (i in 1:length(totdat$livestock_who_sells_4)) {
    totdat$livestock_who_sells_4[i]<-trimws(totdat$livestock_who_sells_4[i])
  }
  
  index<-totdat$livestock_who_sells_4=='FALSE'
  totdat$livestock_who_sells_4[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_who_sells_4)) {
    if (totdat$livestock_who_sells_4[i]=='male'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_head'
    }
    if (totdat$livestock_who_sells_4[i]=='male_child'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_4[i]=='female'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'female_head'
    }
    if (totdat$livestock_who_sells_4[i]=='male female'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_4[i]=='female_youth'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_who_sells_4[i]=='male_youth'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_4[i]=='child'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_who_sells_4<-gsub('woman_single','female_head',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('man_single','male_head',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('other_family_female','female_adult',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('other_family_male','male_adult',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_4)
  
  totdat$livestock_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$livestock_who_sells_4)
  
  totdat$livestock_who_sells_4<-gsub('female ','female_head ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub('male ','male_head ',totdat$livestock_who_sells_4)
  totdat$livestock_who_sells_4<-gsub(' outside_person','',totdat$livestock_who_sells_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_who_sells_4)) {
    if (grepl(' male_youth',totdat$livestock_who_sells_4[i])&!grepl('male_youth_or_child',totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_who_sells_4[i])
    }
    if (grepl(' female_youth',totdat$livestock_who_sells_4[i])&!grepl('female_youth_or_child',totdat$livestock_who_sells_4[i])) {
      totdat$livestock_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_who_sells_4[i])
    }
  }      
  
  index<-totdat$livestock_who_sells_4=='joint'
  totdat$livestock_who_sells_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #########harmonize gender info #####
  if(!is.null(totdat$livestock_who_sells_5))
  {
    for (i in 1:length(totdat$livestock_who_sells_5)) {
      totdat$livestock_who_sells_5[i]<-trimws(totdat$livestock_who_sells_5[i])
    }
    
    index<-totdat$livestock_who_sells_5=='FALSE'
    totdat$livestock_who_sells_5[index]<-'NA'
    
    for (i in 1:length(totdat$livestock_who_sells_5)) {
      if (totdat$livestock_who_sells_5[i]=='male'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_head'
      }
      if (totdat$livestock_who_sells_5[i]=='male_child'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_who_sells_5[i]=='female'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'female_head'
      }
      if (totdat$livestock_who_sells_5[i]=='male female'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$livestock_who_sells_5[i]=='female_youth'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'female_youth_or_child'
      }
      if (totdat$livestock_who_sells_5[i]=='male_youth'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_who_sells_5[i]=='child'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$livestock_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$livestock_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$livestock_who_sells_5<-gsub('woman_single','female_head',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('man_single','male_head',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('other_family_female','female_adult',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('other_family_male','male_adult',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$livestock_who_sells_5)
    
    totdat$livestock_who_sells_5<-gsub('female ','female_head ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub('male ','male_head ',totdat$livestock_who_sells_5)
    totdat$livestock_who_sells_5<-gsub(' outside_person','',totdat$livestock_who_sells_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$livestock_who_sells_5)) {
      if (grepl(' male_youth',totdat$livestock_who_sells_5[i])&!grepl('male_youth_or_child',totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_who_sells_5[i])
      }
      if (grepl(' female_youth',totdat$livestock_who_sells_5[i])&!grepl('female_youth_or_child',totdat$livestock_who_sells_5[i])) {
        totdat$livestock_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_who_sells_5[i])
      }
    }      
    
    index<-totdat$livestock_who_sells_5=='joint'
    totdat$livestock_who_sells_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_sells_1))
{
  for (i in 1:length(totdat$livestock_meat_who_sells_1)) {
    totdat$livestock_meat_who_sells_1[i]<-trimws(totdat$livestock_meat_who_sells_1[i])
  }
  
  index<-totdat$livestock_meat_who_sells_1=='FALSE'
  totdat$livestock_meat_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_sells_1)) {
    if (totdat$livestock_meat_who_sells_1[i]=='male'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='male_child'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='female_child'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='female'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='male female'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='female_youth'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='male_youth'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='child'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_sells_1<-gsub('woman_single','female_head',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('man_single','male_head',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_sells_1)
  
  totdat$livestock_meat_who_sells_1<-gsub('female ','female_head ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub('male ','male_head ',totdat$livestock_meat_who_sells_1)
  totdat$livestock_meat_who_sells_1<-gsub(' outside_person','',totdat$livestock_meat_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_sells_1)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_sells_1[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_sells_1[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_sells_1[i])) {
      totdat$livestock_meat_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_sells_1[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_sells_1=='joint'
  totdat$livestock_meat_who_sells_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_sells_2))
{
  for (i in 1:length(totdat$livestock_meat_who_sells_2)) {
    totdat$livestock_meat_who_sells_2[i]<-trimws(totdat$livestock_meat_who_sells_2[i])
  }
  
  index<-totdat$livestock_meat_who_sells_2=='FALSE'
  totdat$livestock_meat_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_sells_2)) {
    if (totdat$livestock_meat_who_sells_2[i]=='male'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='male_child'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='female'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='male female'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='female_youth'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='male_youth'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='child'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_sells_2<-gsub('woman_single','female_head',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('man_single','male_head',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_sells_2)
  
  totdat$livestock_meat_who_sells_2<-gsub('female ','female_head ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub('male ','male_head ',totdat$livestock_meat_who_sells_2)
  totdat$livestock_meat_who_sells_2<-gsub(' outside_person','',totdat$livestock_meat_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_sells_2)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_sells_2[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_sells_2[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_sells_2[i])) {
      totdat$livestock_meat_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_sells_2[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_sells_2=='joint'
  totdat$livestock_meat_who_sells_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#########harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_sells_3))
{
  for (i in 1:length(totdat$livestock_meat_who_sells_3)) {
    totdat$livestock_meat_who_sells_3[i]<-trimws(totdat$livestock_meat_who_sells_3[i])
  }
  
  index<-totdat$livestock_meat_who_sells_3=='FALSE'
  totdat$livestock_meat_who_sells_3[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_sells_3)) {
    if (totdat$livestock_meat_who_sells_3[i]=='male'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='male_child'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='female'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='male female'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='female_youth'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='male_youth'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='child'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_sells_3<-gsub('woman_single','female_head',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('man_single','male_head',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_sells_3)
  
  totdat$livestock_meat_who_sells_3<-gsub('female ','female_head ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub('male ','male_head ',totdat$livestock_meat_who_sells_3)
  totdat$livestock_meat_who_sells_3<-gsub(' outside_person','',totdat$livestock_meat_who_sells_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_sells_3)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_sells_3[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_sells_3[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_sells_3[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_sells_3[i])) {
      totdat$livestock_meat_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_sells_3[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_sells_3=='joint'
  totdat$livestock_meat_who_sells_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_sells_4))
{
  for (i in 1:length(totdat$livestock_meat_who_sells_4)) {
    totdat$livestock_meat_who_sells_4[i]<-trimws(totdat$livestock_meat_who_sells_4[i])
  }
  
  index<-totdat$livestock_meat_who_sells_4=='FALSE'
  totdat$livestock_meat_who_sells_4[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_sells_4)) {
    if (totdat$livestock_meat_who_sells_4[i]=='male'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='male_child'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='female'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='male female'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='female_youth'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='male_youth'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='child'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_sells_4<-gsub('woman_single','female_head',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('man_single','male_head',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_sells_4)
  
  totdat$livestock_meat_who_sells_4<-gsub('female ','female_head ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub('male ','male_head ',totdat$livestock_meat_who_sells_4)
  totdat$livestock_meat_who_sells_4<-gsub(' outside_person','',totdat$livestock_meat_who_sells_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_sells_4)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_sells_4[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_sells_4[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_sells_4[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_sells_4[i])) {
      totdat$livestock_meat_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_sells_4[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_sells_4=='joint'
  totdat$livestock_meat_who_sells_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$livestock_meat_who_sells_5))
  {
    for (i in 1:length(totdat$livestock_meat_who_sells_5)) {
      totdat$livestock_meat_who_sells_5[i]<-trimws(totdat$livestock_meat_who_sells_5[i])
    }
    
    index<-totdat$livestock_meat_who_sells_5=='FALSE'
    totdat$livestock_meat_who_sells_5[index]<-'NA'
    
    for (i in 1:length(totdat$livestock_meat_who_sells_5)) {
      if (totdat$livestock_meat_who_sells_5[i]=='male'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_head'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='male_child'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='female'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'female_head'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='male female'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='female_youth'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'female_youth_or_child'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='male_youth'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='child'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$livestock_meat_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$livestock_meat_who_sells_5<-gsub('woman_single','female_head',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('man_single','male_head',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_sells_5)
    
    totdat$livestock_meat_who_sells_5<-gsub('female ','female_head ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub('male ','male_head ',totdat$livestock_meat_who_sells_5)
    totdat$livestock_meat_who_sells_5<-gsub(' outside_person','',totdat$livestock_meat_who_sells_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$livestock_meat_who_sells_5)) {
      if (grepl(' male_youth',totdat$livestock_meat_who_sells_5[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_sells_5[i])
      }
      if (grepl(' female_youth',totdat$livestock_meat_who_sells_5[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_sells_5[i])) {
        totdat$livestock_meat_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_sells_5[i])
      }
    }      
    
    index<-totdat$livestock_meat_who_sells_5=='joint'
    totdat$livestock_meat_who_sells_5[index]<-'male_head female_head'
    
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_control_eating_1))
{
  for (i in 1:length(totdat$livestock_meat_who_control_eating_1)) {
    totdat$livestock_meat_who_control_eating_1[i]<-trimws(totdat$livestock_meat_who_control_eating_1[i])
  }
  
  index<-totdat$livestock_meat_who_control_eating_1=='FALSE'
  totdat$livestock_meat_who_control_eating_1[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_control_eating_1)) {
    if (totdat$livestock_meat_who_control_eating_1[i]=='male'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='male_child'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='female_child'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='female'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='male female'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='female_youth'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='male_youth'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='child'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_control_eating_1<-gsub('woman_single','female_head',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('man_single','male_head',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_1)
  
  totdat$livestock_meat_who_control_eating_1<-gsub('female ','female_head ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub('male ','male_head ',totdat$livestock_meat_who_control_eating_1)
  totdat$livestock_meat_who_control_eating_1<-gsub(' outside_person','',totdat$livestock_meat_who_control_eating_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_control_eating_1)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_control_eating_1[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_control_eating_1[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_control_eating_1[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_control_eating_1[i])) {
      totdat$livestock_meat_who_control_eating_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_control_eating_1[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_control_eating_1=='joint'
  totdat$livestock_meat_who_control_eating_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_control_eating_2))
{
  for (i in 1:length(totdat$livestock_meat_who_control_eating_2)) {
    totdat$livestock_meat_who_control_eating_2[i]<-trimws(totdat$livestock_meat_who_control_eating_2[i])
  }
  
  index<-totdat$livestock_meat_who_control_eating_2=='FALSE'
  totdat$livestock_meat_who_control_eating_2[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_control_eating_2)) {
    if (totdat$livestock_meat_who_control_eating_2[i]=='male'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='male_child'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='female'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='male female'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='female_youth'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='male_youth'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='child'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_control_eating_2<-gsub('woman_single','female_head',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('man_single','male_head',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_2)
  
  totdat$livestock_meat_who_control_eating_2<-gsub('female ','female_head ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub('male ','male_head ',totdat$livestock_meat_who_control_eating_2)
  totdat$livestock_meat_who_control_eating_2<-gsub(' outside_person','',totdat$livestock_meat_who_control_eating_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_control_eating_2)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_control_eating_2[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_control_eating_2[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_control_eating_2[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_control_eating_2[i])) {
      totdat$livestock_meat_who_control_eating_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_control_eating_2[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_control_eating_2=='joint'
  totdat$livestock_meat_who_control_eating_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_control_eating_3))
{
  for (i in 1:length(totdat$livestock_meat_who_control_eating_3)) {
    totdat$livestock_meat_who_control_eating_3[i]<-trimws(totdat$livestock_meat_who_control_eating_3[i])
  }
  
  index<-totdat$livestock_meat_who_control_eating_3=='FALSE'
  totdat$livestock_meat_who_control_eating_3[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_control_eating_3)) {
    if (totdat$livestock_meat_who_control_eating_3[i]=='male'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='male_child'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='female'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='male female'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='female_youth'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='male_youth'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='child'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_control_eating_3<-gsub('woman_single','female_head',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('man_single','male_head',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_3)
  
  totdat$livestock_meat_who_control_eating_3<-gsub('female ','female_head ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub('male ','male_head ',totdat$livestock_meat_who_control_eating_3)
  totdat$livestock_meat_who_control_eating_3<-gsub(' outside_person','',totdat$livestock_meat_who_control_eating_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_control_eating_3)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_control_eating_3[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_control_eating_3[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_control_eating_3[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_control_eating_3[i])) {
      totdat$livestock_meat_who_control_eating_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_control_eating_3[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_control_eating_3=='joint'
  totdat$livestock_meat_who_control_eating_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$livestock_meat_who_control_eating_4))
{
  for (i in 1:length(totdat$livestock_meat_who_control_eating_4)) {
    totdat$livestock_meat_who_control_eating_4[i]<-trimws(totdat$livestock_meat_who_control_eating_4[i])
  }
  
  index<-totdat$livestock_meat_who_control_eating_4=='FALSE'
  totdat$livestock_meat_who_control_eating_4[index]<-'NA'
  
  for (i in 1:length(totdat$livestock_meat_who_control_eating_4)) {
    if (totdat$livestock_meat_who_control_eating_4[i]=='male'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_head'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='male_child'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='female'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'female_head'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='male female'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_head female_head'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='female_youth'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='male_youth'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='child'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$livestock_meat_who_control_eating_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$livestock_meat_who_control_eating_4<-gsub('woman_single','female_head',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('man_single','male_head',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_4)
  
  totdat$livestock_meat_who_control_eating_4<-gsub('female ','female_head ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub('male ','male_head ',totdat$livestock_meat_who_control_eating_4)
  totdat$livestock_meat_who_control_eating_4<-gsub(' outside_person','',totdat$livestock_meat_who_control_eating_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$livestock_meat_who_control_eating_4)) {
    if (grepl(' male_youth',totdat$livestock_meat_who_control_eating_4[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_control_eating_4[i])
    }
    if (grepl(' female_youth',totdat$livestock_meat_who_control_eating_4[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_control_eating_4[i])) {
      totdat$livestock_meat_who_control_eating_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_control_eating_4[i])
    }
  }      
  
  index<-totdat$livestock_meat_who_control_eating_4=='joint'
  totdat$livestock_meat_who_control_eating_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$livestock_meat_who_control_eating_5))
  {
    for (i in 1:length(totdat$livestock_meat_who_control_eating_5)) {
      totdat$livestock_meat_who_control_eating_5[i]<-trimws(totdat$livestock_meat_who_control_eating_5[i])
    }
    
    index<-totdat$livestock_meat_who_control_eating_5=='FALSE'
    totdat$livestock_meat_who_control_eating_5[index]<-'NA'
    
    for (i in 1:length(totdat$livestock_meat_who_control_eating_5)) {
      if (totdat$livestock_meat_who_control_eating_5[i]=='male'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_head'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='male_child'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='female'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'female_head'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='male female'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_head female_head'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='female_youth'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'female_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='male_youth'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='child'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_head female_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_head male_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$livestock_meat_who_control_eating_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$livestock_meat_who_control_eating_5<-gsub('woman_single','female_head',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('man_single','male_head',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub(' child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('other_family_female','female_adult',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('other_family_male','male_adult',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('female_youth ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('male_youth ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('female_child ','female_youth_or_child ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub(' female_child',' female_youth_or_child',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('male_child ','male_youth_or_child ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub(' male_child',' male_youth_or_child',totdat$livestock_meat_who_control_eating_5)
    
    totdat$livestock_meat_who_control_eating_5<-gsub('female ','female_head ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub('male ','male_head ',totdat$livestock_meat_who_control_eating_5)
    totdat$livestock_meat_who_control_eating_5<-gsub(' outside_person','',totdat$livestock_meat_who_control_eating_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$livestock_meat_who_control_eating_5)) {
      if (grepl(' male_youth',totdat$livestock_meat_who_control_eating_5[i])&!grepl('male_youth_or_child',totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$livestock_meat_who_control_eating_5[i])
      }
      if (grepl(' female_youth',totdat$livestock_meat_who_control_eating_5[i])&!grepl('female_youth_or_child',totdat$livestock_meat_who_control_eating_5[i])) {
        totdat$livestock_meat_who_control_eating_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$livestock_meat_who_control_eating_5[i])
      }
    }      
    
    index<-totdat$livestock_meat_who_control_eating_5=='joint'
    totdat$livestock_meat_who_control_eating_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_sells_1))
{
  for (i in 1:length(totdat$milk_who_sells_1)) {
    totdat$milk_who_sells_1[i]<-trimws(totdat$milk_who_sells_1[i])
  }
  
  index<-totdat$milk_who_sells_1=='FALSE'
  totdat$milk_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_sells_1)) {
    if (totdat$milk_who_sells_1[i]=='male'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_head'
    }
    if (totdat$milk_who_sells_1[i]=='male_child'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='female_child'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='female'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'female_head'
    }
    if (totdat$milk_who_sells_1[i]=='male female'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$milk_who_sells_1[i]=='female_youth'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='male_youth'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='child'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_sells_1<-gsub('woman_single','female_head',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('man_single','male_head',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('other_family_female','female_adult',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('other_family_male','male_adult',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('female ','female_head ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub('male ','male_head ',totdat$milk_who_sells_1)
  totdat$milk_who_sells_1<-gsub(' outside_person','',totdat$milk_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_sells_1)) {
    if (grepl(' male_youth',totdat$milk_who_sells_1[i])&!grepl('male_youth_or_child',totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$milk_who_sells_1[i])&!grepl('female_youth_or_child',totdat$milk_who_sells_1[i])) {
      totdat$milk_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_sells_1[i])
    }
  }      
  
  index<-totdat$milk_who_sells_1=='joint'
  totdat$milk_who_sells_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_sells_2))
{
  for (i in 1:length(totdat$milk_who_sells_2)) {
    totdat$milk_who_sells_2[i]<-trimws(totdat$milk_who_sells_2[i])
  }
  
  index<-totdat$milk_who_sells_2=='FALSE'
  totdat$milk_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_sells_2)) {
    if (totdat$milk_who_sells_2[i]=='male'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_head'
    }
    if (totdat$milk_who_sells_2[i]=='male_child'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='female'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'female_head'
    }
    if (totdat$milk_who_sells_2[i]=='male female'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$milk_who_sells_2[i]=='female_youth'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='male_youth'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='child'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_sells_2<-gsub('woman_single','female_head',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('man_single','male_head',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('other_family_female','female_adult',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('other_family_male','male_adult',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('female ','female_head ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub('male ','male_head ',totdat$milk_who_sells_2)
  totdat$milk_who_sells_2<-gsub(' outside_person','',totdat$milk_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_sells_2)) {
    if (grepl(' male_youth',totdat$milk_who_sells_2[i])&!grepl('male_youth_or_child',totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$milk_who_sells_2[i])&!grepl('female_youth_or_child',totdat$milk_who_sells_2[i])) {
      totdat$milk_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_sells_2[i])
    }
  }      
  
  index<-totdat$milk_who_sells_2=='joint'
  totdat$milk_who_sells_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_sells_3))
{
  for (i in 1:length(totdat$milk_who_sells_3)) {
    totdat$milk_who_sells_3[i]<-trimws(totdat$milk_who_sells_3[i])
  }
  
  index<-totdat$milk_who_sells_3=='FALSE'
  totdat$milk_who_sells_3[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_sells_3)) {
    if (totdat$milk_who_sells_3[i]=='male'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_head'
    }
    if (totdat$milk_who_sells_3[i]=='male_child'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='female'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'female_head'
    }
    if (totdat$milk_who_sells_3[i]=='male female'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$milk_who_sells_3[i]=='female_youth'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='male_youth'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='child'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_sells_3<-gsub('woman_single','female_head',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('man_single','male_head',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('other_family_female','female_adult',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('other_family_male','male_adult',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('female ','female_head ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub('male ','male_head ',totdat$milk_who_sells_3)
  totdat$milk_who_sells_3<-gsub(' outside_person','',totdat$milk_who_sells_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_sells_3)) {
    if (grepl(' male_youth',totdat$milk_who_sells_3[i])&!grepl('male_youth_or_child',totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_sells_3[i])
    }
    if (grepl(' female_youth',totdat$milk_who_sells_3[i])&!grepl('female_youth_or_child',totdat$milk_who_sells_3[i])) {
      totdat$milk_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_sells_3[i])
    }
  }      
  
  index<-totdat$milk_who_sells_3=='joint'
  totdat$milk_who_sells_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_sells_4))
{
  for (i in 1:length(totdat$milk_who_sells_4)) {
    totdat$milk_who_sells_4[i]<-trimws(totdat$milk_who_sells_4[i])
  }
  
  index<-totdat$milk_who_sells_4=='FALSE'
  totdat$milk_who_sells_4[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_sells_4)) {
    if (totdat$milk_who_sells_4[i]=='male'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_head'
    }
    if (totdat$milk_who_sells_4[i]=='male_child'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='female'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'female_head'
    }
    if (totdat$milk_who_sells_4[i]=='male female'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$milk_who_sells_4[i]=='female_youth'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='male_youth'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='child'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_sells_4<-gsub('woman_single','female_head',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('man_single','male_head',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('other_family_female','female_adult',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('other_family_male','male_adult',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('female ','female_head ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub('male ','male_head ',totdat$milk_who_sells_4)
  totdat$milk_who_sells_4<-gsub(' outside_person','',totdat$milk_who_sells_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_sells_4)) {
    if (grepl(' male_youth',totdat$milk_who_sells_4[i])&!grepl('male_youth_or_child',totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_sells_4[i])
    }
    if (grepl(' female_youth',totdat$milk_who_sells_4[i])&!grepl('female_youth_or_child',totdat$milk_who_sells_4[i])) {
      totdat$milk_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_sells_4[i])
    }
  }      
  
  index<-totdat$milk_who_sells_4=='joint'
  totdat$milk_who_sells_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$milk_who_sells_5))
  {
    for (i in 1:length(totdat$milk_who_sells_5)) {
      totdat$milk_who_sells_5[i]<-trimws(totdat$milk_who_sells_5[i])
    }
    
    index<-totdat$milk_who_sells_5=='FALSE'
    totdat$milk_who_sells_5[index]<-'NA'
    
    for (i in 1:length(totdat$milk_who_sells_5)) {
      if (totdat$milk_who_sells_5[i]=='male'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_head'
      }
      if (totdat$milk_who_sells_5[i]=='male_child'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='female'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'female_head'
      }
      if (totdat$milk_who_sells_5[i]=='male female'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$milk_who_sells_5[i]=='female_youth'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'female_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='male_youth'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='child'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_head female_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_head male_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$milk_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$milk_who_sells_5<-gsub('woman_single','female_head',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('man_single','male_head',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('other_family_female','female_adult',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('other_family_male','male_adult',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('female ','female_head ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub('male ','male_head ',totdat$milk_who_sells_5)
    totdat$milk_who_sells_5<-gsub(' outside_person','',totdat$milk_who_sells_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$milk_who_sells_5)) {
      if (grepl(' male_youth',totdat$milk_who_sells_5[i])&!grepl('male_youth_or_child',totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_sells_5[i])
      }
      if (grepl(' female_youth',totdat$milk_who_sells_5[i])&!grepl('female_youth_or_child',totdat$milk_who_sells_5[i])) {
        totdat$milk_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_sells_5[i])
      }
    }      
    
    index<-totdat$milk_who_sells_5=='joint'
    totdat$milk_who_sells_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_control_eating_1))
{
  for (i in 1:length(totdat$milk_who_control_eating_1)) {
    totdat$milk_who_control_eating_1[i]<-trimws(totdat$milk_who_control_eating_1[i])
  }
  
  index<-totdat$milk_who_control_eating_1=='FALSE'
  totdat$milk_who_control_eating_1[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_control_eating_1)) {
    if (totdat$milk_who_control_eating_1[i]=='male'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_head'
    }
    if (totdat$milk_who_control_eating_1[i]=='male_child'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='female_child'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='female'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'female_head'
    }
    if (totdat$milk_who_control_eating_1[i]=='male female'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_head female_head'
    }
    if (totdat$milk_who_control_eating_1[i]=='female_youth'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='male_youth'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='child'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_control_eating_1<-gsub('woman_single','female_head',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('man_single','male_head',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub(' child',' male_youth_or_child',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('other_family_female','female_adult',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('other_family_male','male_adult',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('female ','female_head ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub('male ','male_head ',totdat$milk_who_control_eating_1)
  totdat$milk_who_control_eating_1<-gsub(' outside_person','',totdat$milk_who_control_eating_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_control_eating_1)) {
    if (grepl(' male_youth',totdat$milk_who_control_eating_1[i])&!grepl('male_youth_or_child',totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_control_eating_1[i])
    }
    if (grepl(' female_youth',totdat$milk_who_control_eating_1[i])&!grepl('female_youth_or_child',totdat$milk_who_control_eating_1[i])) {
      totdat$milk_who_control_eating_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_control_eating_1[i])
    }
  }      
  
  index<-totdat$milk_who_control_eating_1=='joint'
  totdat$milk_who_control_eating_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_control_eating_2))
{
  for (i in 1:length(totdat$milk_who_control_eating_2)) {
    totdat$milk_who_control_eating_2[i]<-trimws(totdat$milk_who_control_eating_2[i])
  }
  
  index<-totdat$milk_who_control_eating_2=='FALSE'
  totdat$milk_who_control_eating_2[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_control_eating_2)) {
    if (totdat$milk_who_control_eating_2[i]=='male'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_head'
    }
    if (totdat$milk_who_control_eating_2[i]=='male_child'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='female'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'female_head'
    }
    if (totdat$milk_who_control_eating_2[i]=='male female'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_head female_head'
    }
    if (totdat$milk_who_control_eating_2[i]=='female_youth'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='male_youth'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='child'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_control_eating_2<-gsub('woman_single','female_head',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('man_single','male_head',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub(' child',' male_youth_or_child',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('other_family_female','female_adult',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('other_family_male','male_adult',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('female ','female_head ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub('male ','male_head ',totdat$milk_who_control_eating_2)
  totdat$milk_who_control_eating_2<-gsub(' outside_person','',totdat$milk_who_control_eating_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_control_eating_2)) {
    if (grepl(' male_youth',totdat$milk_who_control_eating_2[i])&!grepl('male_youth_or_child',totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_control_eating_2[i])
    }
    if (grepl(' female_youth',totdat$milk_who_control_eating_2[i])&!grepl('female_youth_or_child',totdat$milk_who_control_eating_2[i])) {
      totdat$milk_who_control_eating_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_control_eating_2[i])
    }
  }      
  
  index<-totdat$milk_who_control_eating_2=='joint'
  totdat$milk_who_control_eating_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_control_eating_3))
{
  for (i in 1:length(totdat$milk_who_control_eating_3)) {
    totdat$milk_who_control_eating_3[i]<-trimws(totdat$milk_who_control_eating_3[i])
  }
  
  index<-totdat$milk_who_control_eating_3=='FALSE'
  totdat$milk_who_control_eating_3[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_control_eating_3)) {
    if (totdat$milk_who_control_eating_3[i]=='male'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_head'
    }
    if (totdat$milk_who_control_eating_3[i]=='male_child'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='female'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'female_head'
    }
    if (totdat$milk_who_control_eating_3[i]=='male female'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_head female_head'
    }
    if (totdat$milk_who_control_eating_3[i]=='female_youth'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='male_youth'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='child'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_control_eating_3<-gsub('woman_single','female_head',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('man_single','male_head',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub(' child',' male_youth_or_child',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('other_family_female','female_adult',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('other_family_male','male_adult',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('female ','female_head ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub('male ','male_head ',totdat$milk_who_control_eating_3)
  totdat$milk_who_control_eating_3<-gsub(' outside_person','',totdat$milk_who_control_eating_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_control_eating_3)) {
    if (grepl(' male_youth',totdat$milk_who_control_eating_3[i])&!grepl('male_youth_or_child',totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_control_eating_3[i])
    }
    if (grepl(' female_youth',totdat$milk_who_control_eating_3[i])&!grepl('female_youth_or_child',totdat$milk_who_control_eating_3[i])) {
      totdat$milk_who_control_eating_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_control_eating_3[i])
    }
  }      
  
  index<-totdat$milk_who_control_eating_3=='joint'
  totdat$milk_who_control_eating_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$milk_who_control_eating_4))
{
  for (i in 1:length(totdat$milk_who_control_eating_4)) {
    totdat$milk_who_control_eating_4[i]<-trimws(totdat$milk_who_control_eating_4[i])
  }
  
  index<-totdat$milk_who_control_eating_4=='FALSE'
  totdat$milk_who_control_eating_4[index]<-'NA'
  
  for (i in 1:length(totdat$milk_who_control_eating_4)) {
    if (totdat$milk_who_control_eating_4[i]=='male'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_head'
    }
    if (totdat$milk_who_control_eating_4[i]=='male_child'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='female'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'female_head'
    }
    if (totdat$milk_who_control_eating_4[i]=='male female'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_head female_head'
    }
    if (totdat$milk_who_control_eating_4[i]=='female_youth'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='male_youth'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='child'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$milk_who_control_eating_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$milk_who_control_eating_4<-gsub('woman_single','female_head',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('man_single','male_head',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub(' child',' male_youth_or_child',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('other_family_female','female_adult',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('other_family_male','male_adult',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('female ','female_head ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub('male ','male_head ',totdat$milk_who_control_eating_4)
  totdat$milk_who_control_eating_4<-gsub(' outside_person','',totdat$milk_who_control_eating_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$milk_who_control_eating_4)) {
    if (grepl(' male_youth',totdat$milk_who_control_eating_4[i])&!grepl('male_youth_or_child',totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_control_eating_4[i])
    }
    if (grepl(' female_youth',totdat$milk_who_control_eating_4[i])&!grepl('female_youth_or_child',totdat$milk_who_control_eating_4[i])) {
      totdat$milk_who_control_eating_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_control_eating_4[i])
    }
  }      
  
  index<-totdat$milk_who_control_eating_4=='joint'
  totdat$milk_who_control_eating_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$milk_who_control_eating_5))
  {
    for (i in 1:length(totdat$milk_who_control_eating_5)) {
      totdat$milk_who_control_eating_5[i]<-trimws(totdat$milk_who_control_eating_5[i])
    }
    
    index<-totdat$milk_who_control_eating_5=='FALSE'
    totdat$milk_who_control_eating_5[index]<-'NA'
    
    for (i in 1:length(totdat$milk_who_control_eating_5)) {
      if (totdat$milk_who_control_eating_5[i]=='male'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_head'
      }
      if (totdat$milk_who_control_eating_5[i]=='male_child'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='female'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'female_head'
      }
      if (totdat$milk_who_control_eating_5[i]=='male female'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_head female_head'
      }
      if (totdat$milk_who_control_eating_5[i]=='female_youth'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'female_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='male_youth'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='child'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_head female_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_head male_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$milk_who_control_eating_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$milk_who_control_eating_5<-gsub('woman_single','female_head',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('man_single','male_head',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub(' child',' male_youth_or_child',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('other_family_female','female_adult',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('other_family_male','male_adult',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('female_youth ','female_youth_or_child ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('male_youth ','male_youth_or_child ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('male_child ','male_youth_or_child ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub(' male_child',' male_youth_or_child',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('female_child ','female_youth_or_child ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub(' female_child',' female_youth_or_child',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('female ','female_head ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub('male ','male_head ',totdat$milk_who_control_eating_5)
    totdat$milk_who_control_eating_5<-gsub(' outside_person','',totdat$milk_who_control_eating_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$milk_who_control_eating_5)) {
      if (grepl(' male_youth',totdat$milk_who_control_eating_5[i])&!grepl('male_youth_or_child',totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$milk_who_control_eating_5[i])
      }
      if (grepl(' female_youth',totdat$milk_who_control_eating_5[i])&!grepl('female_youth_or_child',totdat$milk_who_control_eating_5[i])) {
        totdat$milk_who_control_eating_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$milk_who_control_eating_5[i])
      }
    }      
    
    index<-totdat$milk_who_control_eating_5=='joint'
    totdat$milk_who_control_eating_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_control_eating_1))
{
  for (i in 1:length(totdat$eggs_who_control_eating_1)) {
    totdat$eggs_who_control_eating_1[i]<-trimws(totdat$eggs_who_control_eating_1[i])
  }
  
  index<-totdat$eggs_who_control_eating_1=='FALSE'
  totdat$eggs_who_control_eating_1[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_control_eating_1)) {
    if (totdat$eggs_who_control_eating_1[i]=='male'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_head'
    }
    if (totdat$eggs_who_control_eating_1[i]=='male_child'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='female_child'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='female'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'female_head'
    }
    if (totdat$eggs_who_control_eating_1[i]=='male female'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_control_eating_1[i]=='female_youth'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='male_youth'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='child'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_control_eating_1<-gsub('woman_single','female_head',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('man_single','male_head',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub(' child',' male_youth_or_child',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('other_family_female','female_adult',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('other_family_male','male_adult',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('female ','female_head ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub('male ','male_head ',totdat$eggs_who_control_eating_1)
  totdat$eggs_who_control_eating_1<-gsub(' outside_person','',totdat$eggs_who_control_eating_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_control_eating_1)) {
    if (grepl(' male_youth',totdat$eggs_who_control_eating_1[i])&!grepl('male_youth_or_child',totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_control_eating_1[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_control_eating_1[i])&!grepl('female_youth_or_child',totdat$eggs_who_control_eating_1[i])) {
      totdat$eggs_who_control_eating_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_control_eating_1[i])
    }
  }      
  
  index<-totdat$eggs_who_control_eating_1=='joint'
  totdat$eggs_who_control_eating_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_control_eating_2))
{
  for (i in 1:length(totdat$eggs_who_control_eating_2)) {
    totdat$eggs_who_control_eating_2[i]<-trimws(totdat$eggs_who_control_eating_2[i])
  }
  
  index<-totdat$eggs_who_control_eating_2=='FALSE'
  totdat$eggs_who_control_eating_2[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_control_eating_2)) {
    if (totdat$eggs_who_control_eating_2[i]=='male'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_head'
    }
    if (totdat$eggs_who_control_eating_2[i]=='male_child'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='female'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'female_head'
    }
    if (totdat$eggs_who_control_eating_2[i]=='male female'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_control_eating_2[i]=='female_youth'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='male_youth'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='child'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_control_eating_2<-gsub('woman_single','female_head',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('man_single','male_head',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub(' child',' male_youth_or_child',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('other_family_female','female_adult',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('other_family_male','male_adult',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('female ','female_head ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub('male ','male_head ',totdat$eggs_who_control_eating_2)
  totdat$eggs_who_control_eating_2<-gsub(' outside_person','',totdat$eggs_who_control_eating_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_control_eating_2)) {
    if (grepl(' male_youth',totdat$eggs_who_control_eating_2[i])&!grepl('male_youth_or_child',totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_control_eating_2[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_control_eating_2[i])&!grepl('female_youth_or_child',totdat$eggs_who_control_eating_2[i])) {
      totdat$eggs_who_control_eating_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_control_eating_2[i])
    }
  }      
  
  index<-totdat$eggs_who_control_eating_2=='joint'
  totdat$eggs_who_control_eating_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_control_eating_3))
{
  for (i in 1:length(totdat$eggs_who_control_eating_3)) {
    totdat$eggs_who_control_eating_3[i]<-trimws(totdat$eggs_who_control_eating_3[i])
  }
  
  index<-totdat$eggs_who_control_eating_3=='FALSE'
  totdat$eggs_who_control_eating_3[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_control_eating_3)) {
    if (totdat$eggs_who_control_eating_3[i]=='male'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_head'
    }
    if (totdat$eggs_who_control_eating_3[i]=='male_child'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='female'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'female_head'
    }
    if (totdat$eggs_who_control_eating_3[i]=='male female'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_control_eating_3[i]=='female_youth'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='male_youth'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='child'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_control_eating_3<-gsub('woman_single','female_head',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('man_single','male_head',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub(' child',' male_youth_or_child',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('other_family_female','female_adult',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('other_family_male','male_adult',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('female ','female_head ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub('male ','male_head ',totdat$eggs_who_control_eating_3)
  totdat$eggs_who_control_eating_3<-gsub(' outside_person','',totdat$eggs_who_control_eating_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_control_eating_3)) {
    if (grepl(' male_youth',totdat$eggs_who_control_eating_3[i])&!grepl('male_youth_or_child',totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_control_eating_3[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_control_eating_3[i])&!grepl('female_youth_or_child',totdat$eggs_who_control_eating_3[i])) {
      totdat$eggs_who_control_eating_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_control_eating_3[i])
    }
  }      
  
  index<-totdat$eggs_who_control_eating_3=='joint'
  totdat$eggs_who_control_eating_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_control_eating_4))
{
  for (i in 1:length(totdat$eggs_who_control_eating_4)) {
    totdat$eggs_who_control_eating_4[i]<-trimws(totdat$eggs_who_control_eating_4[i])
  }
  
  index<-totdat$eggs_who_control_eating_4=='FALSE'
  totdat$eggs_who_control_eating_4[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_control_eating_4)) {
    if (totdat$eggs_who_control_eating_4[i]=='male'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_head'
    }
    if (totdat$eggs_who_control_eating_4[i]=='male_child'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='female'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'female_head'
    }
    if (totdat$eggs_who_control_eating_4[i]=='male female'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_control_eating_4[i]=='female_youth'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='male_youth'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='child'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_control_eating_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_control_eating_4<-gsub('woman_single','female_head',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('man_single','male_head',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub(' child',' male_youth_or_child',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('other_family_female','female_adult',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('other_family_male','male_adult',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('female ','female_head ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub('male ','male_head ',totdat$eggs_who_control_eating_4)
  totdat$eggs_who_control_eating_4<-gsub(' outside_person','',totdat$eggs_who_control_eating_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_control_eating_4)) {
    if (grepl(' male_youth',totdat$eggs_who_control_eating_4[i])&!grepl('male_youth_or_child',totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_control_eating_4[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_control_eating_4[i])&!grepl('female_youth_or_child',totdat$eggs_who_control_eating_4[i])) {
      totdat$eggs_who_control_eating_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_control_eating_4[i])
    }
  }      
  
  index<-totdat$eggs_who_control_eating_4=='joint'
  totdat$eggs_who_control_eating_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$eggs_who_control_eating_5))
  {
    for (i in 1:length(totdat$eggs_who_control_eating_5)) {
      totdat$eggs_who_control_eating_5[i]<-trimws(totdat$eggs_who_control_eating_5[i])
    }
    
    index<-totdat$eggs_who_control_eating_5=='FALSE'
    totdat$eggs_who_control_eating_5[index]<-'NA'
    
    for (i in 1:length(totdat$eggs_who_control_eating_5)) {
      if (totdat$eggs_who_control_eating_5[i]=='male'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_head'
      }
      if (totdat$eggs_who_control_eating_5[i]=='male_child'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='female'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'female_head'
      }
      if (totdat$eggs_who_control_eating_5[i]=='male female'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_head female_head'
      }
      if (totdat$eggs_who_control_eating_5[i]=='female_youth'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'female_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='male_youth'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='child'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_head female_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_head male_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$eggs_who_control_eating_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$eggs_who_control_eating_5<-gsub('woman_single','female_head',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('man_single','male_head',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub(' child',' male_youth_or_child',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('other_family_female','female_adult',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('other_family_male','male_adult',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('female ','female_head ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub('male ','male_head ',totdat$eggs_who_control_eating_5)
    totdat$eggs_who_control_eating_5<-gsub(' outside_person','',totdat$eggs_who_control_eating_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$eggs_who_control_eating_5)) {
      if (grepl(' male_youth',totdat$eggs_who_control_eating_5[i])&!grepl('male_youth_or_child',totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_control_eating_5[i])
      }
      if (grepl(' female_youth',totdat$eggs_who_control_eating_5[i])&!grepl('female_youth_or_child',totdat$eggs_who_control_eating_5[i])) {
        totdat$eggs_who_control_eating_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_control_eating_5[i])
      }
    }      
    
    index<-totdat$eggs_who_control_eating_5=='joint'
    totdat$eggs_who_control_eating_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_sells_1))
{
  for (i in 1:length(totdat$eggs_who_sells_1)) {
    totdat$eggs_who_sells_1[i]<-trimws(totdat$eggs_who_sells_1[i])
  }
  
  index<-totdat$eggs_who_sells_1=='FALSE'
  totdat$eggs_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_sells_1)) {
    if (totdat$eggs_who_sells_1[i]=='male'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_head'
    }
    if (totdat$eggs_who_sells_1[i]=='male_child'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='female_child'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='female'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'female_head'
    }
    if (totdat$eggs_who_sells_1[i]=='male female'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_sells_1[i]=='female_youth'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='male_youth'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='child'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_sells_1<-gsub('woman_single','female_head',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('man_single','male_head',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('other_family_female','female_adult',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('other_family_male','male_adult',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('female ','female_head ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub('male ','male_head ',totdat$eggs_who_sells_1)
  totdat$eggs_who_sells_1<-gsub(' outside_person','',totdat$eggs_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_sells_1)) {
    if (grepl(' male_youth',totdat$eggs_who_sells_1[i])&!grepl('male_youth_or_child',totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_sells_1[i])&!grepl('female_youth_or_child',totdat$eggs_who_sells_1[i])) {
      totdat$eggs_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_sells_1[i])
    }
  }      
  
  index<-totdat$eggs_who_sells_1=='joint'
  totdat$eggs_who_sells_1[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_sells_2))
{
  for (i in 1:length(totdat$eggs_who_sells_2)) {
    totdat$eggs_who_sells_2[i]<-trimws(totdat$eggs_who_sells_2[i])
  }
  
  index<-totdat$eggs_who_sells_2=='FALSE'
  totdat$eggs_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_sells_2)) {
    if (totdat$eggs_who_sells_2[i]=='male'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_head'
    }
    if (totdat$eggs_who_sells_2[i]=='male_child'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='female'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'female_head'
    }
    if (totdat$eggs_who_sells_2[i]=='male female'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_sells_2[i]=='female_youth'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='male_youth'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='child'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_sells_2<-gsub('woman_single','female_head',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('man_single','male_head',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('other_family_female','female_adult',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('other_family_male','male_adult',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('female ','female_head ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub('male ','male_head ',totdat$eggs_who_sells_2)
  totdat$eggs_who_sells_2<-gsub(' outside_person','',totdat$eggs_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_sells_2)) {
    if (grepl(' male_youth',totdat$eggs_who_sells_2[i])&!grepl('male_youth_or_child',totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_sells_2[i])&!grepl('female_youth_or_child',totdat$eggs_who_sells_2[i])) {
      totdat$eggs_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_sells_2[i])
    }
  }      
  
  index<-totdat$eggs_who_sells_2=='joint'
  totdat$eggs_who_sells_2[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_sells_3))
{
  for (i in 1:length(totdat$eggs_who_sells_3)) {
    totdat$eggs_who_sells_3[i]<-trimws(totdat$eggs_who_sells_3[i])
  }
  
  index<-totdat$eggs_who_sells_3=='FALSE'
  totdat$eggs_who_sells_3[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_sells_3)) {
    if (totdat$eggs_who_sells_3[i]=='male'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_head'
    }
    if (totdat$eggs_who_sells_3[i]=='male_child'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='female'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'female_head'
    }
    if (totdat$eggs_who_sells_3[i]=='male female'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_sells_3[i]=='female_youth'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='male_youth'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='child'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_sells_3<-gsub('woman_single','female_head',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('man_single','male_head',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('other_family_female','female_adult',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('other_family_male','male_adult',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('female ','female_head ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub('male ','male_head ',totdat$eggs_who_sells_3)
  totdat$eggs_who_sells_3<-gsub(' outside_person','',totdat$eggs_who_sells_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_sells_3)) {
    if (grepl(' male_youth',totdat$eggs_who_sells_3[i])&!grepl('male_youth_or_child',totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_sells_3[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_sells_3[i])&!grepl('female_youth_or_child',totdat$eggs_who_sells_3[i])) {
      totdat$eggs_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_sells_3[i])
    }
  }      
  
  index<-totdat$eggs_who_sells_3=='joint'
  totdat$eggs_who_sells_3[index]<-'male_head female_head'
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$eggs_who_sells_4))
{
  for (i in 1:length(totdat$eggs_who_sells_4)) {
    totdat$eggs_who_sells_4[i]<-trimws(totdat$eggs_who_sells_4[i])
  }
  
  index<-totdat$eggs_who_sells_4=='FALSE'
  totdat$eggs_who_sells_4[index]<-'NA'
  
  for (i in 1:length(totdat$eggs_who_sells_4)) {
    if (totdat$eggs_who_sells_4[i]=='male'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_head'
    }
    if (totdat$eggs_who_sells_4[i]=='male_child'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='female'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'female_head'
    }
    if (totdat$eggs_who_sells_4[i]=='male female'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_head female_head'
    }
    if (totdat$eggs_who_sells_4[i]=='female_youth'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'female_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='male_youth'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='child'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$eggs_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$eggs_who_sells_4<-gsub('woman_single','female_head',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('man_single','male_head',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('other_family_female','female_adult',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('other_family_male','male_adult',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('female ','female_head ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub('male ','male_head ',totdat$eggs_who_sells_4)
  totdat$eggs_who_sells_4<-gsub(' outside_person','',totdat$eggs_who_sells_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$eggs_who_sells_4)) {
    if (grepl(' male_youth',totdat$eggs_who_sells_4[i])&!grepl('male_youth_or_child',totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_sells_4[i])
    }
    if (grepl(' female_youth',totdat$eggs_who_sells_4[i])&!grepl('female_youth_or_child',totdat$eggs_who_sells_4[i])) {
      totdat$eggs_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_sells_4[i])
    }
  }      
  
  index<-totdat$eggs_who_sells_4=='joint'
  totdat$eggs_who_sells_4[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$eggs_who_sells_5))
  {
    for (i in 1:length(totdat$eggs_who_sells_5)) {
      totdat$eggs_who_sells_5[i]<-trimws(totdat$eggs_who_sells_5[i])
    }
    
    index<-totdat$eggs_who_sells_5=='FALSE'
    totdat$eggs_who_sells_5[index]<-'NA'
    
    for (i in 1:length(totdat$eggs_who_sells_5)) {
      if (totdat$eggs_who_sells_5[i]=='male'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_head'
      }
      if (totdat$eggs_who_sells_5[i]=='male_child'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='female'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'female_head'
      }
      if (totdat$eggs_who_sells_5[i]=='male female'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_head female_head'
      }
      if (totdat$eggs_who_sells_5[i]=='female_youth'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'female_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='male_youth'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='child'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_head female_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_head male_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'female_head male_youth_or_child'
      }
      if (totdat$eggs_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$eggs_who_sells_5<-gsub('woman_single','female_head',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('man_single','male_head',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('other_family_female','female_adult',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('other_family_male','male_adult',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('female ','female_head ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub('male ','male_head ',totdat$eggs_who_sells_5)
    totdat$eggs_who_sells_5<-gsub(' outside_person','',totdat$eggs_who_sells_5)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$eggs_who_sells_5)) {
      if (grepl(' male_youth',totdat$eggs_who_sells_5[i])&!grepl('male_youth_or_child',totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$eggs_who_sells_5[i])
      }
      if (grepl(' female_youth',totdat$eggs_who_sells_5[i])&!grepl('female_youth_or_child',totdat$eggs_who_sells_5[i])) {
        totdat$eggs_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$eggs_who_sells_5[i])
      }
    }      
    
    index<-totdat$eggs_who_sells_5=='joint'
    totdat$eggs_who_sells_5[index]<-'male_head female_head'
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$bees_who_sells_1))
{
  for (i in 1:length(totdat$bees_who_sells_1)) {
    totdat$bees_who_sells_1[i]<-trimws(totdat$bees_who_sells_1[i])
  }
  
  index<-totdat$bees_who_sells_1=='FALSE'
  totdat$bees_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_sells_1)) {
    if (totdat$bees_who_sells_1[i]=='male'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head'
    }
    if (totdat$bees_who_sells_1[i]=='male_child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='female_child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='female'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_head'
    }
    if (totdat$bees_who_sells_1[i]=='male female'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$bees_who_sells_1[i]=='female_youth'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='male_youth'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_sells_1<-gsub('woman_single','female_head',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('man_single','male_head',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('other_family_female','female_adult',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('other_family_male','male_adult',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female ','female_head ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male ','male_head ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' outside_person','',totdat$bees_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_sells_1)) {
    if (grepl(' male_youth',totdat$bees_who_sells_1[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$bees_who_sells_1[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_1[i])
    }
  }      
  
  index<-totdat$bees_who_sells_1=='joint'
  totdat$bees_who_sells_1[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  for (i in 1:length(totdat$bees_who_sells_2)) {
    totdat$bees_who_sells_2[i]<-trimws(totdat$bees_who_sells_2[i])
  }
  
  index<-totdat$bees_who_sells_2=='FALSE'
  totdat$bees_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_sells_2)) {
    if (totdat$bees_who_sells_2[i]=='male'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head'
    }
    if (totdat$bees_who_sells_2[i]=='male_child'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='female'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_head'
    }
    if (totdat$bees_who_sells_2[i]=='male female'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$bees_who_sells_2[i]=='female_youth'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='male_youth'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='child'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_sells_2<-gsub('woman_single','female_head',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('man_single','male_head',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('other_family_female','female_adult',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('other_family_male','male_adult',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female ','female_head ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male ','male_head ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' outside_person','',totdat$bees_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_sells_2)) {
    if (grepl(' male_youth',totdat$bees_who_sells_2[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$bees_who_sells_2[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_2[i])
    }
  }      
  
  index<-totdat$bees_who_sells_2=='joint'
  totdat$bees_who_sells_2[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_sells_3))
  {
    for (i in 1:length(totdat$bees_who_sells_3)) {
      totdat$bees_who_sells_3[i]<-trimws(totdat$bees_who_sells_3[i])
    }
    
    index<-totdat$bees_who_sells_3=='FALSE'
    totdat$bees_who_sells_3[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_sells_3)) {
      if (totdat$bees_who_sells_3[i]=='male'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head'
      }
      if (totdat$bees_who_sells_3[i]=='male_child'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='female'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_head'
      }
      if (totdat$bees_who_sells_3[i]=='male female'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_head'
      }
      if (totdat$bees_who_sells_3[i]=='female_youth'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='male_youth'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='child'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_sells_3<-gsub('woman_single','female_head',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('man_single','male_head',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('other_family_female','female_adult',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('other_family_male','male_adult',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female ','female_head ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male ','male_head ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' outside_person','',totdat$bees_who_sells_3)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_sells_3)) {
      if (grepl(' male_youth',totdat$bees_who_sells_3[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_3[i])
      }
      if (grepl(' female_youth',totdat$bees_who_sells_3[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_3[i])
      }
    }      
    
    index<-totdat$bees_who_sells_3=='joint'
    totdat$bees_who_sells_3[index]<-'male_head female_head'
  }
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_sells_4))
  {
    for (i in 1:length(totdat$bees_who_sells_4)) {
      totdat$bees_who_sells_4[i]<-trimws(totdat$bees_who_sells_4[i])
    }
    
    index<-totdat$bees_who_sells_4=='FALSE'
    totdat$bees_who_sells_4[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_sells_4)) {
      if (totdat$bees_who_sells_4[i]=='male'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head'
      }
      if (totdat$bees_who_sells_4[i]=='male_child'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='female'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_head'
      }
      if (totdat$bees_who_sells_4[i]=='male female'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_head'
      }
      if (totdat$bees_who_sells_4[i]=='female_youth'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='male_youth'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='child'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_sells_4<-gsub('woman_single','female_head',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('man_single','male_head',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('other_family_female','female_adult',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('other_family_male','male_adult',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female ','female_head ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male ','male_head ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' outside_person','',totdat$bees_who_sells_4)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_sells_4)) {
      if (grepl(' male_youth',totdat$bees_who_sells_4[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_4[i])
      }
      if (grepl(' female_youth',totdat$bees_who_sells_4[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_4[i])
      }
    }      
    
    index<-totdat$bees_who_sells_4=='joint'
    totdat$bees_who_sells_4[index]<-'male_head female_head'
    
    #------------------------------------------------------------------------
    #####harmonize gender info #####
    if(!is.null(totdat$bees_who_sells_5))
    {
      for (i in 1:length(totdat$bees_who_sells_5)) {
        totdat$bees_who_sells_5[i]<-trimws(totdat$bees_who_sells_5[i])
      }
      
      index<-totdat$bees_who_sells_5=='FALSE'
      totdat$bees_who_sells_5[index]<-'NA'
      
      for (i in 1:length(totdat$bees_who_sells_5)) {
        if (totdat$bees_who_sells_5[i]=='male'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head'
        }
        if (totdat$bees_who_sells_5[i]=='male_child'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='female'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_head'
        }
        if (totdat$bees_who_sells_5[i]=='male female'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_head'
        }
        if (totdat$bees_who_sells_5[i]=='female_youth'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='male_youth'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='child'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_head male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_head male_youth_or_child'
        }
      }
      #first female then male!!!
      totdat$bees_who_sells_5<-gsub('woman_single','female_head',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('man_single','male_head',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('other_family_female','female_adult',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('other_family_male','male_adult',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female ','female_head ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male ','male_head ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' outside_person','',totdat$bees_who_sells_5)
      
      #check for male_youth; if also male_youth_or_child is true do not change
      for (i in 1:length(totdat$bees_who_sells_5)) {
        if (grepl(' male_youth',totdat$bees_who_sells_5[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_5[i])
        }
        if (grepl(' female_youth',totdat$bees_who_sells_5[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_5[i])
        }
      }      
      
      index<-totdat$bees_who_sells_5=='joint'
      totdat$bees_who_sells_5[index]<-'male_head female_head'
      
    }
  }
}
#####harmonize gender info #####

if(!is.null(totdat$bees_who_sells_1))
{
  for (i in 1:length(totdat$bees_who_sells_1)) {
    totdat$bees_who_sells_1[i]<-trimws(totdat$bees_who_sells_1[i])
  }
  
  index<-totdat$bees_who_sells_1=='FALSE'
  totdat$bees_who_sells_1[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_sells_1)) {
    if (totdat$bees_who_sells_1[i]=='male'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head'
    }
    if (totdat$bees_who_sells_1[i]=='male_child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='female_child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='female'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_head'
    }
    if (totdat$bees_who_sells_1[i]=='male female'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_head'
    }
    if (totdat$bees_who_sells_1[i]=='female_youth'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='male_youth'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='child'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_sells_1<-gsub('woman_single','female_head',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('man_single','male_head',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('other_family_female','female_adult',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('other_family_male','male_adult',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('female ','female_head ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub('male ','male_head ',totdat$bees_who_sells_1)
  totdat$bees_who_sells_1<-gsub(' outside_person','',totdat$bees_who_sells_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_sells_1)) {
    if (grepl(' male_youth',totdat$bees_who_sells_1[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_1[i])
    }
    if (grepl(' female_youth',totdat$bees_who_sells_1[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_1[i])) {
      totdat$bees_who_sells_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_1[i])
    }
  }      
  
  index<-totdat$bees_who_sells_1=='joint'
  totdat$bees_who_sells_1[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  for (i in 1:length(totdat$bees_who_sells_2)) {
    totdat$bees_who_sells_2[i]<-trimws(totdat$bees_who_sells_2[i])
  }
  
  index<-totdat$bees_who_sells_2=='FALSE'
  totdat$bees_who_sells_2[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_sells_2)) {
    if (totdat$bees_who_sells_2[i]=='male'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head'
    }
    if (totdat$bees_who_sells_2[i]=='male_child'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='female'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_head'
    }
    if (totdat$bees_who_sells_2[i]=='male female'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_head'
    }
    if (totdat$bees_who_sells_2[i]=='female_youth'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='male_youth'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='child'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_sells_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_sells_2<-gsub('woman_single','female_head',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('man_single','male_head',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('other_family_female','female_adult',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('other_family_male','male_adult',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('female ','female_head ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub('male ','male_head ',totdat$bees_who_sells_2)
  totdat$bees_who_sells_2<-gsub(' outside_person','',totdat$bees_who_sells_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_sells_2)) {
    if (grepl(' male_youth',totdat$bees_who_sells_2[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_2[i])
    }
    if (grepl(' female_youth',totdat$bees_who_sells_2[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_2[i])) {
      totdat$bees_who_sells_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_2[i])
    }
  }      
  
  index<-totdat$bees_who_sells_2=='joint'
  totdat$bees_who_sells_2[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_sells_3))
  {
    for (i in 1:length(totdat$bees_who_sells_3)) {
      totdat$bees_who_sells_3[i]<-trimws(totdat$bees_who_sells_3[i])
    }
    
    index<-totdat$bees_who_sells_3=='FALSE'
    totdat$bees_who_sells_3[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_sells_3)) {
      if (totdat$bees_who_sells_3[i]=='male'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head'
      }
      if (totdat$bees_who_sells_3[i]=='male_child'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='female'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_head'
      }
      if (totdat$bees_who_sells_3[i]=='male female'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_head'
      }
      if (totdat$bees_who_sells_3[i]=='female_youth'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='male_youth'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='child'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_sells_3<-gsub('woman_single','female_head',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('man_single','male_head',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('other_family_female','female_adult',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('other_family_male','male_adult',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('female ','female_head ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub('male ','male_head ',totdat$bees_who_sells_3)
    totdat$bees_who_sells_3<-gsub(' outside_person','',totdat$bees_who_sells_3)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_sells_3)) {
      if (grepl(' male_youth',totdat$bees_who_sells_3[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_3[i])
      }
      if (grepl(' female_youth',totdat$bees_who_sells_3[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_3[i])) {
        totdat$bees_who_sells_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_3[i])
      }
    }      
    
    index<-totdat$bees_who_sells_3=='joint'
    totdat$bees_who_sells_3[index]<-'male_head female_head'
  }
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_sells_4))
  {
    for (i in 1:length(totdat$bees_who_sells_4)) {
      totdat$bees_who_sells_4[i]<-trimws(totdat$bees_who_sells_4[i])
    }
    
    index<-totdat$bees_who_sells_4=='FALSE'
    totdat$bees_who_sells_4[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_sells_4)) {
      if (totdat$bees_who_sells_4[i]=='male'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head'
      }
      if (totdat$bees_who_sells_4[i]=='male_child'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='female'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_head'
      }
      if (totdat$bees_who_sells_4[i]=='male female'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_head'
      }
      if (totdat$bees_who_sells_4[i]=='female_youth'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='male_youth'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='child'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_sells_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_sells_4<-gsub('woman_single','female_head',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('man_single','male_head',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('other_family_female','female_adult',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('other_family_male','male_adult',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('female ','female_head ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub('male ','male_head ',totdat$bees_who_sells_4)
    totdat$bees_who_sells_4<-gsub(' outside_person','',totdat$bees_who_sells_4)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_sells_4)) {
      if (grepl(' male_youth',totdat$bees_who_sells_4[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_4[i])
      }
      if (grepl(' female_youth',totdat$bees_who_sells_4[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_4[i])) {
        totdat$bees_who_sells_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_4[i])
      }
    }      
    
    index<-totdat$bees_who_sells_4=='joint'
    totdat$bees_who_sells_4[index]<-'male_head female_head'
    
    #------------------------------------------------------------------------
    #####harmonize gender info #####
    if(!is.null(totdat$bees_who_sells_5))
    {
      for (i in 1:length(totdat$bees_who_sells_5)) {
        totdat$bees_who_sells_5[i]<-trimws(totdat$bees_who_sells_5[i])
      }
      
      index<-totdat$bees_who_sells_5=='FALSE'
      totdat$bees_who_sells_5[index]<-'NA'
      
      for (i in 1:length(totdat$bees_who_sells_5)) {
        if (totdat$bees_who_sells_5[i]=='male'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head'
        }
        if (totdat$bees_who_sells_5[i]=='male_child'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='female'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_head'
        }
        if (totdat$bees_who_sells_5[i]=='male female'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_head'
        }
        if (totdat$bees_who_sells_5[i]=='female_youth'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='male_youth'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='child'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'female_head male_youth_or_child'
        }
        if (totdat$bees_who_sells_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-'male_head female_head male_youth_or_child'
        }
      }
      #first female then male!!!
      totdat$bees_who_sells_5<-gsub('woman_single','female_head',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('man_single','male_head',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' child',' male_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('other_family_female','female_adult',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('other_family_male','male_adult',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('female ','female_head ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub('male ','male_head ',totdat$bees_who_sells_5)
      totdat$bees_who_sells_5<-gsub(' outside_person','',totdat$bees_who_sells_5)
      
      #check for male_youth; if also male_youth_or_child is true do not change
      for (i in 1:length(totdat$bees_who_sells_5)) {
        if (grepl(' male_youth',totdat$bees_who_sells_5[i])&!grepl('male_youth_or_child',totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_sells_5[i])
        }
        if (grepl(' female_youth',totdat$bees_who_sells_5[i])&!grepl('female_youth_or_child',totdat$bees_who_sells_5[i])) {
          totdat$bees_who_sells_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_sells_5[i])
        }
      }      
      
      index<-totdat$bees_who_sells_5=='joint'
      totdat$bees_who_sells_5[index]<-'male_head female_head'
      
    }
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$bees_who_control_eating_1))
{
  for (i in 1:length(totdat$bees_who_control_eating_1)) {
    totdat$bees_who_control_eating_1[i]<-trimws(totdat$bees_who_control_eating_1[i])
  }
  
  index<-totdat$bees_who_control_eating_1=='FALSE'
  totdat$bees_who_control_eating_1[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_control_eating_1)) {
    if (totdat$bees_who_control_eating_1[i]=='male'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_head'
    }
    if (totdat$bees_who_control_eating_1[i]=='male_child'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='female_child'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='female'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'female_head'
    }
    if (totdat$bees_who_control_eating_1[i]=='male female'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_head female_head'
    }
    if (totdat$bees_who_control_eating_1[i]=='female_youth'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='male_youth'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='child'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_control_eating_1<-gsub('woman_single','female_head',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('man_single','male_head',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub(' child',' male_youth_or_child',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('other_family_female','female_adult',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('other_family_male','male_adult',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('female ','female_head ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub('male ','male_head ',totdat$bees_who_control_eating_1)
  totdat$bees_who_control_eating_1<-gsub(' outside_person','',totdat$bees_who_control_eating_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_control_eating_1)) {
    if (grepl(' male_youth',totdat$bees_who_control_eating_1[i])&!grepl('male_youth_or_child',totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_control_eating_1[i])
    }
    if (grepl(' female_youth',totdat$bees_who_control_eating_1[i])&!grepl('female_youth_or_child',totdat$bees_who_control_eating_1[i])) {
      totdat$bees_who_control_eating_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_control_eating_1[i])
    }
  }      
  
  index<-totdat$bees_who_control_eating_1=='joint'
  totdat$bees_who_control_eating_1[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  for (i in 1:length(totdat$bees_who_control_eating_2)) {
    totdat$bees_who_control_eating_2[i]<-trimws(totdat$bees_who_control_eating_2[i])
  }
  
  index<-totdat$bees_who_control_eating_2=='FALSE'
  totdat$bees_who_control_eating_2[index]<-'NA'
  
  for (i in 1:length(totdat$bees_who_control_eating_2)) {
    if (totdat$bees_who_control_eating_2[i]=='male'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_head'
    }
    if (totdat$bees_who_control_eating_2[i]=='male_child'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='female'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'female_head'
    }
    if (totdat$bees_who_control_eating_2[i]=='male female'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_head female_head'
    }
    if (totdat$bees_who_control_eating_2[i]=='female_youth'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'female_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='male_youth'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='child'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$bees_who_control_eating_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$bees_who_control_eating_2<-gsub('woman_single','female_head',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('man_single','male_head',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub(' child',' male_youth_or_child',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('other_family_female','female_adult',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('other_family_male','male_adult',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('female ','female_head ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub('male ','male_head ',totdat$bees_who_control_eating_2)
  totdat$bees_who_control_eating_2<-gsub(' outside_person','',totdat$bees_who_control_eating_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$bees_who_control_eating_2)) {
    if (grepl(' male_youth',totdat$bees_who_control_eating_2[i])&!grepl('male_youth_or_child',totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_control_eating_2[i])
    }
    if (grepl(' female_youth',totdat$bees_who_control_eating_2[i])&!grepl('female_youth_or_child',totdat$bees_who_control_eating_2[i])) {
      totdat$bees_who_control_eating_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_control_eating_2[i])
    }
  }      
  
  index<-totdat$bees_who_control_eating_2=='joint'
  totdat$bees_who_control_eating_2[index]<-'male_head female_head'
  
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_control_eating_3))
  {
    for (i in 1:length(totdat$bees_who_control_eating_3)) {
      totdat$bees_who_control_eating_3[i]<-trimws(totdat$bees_who_control_eating_3[i])
    }
    
    index<-totdat$bees_who_control_eating_3=='FALSE'
    totdat$bees_who_control_eating_3[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_control_eating_3)) {
      if (totdat$bees_who_control_eating_3[i]=='male'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_head'
      }
      if (totdat$bees_who_control_eating_3[i]=='male_child'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='female'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'female_head'
      }
      if (totdat$bees_who_control_eating_3[i]=='male female'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_head female_head'
      }
      if (totdat$bees_who_control_eating_3[i]=='female_youth'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='male_youth'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='child'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_control_eating_3<-gsub('woman_single','female_head',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('man_single','male_head',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub(' child',' male_youth_or_child',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('other_family_female','female_adult',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('other_family_male','male_adult',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('female ','female_head ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub('male ','male_head ',totdat$bees_who_control_eating_3)
    totdat$bees_who_control_eating_3<-gsub(' outside_person','',totdat$bees_who_control_eating_3)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_control_eating_3)) {
      if (grepl(' male_youth',totdat$bees_who_control_eating_3[i])&!grepl('male_youth_or_child',totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_control_eating_3[i])
      }
      if (grepl(' female_youth',totdat$bees_who_control_eating_3[i])&!grepl('female_youth_or_child',totdat$bees_who_control_eating_3[i])) {
        totdat$bees_who_control_eating_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_control_eating_3[i])
      }
    }      
    
    index<-totdat$bees_who_control_eating_3=='joint'
    totdat$bees_who_control_eating_3[index]<-'male_head female_head'
  }
  #------------------------------------------------------------------------
  #####harmonize gender info #####
  if(!is.null(totdat$bees_who_control_eating_4))
  {
    for (i in 1:length(totdat$bees_who_control_eating_4)) {
      totdat$bees_who_control_eating_4[i]<-trimws(totdat$bees_who_control_eating_4[i])
    }
    
    index<-totdat$bees_who_control_eating_4=='FALSE'
    totdat$bees_who_control_eating_4[index]<-'NA'
    
    for (i in 1:length(totdat$bees_who_control_eating_4)) {
      if (totdat$bees_who_control_eating_4[i]=='male'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_head'
      }
      if (totdat$bees_who_control_eating_4[i]=='male_child'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='female'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'female_head'
      }
      if (totdat$bees_who_control_eating_4[i]=='male female'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_head female_head'
      }
      if (totdat$bees_who_control_eating_4[i]=='female_youth'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'female_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='male_youth'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='child'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_head female_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_head male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'female_head male_youth_or_child'
      }
      if (totdat$bees_who_control_eating_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-'male_head female_head male_youth_or_child'
      }
    }
    #first female then male!!!
    totdat$bees_who_control_eating_4<-gsub('woman_single','female_head',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('man_single','male_head',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub(' child',' male_youth_or_child',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('other_family_female','female_adult',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('other_family_male','male_adult',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('female ','female_head ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub('male ','male_head ',totdat$bees_who_control_eating_4)
    totdat$bees_who_control_eating_4<-gsub(' outside_person','',totdat$bees_who_control_eating_4)
    
    #check for male_youth; if also male_youth_or_child is true do not change
    for (i in 1:length(totdat$bees_who_control_eating_4)) {
      if (grepl(' male_youth',totdat$bees_who_control_eating_4[i])&!grepl('male_youth_or_child',totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_control_eating_4[i])
      }
      if (grepl(' female_youth',totdat$bees_who_control_eating_4[i])&!grepl('female_youth_or_child',totdat$bees_who_control_eating_4[i])) {
        totdat$bees_who_control_eating_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_control_eating_4[i])
      }
    }      
    
    index<-totdat$bees_who_control_eating_4=='joint'
    totdat$bees_who_control_eating_4[index]<-'male_head female_head'
    
    #------------------------------------------------------------------------
    #####harmonize gender info #####
    if(!is.null(totdat$bees_who_control_eating_5))
    {
      for (i in 1:length(totdat$bees_who_control_eating_5)) {
        totdat$bees_who_control_eating_5[i]<-trimws(totdat$bees_who_control_eating_5[i])
      }
      
      index<-totdat$bees_who_control_eating_5=='FALSE'
      totdat$bees_who_control_eating_5[index]<-'NA'
      
      for (i in 1:length(totdat$bees_who_control_eating_5)) {
        if (totdat$bees_who_control_eating_5[i]=='male'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_head'
        }
        if (totdat$bees_who_control_eating_5[i]=='male_child'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='female'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'female_head'
        }
        if (totdat$bees_who_control_eating_5[i]=='male female'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_head female_head'
        }
        if (totdat$bees_who_control_eating_5[i]=='female_youth'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'female_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='male_youth'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='child'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='c(\"male\", \"female\")'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_head female_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='c(\"male\", \"child\")'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_head male_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='c(\"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'female_head male_youth_or_child'
        }
        if (totdat$bees_who_control_eating_5[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-'male_head female_head male_youth_or_child'
        }
      }
      #first female then male!!!
      totdat$bees_who_control_eating_5<-gsub('woman_single','female_head',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('man_single','male_head',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub(' child',' male_youth_or_child',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('other_family_female','female_adult',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('other_family_male','male_adult',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('female_youth ','female_youth_or_child ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('male_youth ','male_youth_or_child ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('male_child ','male_youth_or_child ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub(' male_child',' male_youth_or_child',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('female_child ','female_youth_or_child ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub(' female_child',' female_youth_or_child',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('female ','female_head ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub('male ','male_head ',totdat$bees_who_control_eating_5)
      totdat$bees_who_control_eating_5<-gsub(' outside_person','',totdat$bees_who_control_eating_5)
      
      #check for male_youth; if also male_youth_or_child is true do not change
      for (i in 1:length(totdat$bees_who_control_eating_5)) {
        if (grepl(' male_youth',totdat$bees_who_control_eating_5[i])&!grepl('male_youth_or_child',totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-gsub(' male_youth',' male_youth_or_child',totdat$bees_who_control_eating_5[i])
        }
        if (grepl(' female_youth',totdat$bees_who_control_eating_5[i])&!grepl('female_youth_or_child',totdat$bees_who_control_eating_5[i])) {
          totdat$bees_who_control_eating_5[i]<-gsub(' female_youth',' female_youth_or_child',totdat$bees_who_control_eating_5[i])
        }
      }      
      
      index<-totdat$bees_who_control_eating_5=='joint'
      totdat$bees_who_control_eating_5[index]<-'male_head female_head'
    }
  }
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$offfarm_who_control_revenue_1))
{
  for (i in 1:length(totdat$offfarm_who_control_revenue_1)) {
    totdat$offfarm_who_control_revenue_1[i]<-trimws(totdat$offfarm_who_control_revenue_1[i])
  }
  
  index<-totdat$offfarm_who_control_revenue_1=='FALSE'
  totdat$offfarm_who_control_revenue_1[index]<-'NA'
  
  for (i in 1:length(totdat$offfarm_who_control_revenue_1)) {
    if (totdat$offfarm_who_control_revenue_1[i]=='male'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='male_child'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='female'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'female_head'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='male female'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head female_head'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='female_youth'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='male_youth'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='child'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"male\", \"female\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"male\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='youth'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"male\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_1[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$offfarm_who_control_revenue_1<-gsub('woman_single','female_head',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('man_single','male_head',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub(' child',' male_youth_or_child',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('other_family_female','female_adult',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('other_family_male','male_adult',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('female_youth ','female_youth_or_child ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('male_youth ','male_youth_or_child ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('male_child ','male_youth_or_child ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub(' male_child',' male_youth_or_child',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('female_child ','female_youth_or_child ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub(' female_child',' female_youth_or_child',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('female ','female_head ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub('male ','male_head ',totdat$offfarm_who_control_revenue_1)
  totdat$offfarm_who_control_revenue_1<-gsub(' outside_person','',totdat$offfarm_who_control_revenue_1)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$offfarm_who_control_revenue_1)) {
    if (grepl(' male_youth',totdat$offfarm_who_control_revenue_1[i])&!grepl('male_youth_or_child',totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-gsub(' male_youth',' male_youth_or_child',totdat$offfarm_who_control_revenue_1[i])
    }
    if (grepl(' female_youth',totdat$offfarm_who_control_revenue_1[i])&!grepl('female_youth_or_child',totdat$offfarm_who_control_revenue_1[i])) {
      totdat$offfarm_who_control_revenue_1[i]<-gsub(' female_youth',' female_youth_or_child',totdat$offfarm_who_control_revenue_1[i])
    }
  }      
  
  index<-totdat$offfarm_who_control_revenue_1=='joint'
  totdat$offfarm_who_control_revenue_1[index]<-'male_head female_head'
}

#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$offfarm_who_control_revenue_2))
{
  for (i in 1:length(totdat$offfarm_who_control_revenue_2)) {
    totdat$offfarm_who_control_revenue_2[i]<-trimws(totdat$offfarm_who_control_revenue_2[i])
  }
  
  index<-totdat$offfarm_who_control_revenue_2=='FALSE'
  totdat$offfarm_who_control_revenue_2[index]<-'NA'
  
  for (i in 1:length(totdat$offfarm_who_control_revenue_2)) {
    if (totdat$offfarm_who_control_revenue_2[i]=='male'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='male_child'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='female'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'female_head'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='male female'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head female_head'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='female_youth'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='male_youth'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='child'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"male\", \"female\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"male\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='youth'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"male\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_2[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$offfarm_who_control_revenue_2<-gsub('woman_single','female_head',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('man_single','male_head',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub(' child',' male_youth_or_child',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('other_family_female','female_adult',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('other_family_male','male_adult',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('female_youth ','female_youth_or_child ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('male_youth ','male_youth_or_child ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('male_child ','male_youth_or_child ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub(' male_child',' male_youth_or_child',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('female_child ','female_youth_or_child ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub(' female_child',' female_youth_or_child',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('female ','female_head ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub('male ','male_head ',totdat$offfarm_who_control_revenue_2)
  totdat$offfarm_who_control_revenue_2<-gsub(' outside_person','',totdat$offfarm_who_control_revenue_2)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$offfarm_who_control_revenue_2)) {
    if (grepl(' male_youth',totdat$offfarm_who_control_revenue_2[i])&!grepl('male_youth_or_child',totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-gsub(' male_youth',' male_youth_or_child',totdat$offfarm_who_control_revenue_2[i])
    }
    if (grepl(' female_youth',totdat$offfarm_who_control_revenue_2[i])&!grepl('female_youth_or_child',totdat$offfarm_who_control_revenue_2[i])) {
      totdat$offfarm_who_control_revenue_2[i]<-gsub(' female_youth',' female_youth_or_child',totdat$offfarm_who_control_revenue_2[i])
    }
  }      
  
  index<-totdat$offfarm_who_control_revenue_2=='joint'
  totdat$offfarm_who_control_revenue_2[index]<-'male_head female_head'
  
}
#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$offfarm_who_control_revenue_3))
{
  for (i in 1:length(totdat$offfarm_who_control_revenue_3)) {
    totdat$offfarm_who_control_revenue_3[i]<-trimws(totdat$offfarm_who_control_revenue_3[i])
  }
  
  index<-totdat$offfarm_who_control_revenue_3=='FALSE'
  totdat$offfarm_who_control_revenue_3[index]<-'NA'
  
  for (i in 1:length(totdat$offfarm_who_control_revenue_3)) {
    if (totdat$offfarm_who_control_revenue_3[i]=='male'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='male_child'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='female'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'female_head'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='male female'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head female_head'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='female_youth'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='male_youth'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='child'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"male\", \"female\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"male\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='youth'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"male\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_3[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$offfarm_who_control_revenue_3<-gsub('woman_single','female_head',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('man_single','male_head',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub(' child',' male_youth_or_child',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('other_family_female','female_adult',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('other_family_male','male_adult',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('female_youth ','female_youth_or_child ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('male_youth ','male_youth_or_child ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('male_child ','male_youth_or_child ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub(' male_child',' male_youth_or_child',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('female_child ','female_youth_or_child ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub(' female_child',' female_youth_or_child',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('female ','female_head ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub('male ','male_head ',totdat$offfarm_who_control_revenue_3)
  totdat$offfarm_who_control_revenue_3<-gsub(' outside_person','',totdat$offfarm_who_control_revenue_3)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$offfarm_who_control_revenue_3)) {
    if (grepl(' male_youth',totdat$offfarm_who_control_revenue_3[i])&!grepl('male_youth_or_child',totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-gsub(' male_youth',' male_youth_or_child',totdat$offfarm_who_control_revenue_3[i])
    }
    if (grepl(' female_youth',totdat$offfarm_who_control_revenue_3[i])&!grepl('female_youth_or_child',totdat$offfarm_who_control_revenue_3[i])) {
      totdat$offfarm_who_control_revenue_3[i]<-gsub(' female_youth',' female_youth_or_child',totdat$offfarm_who_control_revenue_3[i])
    }
  }      
  
  index<-totdat$offfarm_who_control_revenue_3=='joint'
  totdat$offfarm_who_control_revenue_3[index]<-'male_head female_head'
  
}

#------------------------------------------------------------------------
#####harmonize gender info #####
if(!is.null(totdat$offfarm_who_control_revenue_4))
{
  for (i in 1:length(totdat$offfarm_who_control_revenue_4)) {
    totdat$offfarm_who_control_revenue_4[i]<-trimws(totdat$offfarm_who_control_revenue_4[i])
  }
  
  index<-totdat$offfarm_who_control_revenue_4=='FALSE'
  totdat$offfarm_who_control_revenue_4[index]<-'NA'
  
  for (i in 1:length(totdat$offfarm_who_control_revenue_4)) {
    if (totdat$offfarm_who_control_revenue_4[i]=='male'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='male_child'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='female'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'female_head'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='male female'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head female_head'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='female_youth'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='male_youth'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='child'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"male\", \"female\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head female_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"male\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='youth'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"male\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"male\", \"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"female\", \"youth\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'female_head male_youth_or_child'
    }
    if (totdat$offfarm_who_control_revenue_4[i]=='c(\"male\", \"female\", \"child\")'&!is.na(totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-'male_head female_head male_youth_or_child'
    }
  }
  #first female then male!!!
  totdat$offfarm_who_control_revenue_4<-gsub('woman_single','female_head',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('man_single','male_head',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub(' child',' male_youth_or_child',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('other_family_female','female_adult',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('other_family_male','male_adult',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('female_youth ','female_youth_or_child ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('male_youth ','male_youth_or_child ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('male_child ','male_youth_or_child ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub(' male_child',' male_youth_or_child',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('female_child ','female_youth_or_child ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub(' female_child',' female_youth_or_child',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('female ','female_head ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub('male ','male_head ',totdat$offfarm_who_control_revenue_4)
  totdat$offfarm_who_control_revenue_4<-gsub(' outside_person','',totdat$offfarm_who_control_revenue_4)
  
  #check for male_youth; if also male_youth_or_child is true do not change
  for (i in 1:length(totdat$offfarm_who_control_revenue_4)) {
    if (grepl(' male_youth',totdat$offfarm_who_control_revenue_4[i])&!grepl('male_youth_or_child',totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-gsub(' male_youth',' male_youth_or_child',totdat$offfarm_who_control_revenue_4[i])
    }
    if (grepl(' female_youth',totdat$offfarm_who_control_revenue_4[i])&!grepl('female_youth_or_child',totdat$offfarm_who_control_revenue_4[i])) {
      totdat$offfarm_who_control_revenue_4[i]<-gsub(' female_youth',' female_youth_or_child',totdat$offfarm_who_control_revenue_4[i])
    }
  }      
  
  index<-totdat$offfarm_who_control_revenue_4=='joint'
  totdat$offfarm_who_control_revenue_4[index]<-'male_head female_head'
}

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#### nutrition ####
if (HDDS_type=='Ten_Groups')
{
totdat$GrainsRootsTubers_source_bad<-gsub('gift_exchange','free',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('onfarm','on-farm',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('not_eaten','',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('gathered','free',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('off-farm','bought',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('free free','free',totdat$GrainsRootsTubers_source_bad)
totdat$GrainsRootsTubers_source_bad<-gsub('both','on-farm bought',totdat$GrainsRootsTubers_source_bad)
for (i in 1:length(totdat$GrainsRootsTubers_source_bad)) {
  totdat$GrainsRootsTubers_source_bad[i]<-trimws(totdat$GrainsRootsTubers_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$GrainsRootsTubers_source_good))
{
totdat$GrainsRootsTubers_source_good<-gsub('gift_exchange','free',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('onfarm','on-farm',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('not_eaten','',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('gathered','free',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('off-farm','bought',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('free free','free',totdat$GrainsRootsTubers_source_good)
totdat$GrainsRootsTubers_source_good<-gsub('both','on-farm bought',totdat$GrainsRootsTubers_source_good)
for (i in 1:length(totdat$GrainsRootsTubers_source_good)) {
  totdat$GrainsRootsTubers_source_good[i]<-trimws(totdat$GrainsRootsTubers_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$GrainsRootsTubers_source_last_month<-gsub('gift_exchange','free',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('onfarm','on-farm',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('not_eaten','',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('gathered','free',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('off-farm','bought',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('free free','free',totdat$GrainsRootsTubers_source_last_month)
totdat$GrainsRootsTubers_source_last_month<-gsub('both','on-farm bought',totdat$GrainsRootsTubers_source_last_month)
for (i in 1:length(totdat$GrainsRootsTubers_source_last_month)) {
  totdat$GrainsRootsTubers_source_last_month[i]<-trimws(totdat$GrainsRootsTubers_source_last_month[i])
}


#------------------------------------------------------------------------
#### nutrition ####
totdat$Legumes_source_bad<-gsub('gift_exchange','free',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('onfarm','on-farm',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('not_eaten','',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('gathered','free',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('off-farm','bought',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('free free','free',totdat$Legumes_source_bad)
totdat$Legumes_source_bad<-gsub('both','on-farm bought',totdat$Legumes_source_bad)
for (i in 1:length(totdat$Legumes_source_bad)) {
  totdat$Legumes_source_bad[i]<-trimws(totdat$Legumes_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Legumes_source_good))
{
totdat$Legumes_source_good<-gsub('gift_exchange','free',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('onfarm','on-farm',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('not_eaten','',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('gathered','free',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('off-farm','bought',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('free free','free',totdat$Legumes_source_good)
totdat$Legumes_source_good<-gsub('both','on-farm bought',totdat$Legumes_source_good)
for (i in 1:length(totdat$Legumes_source_good)) {
  totdat$Legumes_source_good[i]<-trimws(totdat$Legumes_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####

totdat$Legumes_source_last_month<-gsub('gift_exchange','free',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('onfarm','on-farm',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('not_eaten','',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('gathered','free',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('off-farm','bought',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('free free','free',totdat$Legumes_source_last_month)
totdat$Legumes_source_last_month<-gsub('both','on-farm bought',totdat$Legumes_source_last_month)
for (i in 1:length(totdat$Legumes_source_last_month)) {
  totdat$Legumes_source_last_month[i]<-trimws(totdat$Legumes_source_last_month[i])
}


#------------------------------------------------------------------------
#### nutrition ####
totdat$Nuts_Seeds_source_bad<-gsub('gift_exchange','free',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('onfarm','on-farm',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('not_eaten','',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('gathered','free',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('off-farm','bought',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('free free','free',totdat$Nuts_Seeds_source_bad)
totdat$Nuts_Seeds_source_bad<-gsub('both','on-farm bought',totdat$Nuts_Seeds_source_bad)
for (i in 1:length(totdat$Nuts_Seeds_source_bad)) {
  totdat$Nuts_Seeds_source_bad[i]<-trimws(totdat$Nuts_Seeds_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Nuts_Seeds_source_good))
{
totdat$Nuts_Seeds_source_good<-gsub('gift_exchange','free',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('onfarm','on-farm',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('not_eaten','',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('gathered','free',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('off-farm','bought',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('free free','free',totdat$Nuts_Seeds_source_good)
totdat$Nuts_Seeds_source_good<-gsub('both','on-farm bought',totdat$Nuts_Seeds_source_good)
for (i in 1:length(totdat$Nuts_Seeds_source_good)) {
  totdat$Nuts_Seeds_source_good[i]<-trimws(totdat$Nuts_Seeds_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Nuts_Seeds_source_last_month<-gsub('gift_exchange','free',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('onfarm','on-farm',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('not_eaten','',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('gathered','free',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('off-farm','bought',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('free free','free',totdat$Nuts_Seeds_source_last_month)
totdat$Nuts_Seeds_source_last_month<-gsub('both','on-farm bought',totdat$Nuts_Seeds_source_last_month)
for (i in 1:length(totdat$Nuts_Seeds_source_last_month)) {
  totdat$Nuts_Seeds_source_last_month[i]<-trimws(totdat$Nuts_Seeds_source_last_month[i])
}



#------------------------------------------------------------------------
#### nutrition ####
totdat$Veg_Leafy_source_bad<-gsub('gift_exchange','free',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('onfarm','on-farm',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('not_eaten','',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('gathered','free',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('off-farm','bought',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('free free','free',totdat$Veg_Leafy_source_bad)
totdat$Veg_Leafy_source_bad<-gsub('both','on-farm bought',totdat$Veg_Leafy_source_bad)
for (i in 1:length(totdat$Veg_Leafy_source_bad)) {
  totdat$Veg_Leafy_source_bad[i]<-trimws(totdat$Veg_Leafy_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Veg_Leafy_source_good))
{
totdat$Veg_Leafy_source_good<-gsub('gift_exchange','free',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('onfarm','on-farm',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('not_eaten','',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('gathered','free',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('off-farm','bought',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('free free','free',totdat$Veg_Leafy_source_good)
totdat$Veg_Leafy_source_good<-gsub('both','on-farm bought',totdat$Veg_Leafy_source_good)
for (i in 1:length(totdat$Veg_Leafy_source_good)) {
  totdat$Veg_Leafy_source_good[i]<-trimws(totdat$Veg_Leafy_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Veg_Leafy_source_last_month<-gsub('gift_exchange','free',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('onfarm','on-farm',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('not_eaten','',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('gathered','free',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('off-farm','bought',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('free free','free',totdat$Veg_Leafy_source_last_month)
totdat$Veg_Leafy_source_last_month<-gsub('both','on-farm bought',totdat$Veg_Leafy_source_last_month)
for (i in 1:length(totdat$Veg_Leafy_source_last_month)) {
  totdat$Veg_Leafy_source_last_month[i]<-trimws(totdat$Veg_Leafy_source_last_month[i])
}


#------------------------------------------------------------------------
#### nutrition ####
totdat$VitA_Veg_Fruit_source_bad<-gsub('gift_exchange','free',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('onfarm','on-farm',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('not_eaten','',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('gathered','free',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('off-farm','bought',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('free free','free',totdat$VitA_Veg_Fruit_source_bad)
totdat$VitA_Veg_Fruit_source_bad<-gsub('both','on-farm bought',totdat$VitA_Veg_Fruit_source_bad)
for (i in 1:length(totdat$VitA_Veg_Fruit_source_bad)) {
  totdat$VitA_Veg_Fruit_source_bad[i]<-trimws(totdat$VitA_Veg_Fruit_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$VitA_Veg_Fruit_source_good))
{
totdat$VitA_Veg_Fruit_source_good<-gsub('gift_exchange','free',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('onfarm','on-farm',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('not_eaten','',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('gathered','free',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('off-farm','bought',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('free free','free',totdat$VitA_Veg_Fruit_source_good)
totdat$VitA_Veg_Fruit_source_good<-gsub('both','on-farm bought',totdat$VitA_Veg_Fruit_source_good)
for (i in 1:length(totdat$VitA_Veg_Fruit_source_good)) {
  totdat$VitA_Veg_Fruit_source_good[i]<-trimws(totdat$VitA_Veg_Fruit_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$VitA_Veg_Fruit_source_last_month<-gsub('gift_exchange','free',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('onfarm','on-farm',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('not_eaten','',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('gathered','free',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('off-farm','bought',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('free free','free',totdat$VitA_Veg_Fruit_source_last_month)
totdat$VitA_Veg_Fruit_source_last_month<-gsub('both','on-farm bought',totdat$VitA_Veg_Fruit_source_last_month)
for (i in 1:length(totdat$VitA_Veg_Fruit_source_last_month)) {
  totdat$VitA_Veg_Fruit_source_last_month[i]<-trimws(totdat$VitA_Veg_Fruit_source_last_month[i])
}


#------------------------------------------------------------------------
#### nutrition ####
totdat$Vegetables_source_bad<-gsub('gift_exchange','free',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('onfarm','on-farm',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('not_eaten','',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('gathered','free',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('off-farm','bought',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('free free','free',totdat$Vegetables_source_bad)
totdat$Vegetables_source_bad<-gsub('both','on-farm bought',totdat$Vegetables_source_bad)
for (i in 1:length(totdat$Vegetables_source_bad)) {
  totdat$Vegetables_source_bad[i]<-trimws(totdat$Vegetables_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Vegetables_source_good))
{
totdat$Vegetables_source_good<-gsub('gift_exchange','free',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('onfarm','on-farm',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('not_eaten','',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('gathered','free',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('off-farm','bought',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('free free','free',totdat$Vegetables_source_good)
totdat$Vegetables_source_good<-gsub('both','on-farm bought',totdat$Vegetables_source_good)
for (i in 1:length(totdat$Vegetables_source_good)) {
  totdat$Vegetables_source_good[i]<-trimws(totdat$Vegetables_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Vegetables_source_last_month<-gsub('gift_exchange','free',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('onfarm','on-farm',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('not_eaten','',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('gathered','free',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('off-farm','bought',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('free free','free',totdat$Vegetables_source_last_month)
totdat$Vegetables_source_last_month<-gsub('both','on-farm bought',totdat$Vegetables_source_last_month)
for (i in 1:length(totdat$Vegetables_source_last_month)) {
  totdat$Vegetables_source_last_month[i]<-trimws(totdat$Vegetables_source_last_month[i])
}


#------------------------------------------------------------------------
#### nutrition ####
totdat$Fruits_source_bad<-gsub('gift_exchange','free',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('onfarm','on-farm',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('not_eaten','',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('gathered','free',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('off-farm','bought',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('free free','free',totdat$Fruits_source_bad)
totdat$Fruits_source_bad<-gsub('both','on-farm bought',totdat$Fruits_source_bad)
for (i in 1:length(totdat$Fruits_source_bad)) {
  totdat$Fruits_source_bad[i]<-trimws(totdat$Fruits_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Fruits_source_good))
{
totdat$Fruits_source_good<-gsub('gift_exchange','free',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('onfarm','on-farm',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('not_eaten','',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('gathered','free',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('off-farm','bought',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('free free','free',totdat$Fruits_source_good)
totdat$Fruits_source_good<-gsub('both','on-farm bought',totdat$Fruits_source_good)
for (i in 1:length(totdat$Fruits_source_good)) {
  totdat$Fruits_source_good[i]<-trimws(totdat$Fruits_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Fruits_source_last_month<-gsub('gift_exchange','free',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('onfarm','on-farm',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('not_eaten','',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('gathered','free',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('off-farm','bought',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('free free','free',totdat$Fruits_source_last_month)
totdat$Fruits_source_last_month<-gsub('both','on-farm bought',totdat$Fruits_source_last_month)
for (i in 1:length(totdat$Fruits_source_last_month)) {
  totdat$Fruits_source_last_month[i]<-trimws(totdat$Fruits_source_last_month[i])
}

#------------------------------------------------------------------------
#### nutrition ####
totdat$Meat_source_bad<-gsub('gift_exchange','free',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('onfarm','on-farm',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('not_eaten','',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('gathered','free',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('off-farm','bought',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('free free','free',totdat$Meat_source_bad)
totdat$Meat_source_bad<-gsub('both','on-farm bought',totdat$Meat_source_bad)
for (i in 1:length(totdat$Meat_source_bad)) {
  totdat$Meat_source_bad[i]<-trimws(totdat$Meat_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Meat_source_good))
{
totdat$Meat_source_good<-gsub('gift_exchange','free',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('onfarm','on-farm',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('not_eaten','',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('gathered','free',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('off-farm','bought',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('free free','free',totdat$Meat_source_good)
totdat$Meat_source_good<-gsub('both','on-farm bought',totdat$Meat_source_good)
for (i in 1:length(totdat$Meat_source_good)) {
  totdat$Meat_source_good[i]<-trimws(totdat$Meat_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Meat_source_last_month<-gsub('gift_exchange','free',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('onfarm','on-farm',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('not_eaten','',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('gathered','free',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('off-farm','bought',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('free free','free',totdat$Meat_source_last_month)
totdat$Meat_source_last_month<-gsub('both','on-farm bought',totdat$Meat_source_last_month)
for (i in 1:length(totdat$Meat_source_last_month)) {
  totdat$Meat_source_last_month[i]<-trimws(totdat$Meat_source_last_month[i])
}

#------------------------------------------------------------------------
#### nutrition ####
totdat$Eggs_source_bad<-gsub('gift_exchange','free',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('onfarm','on-farm',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('not_eaten','',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('gathered','free',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('off-farm','bought',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('free free','free',totdat$Eggs_source_bad)
totdat$Eggs_source_bad<-gsub('both','on-farm bought',totdat$Eggs_source_bad)
for (i in 1:length(totdat$Eggs_source_bad)) {
  totdat$Eggs_source_bad[i]<-trimws(totdat$Eggs_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Eggs_source_good))
{
totdat$Eggs_source_good<-gsub('gift_exchange','free',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('onfarm','on-farm',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('not_eaten','',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('gathered','free',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('off-farm','bought',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('free free','free',totdat$Eggs_source_good)
totdat$Eggs_source_good<-gsub('both','on-farm bought',totdat$Eggs_source_good)
for (i in 1:length(totdat$Eggs_source_good)) {
  totdat$Eggs_source_good[i]<-trimws(totdat$Eggs_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Eggs_source_last_month<-gsub('gift_exchange','free',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('onfarm','on-farm',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('not_eaten','',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('gathered','free',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('off-farm','bought',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('free free','free',totdat$Eggs_source_last_month)
totdat$Eggs_source_last_month<-gsub('both','on-farm bought',totdat$Eggs_source_last_month)
for (i in 1:length(totdat$Eggs_source_last_month)) {
  totdat$Eggs_source_last_month[i]<-trimws(totdat$Eggs_source_last_month[i])
}

#------------------------------------------------------------------------
#### nutrition ####

totdat$Milk_Dairy_source_bad<-gsub('gift_exchange','free',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('onfarm','on-farm',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('not_eaten','',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('gathered','free',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('off-farm','bought',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('free free','free',totdat$Milk_Dairy_source_bad)
totdat$Milk_Dairy_source_bad<-gsub('both','on-farm bought',totdat$Milk_Dairy_source_bad)
for (i in 1:length(totdat$Milk_Dairy_source_bad)) {
  totdat$Milk_Dairy_source_bad[i]<-trimws(totdat$Milk_Dairy_source_bad[i])
}
#------------------------------------------------------------------------
#### nutrition ####
if (!is.null(totdat$Milk_Dairy_source_good))
{
totdat$Milk_Dairy_source_good<-gsub('gift_exchange','free',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('onfarm','on-farm',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('not_eaten','',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('gathered','free',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('off-farm','bought',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('free free','free',totdat$Milk_Dairy_source_good)
totdat$Milk_Dairy_source_good<-gsub('both','on-farm bought',totdat$Milk_Dairy_source_good)
for (i in 1:length(totdat$Milk_Dairy_source_good)) {
  totdat$Milk_Dairy_source_good[i]<-trimws(totdat$Milk_Dairy_source_good[i])
}
}
#------------------------------------------------------------------------
#### nutrition ####
totdat$Milk_Dairy_source_last_month<-gsub('gift_exchange','free',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('onfarm','on-farm',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('not_eaten','',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('gathered','free',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('off-farm','bought',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('free free','free',totdat$Milk_Dairy_source_last_month)
totdat$Milk_Dairy_source_last_month<-gsub('both','on-farm bought',totdat$Milk_Dairy_source_last_month)
for (i in 1:length(totdat$Milk_Dairy_source_last_month)) {
  totdat$Milk_Dairy_source_last_month[i]<-trimws(totdat$Milk_Dairy_source_last_month[i])
}
}
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
####crop residues ####
if (!is.null(totdat$crop_residue_use_1))
{
totdat$crop_residue_use_1<-gsub('sell_give','sell',totdat$crop_residue_use_1)
}
# totdat$crop_residue_use_1[9245]<-'soil fuel'
# totdat$crop_residue_use_1[9273]<-'soil feed'
# totdat$crop_residue_use_1[9279]<-'soil feed'
# totdat$crop_residue_use_1[9281]<-'soil feed'
# totdat$crop_residue_use_1[9283]<-'soil feed'
# totdat$crop_residue_use_1[9297]<-'soil burn'
# totdat$crop_residue_use_1[9325]<-'soil fuel'
# totdat$crop_residue_use_1[9332]<-'soil feed'
# totdat$crop_residue_use_1[9333]<-'soil burn'
# totdat$crop_residue_use_1[9348]<-'soil burn feed'
# totdat$crop_residue_use_1[9411]<-'soil fuel'
# totdat$crop_residue_use_1[9456]<-'soil fuel'
# totdat$crop_residue_use_1[9459]<-'soil fuel'
# totdat$crop_residue_use_1[9465]<-'soil fuel feed'
# totdat$crop_residue_use_1[9469]<-'soil feed'
# totdat$crop_residue_use_1[9479]<-'soil feed'
# totdat$crop_residue_use_1[9502]<-'soil feed'
# totdat$crop_residue_use_1[9506]<-'soil feed'

if(!is.null(totdat$crop_residue_use_1))
{
for (i in 1:length(totdat$crop_residue_use_1)) {
  totdat$crop_residue_use_1[i]<-trimws(totdat$crop_residue_use_1[i])
}
}
if(!is.null(totdat$crop_residue_use_2))
{
totdat$crop_residue_use_2<-gsub('sell_give','sell',totdat$crop_residue_use_2)
for (i in 1:length(totdat$crop_residue_use_2)) {
  totdat$crop_residue_use_2[i]<-trimws(totdat$crop_residue_use_2[i])
}
}

if(!is.null(totdat$crop_residue_use_3))
{
  totdat$crop_residue_use_3<-gsub('sell_give','sell',totdat$crop_residue_use_3)
  for (i in 1:length(totdat$crop_residue_use_3)) {
    totdat$crop_residue_use_3[i]<-trimws(totdat$crop_residue_use_3[i])
  }
}
#totdat$crop_residue_use_3[9469]<-'soil feed'

if(!is.null(totdat$crop_residue_use_4))
{
  totdat$crop_residue_use_4<-gsub('sell_give','sell',totdat$crop_residue_use_4)
  for (i in 1:length(totdat$crop_residue_use_4)) {
    totdat$crop_residue_use_4[i]<-trimws(totdat$crop_residue_use_4[i])
  }
}

if(!is.null(totdat$crop_residue_use_5))
{
  totdat$crop_residue_use_5<-gsub('sell_give','sell',totdat$crop_residue_use_5)
  for (i in 1:length(totdat$crop_residue_use_5)) {
    totdat$crop_residue_use_5[i]<-trimws(totdat$crop_residue_use_5[i])
  }
}

if(!is.null(totdat$crop_residue_use_6))
{
  totdat$crop_residue_use_6<-gsub('sell_give','sell',totdat$crop_residue_use_6)
  for (i in 1:length(totdat$crop_residue_use_6)) {
    totdat$crop_residue_use_6[i]<-trimws(totdat$crop_residue_use_6[i])
  }
}

if(!is.null(totdat$crop_residue_use_7))
{
  totdat$crop_residue_use_7<-gsub('sell_give','sell',totdat$crop_residue_use_7)
  for (i in 1:length(totdat$crop_residue_use_7)) {
    totdat$crop_residue_use_7[i]<-trimws(totdat$crop_residue_use_7[i])
  }
}
if(!is.null(totdat$crop_residue_use_8))
{
  totdat$crop_residue_use_8<-gsub('sell_give','sell',totdat$crop_residue_use_8)
  for (i in 1:length(totdat$crop_residue_use_8)) {
    totdat$crop_residue_use_8[i]<-trimws(totdat$crop_residue_use_8[i])
  }
}
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
####crop use ####
if(!is.null(totdat$crop_use_1))
{
for (i in 1:length(totdat$crop_use_1)) {
  totdat$crop_use_1[i]<-trimws(totdat$crop_use_1[i])
}
index<-totdat$crop_use_1=='c(\"use\", \"sell\", \"givetrade\")'
totdat$crop_use_1[index]<-'use sell givetrade'
index<-totdat$crop_use_1=='c(\"use\", \"givetrade\")'
totdat$crop_use_1[index]<-'use givetrade'
index<-totdat$crop_use_1=='c(\"use\", \"sell\")'
totdat$crop_use_1[index]<-'use sell'
index<-totdat$crop_use_1=='c(\"sell\", \"givetrade\")'
totdat$crop_use_1[index]<-'use givetrade'
totdat$crop_use_1<-gsub('use','eat',totdat$crop_use_1)
}
#------------------------------------------------------------------------
####crop use ####
if(!is.null(totdat$crop_use_2))
{
for (i in 1:length(totdat$crop_use_2)) {
  totdat$crop_use_2[i]<-trimws(totdat$crop_use_2[i])
}
index<-totdat$crop_use_2=='c(\"use\", \"sell\", \"givetrade\")'
totdat$crop_use_2[index]<-'use sell givetrade'
index<-totdat$crop_use_2=='c(\"use\", \"givetrade\")'
totdat$crop_use_2[index]<-'use givetrade'
index<-totdat$crop_use_2=='c(\"use\", \"sell\")'
totdat$crop_use_2[index]<-'use sell'
index<-totdat$crop_use_2=='c(\"sell\", \"givetrade\")'
totdat$crop_use_2[index]<-'use givetrade'
totdat$crop_use_2<-gsub('use','eat',totdat$crop_use_2)
}
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------


# basename(file_name)
# 
# write.csv(totdat,paste0('./Clean_Data/', basename(file_name)))

totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub('youth_or_child', 'youth', x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub('female_head', 'female_adult', x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub('male_head', 'male_adult', x))) 

#totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub('male_head', 'male_adult', x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub("(adult)(female)", "\\1 \\2", x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub("(adult)(male)", "\\1 \\2", x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub("(youth)(female)", "\\1 \\2", x))) 
totdat[,grep('control|who|ownership', colnames(totdat))]<-data.frame(lapply(totdat[,grep('control|who|ownership', colnames(totdat))], function(x) gsub("(youth)(male)", "\\1 \\2", x))) 




dat_all<-totdat
