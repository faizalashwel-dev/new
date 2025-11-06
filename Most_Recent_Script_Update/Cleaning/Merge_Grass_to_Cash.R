dat_1<-data.frame(read_csv('Data/Kenya_Ethiopia_Only.csv',na=c('n/a','<NA>')))
dat_2<-data.frame(read_csv('Data/ONA_Ethiopia_Only.csv',na=c('n/a','<NA>')))
dat_3<-data.frame(read_csv('Data/Kenya_Forages_Only.csv',na=c('n/a','<NA>')))

nrow(dat_1)
nrow(dat_2)
nrow(dat_3)

ncol(dat_1)
ncol(dat_2)
ncol(dat_3)


dat_3$SECTION_META.householdID[dat_3$SECTION_META.householdID %in% dat_1$SECTION_META.householdID==F]

dat_1$SECTION_META.householdID<- gsub(' ','',dat_1$SECTION_META.householdID)

dat_1$SECTION_META.householdID<- gsub('KAKAMEGA','KAK',dat_1$SECTION_META.householdID)
dat_1$SECTION_META.householdID<- gsub('AKA','KAK',dat_1$SECTION_META.householdID)
dat_1$SECTION_META.householdID<- gsub('KAk','KAK',dat_1$SECTION_META.householdID)


# 
# 
# dat_1[dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID ,colnames(dat_1)[colnames(dat_1)%in% colnames(dat_3)==T]]<- dat_3[dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID ,colnames(dat_1)[colnames(dat_1)%in% colnames(dat_3)==T]]  
# 
# 
# 
# dat_1[dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID ,colnames(dat_1)[colnames(dat_1)%in% colnames(dat_3)==T]]
# 
# 
# 
# dat_1[dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID ,colnames(dat_1)[colnames(dat_1)%in% colnames(dat_3)==T]]
# 
# 
# dat_1$SECTION_META.householdID[which(dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID)]
# 
 dat_3$SECTION_META.householdID[which(dat_1$SECTION_META.householdID %in% dat_3$SECTION_META.householdID)]
# 
# order_1<-unique(dat_1$SECTION_META.householdID[ dat_1$SECTION_META.householdID%in% dat_3$SECTION_META.householdID])
# 
# dat_3$SECTION_META.householdID<- factor(dat_3$SECTION_META.householdID, levels = c(order_1))
# 
# data.frame(order_1, dat_3$SECTION_META.householdID[order_1])
# 
which(dat_1$SECTION_META.householdID%in%dat_3$SECTION_META.householdID)



for (i in 1:nrow(dat_1))
{
  if (!is.na(dat_1$SECTION_META.householdID[i]))
  {
  if (dat_1$SECTION_META.householdID[i] %in% dat_3$SECTION_META.householdID==T)
  {
    dat_1[i,which(colnames(dat_1)%in% colnames(dat_3))]<- dat_3[dat_3$SECTION_META.householdID==dat_1$SECTION_META.householdID[i],which(colnames(dat_3)%in% colnames(dat_1))]
  }
  }
}


write_csv(dat_1,'Data/Kenya_Merged.csv')




