colnames(dat_all)


startwith_person<-grep("^person|^head_person", colnames(dat_all))
end_with_num<-grep("[1-9$]", colnames(dat_all))
person_rep<- dat_all[startwith_person[startwith_person%in%end_with_num]]

loop_number<-as.numeric(unique(gsub('.*_', '', colnames(person_rep))))




dat_all$children_under_4<-rep(0, nrow(dat_all))
dat_all$children_4to10<-rep(0, nrow(dat_all))
dat_all$males11to24<-rep(0, nrow(dat_all))
dat_all$females11to24<-rep(0, nrow(dat_all))
dat_all$males25to50<-rep(0, nrow(dat_all))
dat_all$females25to50<-rep(0, nrow(dat_all))
dat_all$malesover50<-rep(0, nrow(dat_all))
dat_all$femalesover50<-rep(0, nrow(dat_all))

rowSums(!is.na(data.frame(dat_all$age_femalehead,dat_all$age_malehead)))

person_age<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))
person_sex<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))
head_age<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))
head_sex<- data.frame(matrix(NA, ncol=length(loop_number), nrow=nrow(dat_all)))

for (i in loop_number)
{
  age_temp<- as.numeric(as.character(person_rep[,  colnames(person_rep)==paste0("person_age_",i)]))
  sex_temp<-as.character(person_rep[,  colnames(person_rep)==paste0("person_gender_",i)])
  
  person_age[,i]<-age_temp
  person_sex[,i]<-sex_temp
  
  for (j in 1:nrow(dat_all))
  {
    if (!is.na(age_temp[j]) && !is.na(sex_temp[j]))
    {
      if (age_temp[j]>0 && age_temp[j] <=4) {dat_all$children_under_4[j]<- dat_all$children_under_4[j]+1}
      if (age_temp[j]>4 && age_temp[j] <=10) {dat_all$children_4to10[j]<-dat_all$children_4to10[j]+1}
      if (age_temp[j]>10 && age_temp[j] <=24 && sex_temp[j]=="M") {dat_all$males11to24[j]<-dat_all$males11to24[j]+1}
      if (age_temp[j]>10 && age_temp[j] <=24 && sex_temp[j]=="F") {dat_all$females11to24[j]<-dat_all$females11to24[j]+1}
      if (age_temp[j]>24 && age_temp[j] <=50 && sex_temp[j]=="M") {dat_all$males25to50[j]<-dat_all$males25to50[j]+1}
      if (age_temp[j]>24 && age_temp[j] <=50 && sex_temp[j]=="F") {dat_all$females25to50[j]<-dat_all$females25to50[j]+1}
      if (age_temp[j]>50 &&  sex_temp[j]=="M") {dat_all$malesover50[j]<-dat_all$malesover50[j]+1}
      if (age_temp[j]>50 && sex_temp[j]=="F") {dat_all$femalesover50[j]<-dat_all$femalesover50[j]+1}
      
    }
  }
}
