previous_crop_names<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv",na = c("n/a","<NA>", "999", 'NA')))#, fileEncoding="UTF-8", stringsAsFactors = FALSE)
previous_livestock_names<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Livestock_Spellings.csv",na= c("n/a","<NA>", "999", 'NA')))#, fileEncoding="UTF-8", stringsAsFactors = FALSE)

for (i in 1:length(grep("crop_name_", colnames(dat_all))))
{
  
  for (j in 1:nrow(dat_all))
  {
    if(!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
    {
      
      
      if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other1")
      {
        dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other1[j] 
      }
      
      if (!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other2")
        {
          dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other2[j] 
        }
      }
      if (!is.na(dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]=="other3")
        {
          dat_all[j,colnames(dat_all)==paste0("crop_name_",i)]<- dat_all$crops_other3[j] 
        }
      }
    }
  }
  
  crop_names_temp<- unique(dat_all[, colnames(dat_all)==paste0("crop_name_",i)])
  
  
  for (j in 1:length(crop_names_temp))
  {
    if (!is.na(crop_names_temp[j]))
    {
    if (crop_names_temp[j]%in%previous_crop_names$Original_Spelling==FALSE && crop_names_temp[j]%in%previous_crop_names$Standardised_Spelling==FALSE)
    {
      previous_crop_names<- rbind(previous_crop_names, data.frame("Original_Spelling"=crop_names_temp[j], "Standardised_Spelling"=NA))
    }
    }
  }
  
}



for (i in 1:length(grep("livestock_name_", colnames(dat_all))))
{
  
  for (j in 1:nrow(dat_all))
  {
    if(!is.na(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]))
    {
      if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]=="other1")
      {
        dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]<- dat_all$livestock_other1[j] 
      }
      
      if (!is.na(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]=="other2")
        {
          dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]<- dat_all$livestock_other2[j] 
        }
      }
      if(!is.na(dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]))
      {
        if (dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]=="other3")
        {
          dat_all[j,colnames(dat_all)==paste0("livestock_name_",i)]<- dat_all$livestock_other3[j] 
        }
      }
      
    }
  }
  
  livestock_names_temp<- unique(dat_all[, colnames(dat_all)==paste0("livestock_name_",i)])
  
  
  for (j in 1:length(livestock_names_temp))
  {
    if (!is.na(livestock_names_temp[j]))
    {
    if ( livestock_names_temp[j]%in%previous_livestock_names$Original_Spelling==FALSE && livestock_names_temp[j]%in%previous_livestock_names$Standardised_Spelling==FALSE)
    {
      
      previous_livestock_names<- rbind(previous_livestock_names, data.frame("Original_Spelling"=livestock_names_temp[j], "Standardised_Spelling"=NA))
    }
    }
    
  }
}


previous_crop_names<- write_csv(previous_crop_names,"../../Units_and_Cleaning_Data/Cleaning/Crop_Spellings.csv")
previous_livestock_names<- write_csv(previous_livestock_names, "../../Units_and_Cleaning_Data/Cleaning/Livestock_Spellings.csv")









