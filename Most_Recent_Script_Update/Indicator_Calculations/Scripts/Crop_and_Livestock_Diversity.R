####crop diversity and livestock diversity####


nr_crops<- rowSums(List_to_True_False(as.character(dat_all$crops_all)))

nr_fruits<- rowSums(List_to_True_False(as.character(dat_all$fruits_which)))

nr_veg<- rowSums(List_to_True_False(as.character(dat_all$vegetables_which)))

cropdiv_nr<-nr_crops+nr_veg+nr_fruits

livestock_div<-as.numeric(as.character(dat_all$livestock_count))
