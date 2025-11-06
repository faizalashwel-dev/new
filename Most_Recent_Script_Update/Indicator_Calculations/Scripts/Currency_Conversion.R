#### Currency Conversion script ####
# A script which changes all incomes in the core data sheet into the private international $ PPP (based on world bank info)


#grouping all of the income columns together
income_columns<- grep("income", colnames(dat_all))
# Removing non-numeric income columns so that their data isn't lost
remove_columns<- which(colnames(dat_all)%in%c("offfarm_income_name_1",
                                                  "offfarm_income_name_2",
                                                  "offfarm_income_name_3",
                                                  "offfarm_income_name_4",
                                                  "offfarm_income_name_5",
                                                  "offfarm_income_name_6",
                                                  
                                                  "spending_On_farm_income",
                                                  "spending_Off_farm_income",
                                                  "offfarm_income_proportion",
                                                  "offfarm_incomes_other",
                                                  "offfarm_incomes_count",
                                                  "offfarm_incomes",
                                                  "offfarm_incomes_any",
                                                  "shea_fruit_sold_income_who",
                                                  "shea_nut_sold_income_who",
                                                  "baobab_sold_income_who",
                                                  "moringa_sold_income_who",
                                                  "balanites_sold_income_who",
                                                  "honey_sold_income_who",
                                                  "locustbean_sold_income_who",
                                                  "gumarabic_sold_income_who",
                                                  "tamarind_sold_income_who",
                                                  "sabasenegal_sold_income_who",
                                                  "jujube_sold_income_who",
                                                  "tallow_sold_income_who",
                                                  "cashew_sold_income_who",
                                                  "wild_grape_sold_income_who",
                                                  "FP_materials_sold_income_prop",
                                                  "FP_materials_income_control"))


# Removing character columns from income collumns
income_columns<-income_columns[!(income_columns%in%remove_columns)]
income_columns<- colnames(dat_all[,income_columns])

#making sure the "year" value is numeric
dat_all$year<- as.numeric(dat_all$year)

conversion_factor<-data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
# looping through every row of the data set
for (i in 1:nrow(dat_all))
{
  #checking if the country name is in the world bank table
    if(dat_all$country_wb[i] %in% as.character(wb_table$`Country Name`)==TRUE)
  {
    #' this while loop changes the year variable to make sure that conversion factors can be extracted from the world
    #' bank currency conversion table for years where a conversion factor is listed
    while(length(wb_table[wb_table$`Country Name`==dat_all$country_wb[i],colnames(wb_table)==dat_all$year[i]])==0 || is.na(wb_table[wb_table$`Country Name`==dat_all$country_wb[i],colnames(wb_table)==dat_all$year[i]]))
    {
      if(is.na(dat_all$year[i])) {dat_all$year[i]<-2018}
      if(dat_all$year[i]<2012)   {dat_all$year[i]<- dat_all$year[i]+1}
      if(dat_all$year[i]>2016)   {dat_all$year[i]<- dat_all$year[i]-1}
    }
    conversion_factor[i,1]<- wb_table[as.character(wb_table$`Country Name`)==dat_all$country_wb[i],colnames(wb_table)==as.character(dat_all$year[i])]
  }
}

#In some projects, incomes are given as a factor of 1000LCU, this if statements is used to adjust the conversion factor accordingly
if (project_ID=='VN_HAM_2016')
{
  conversion_factor[,1]== conversion_factor[,1]/1000
}
if (project_ID=='KH_HAM_2016')
{
  conversion_factor[,1]<- conversion_factor[,1]/1000
}

if (project_ID=='KH_SIL_2018')
{
  conversion_factor[,1]<- conversion_factor[,1]/1000
}

if (project_ID=='LA_HAM_2016')
{
  conversion_factor[,1]<- conversion_factor[,1]
}

if (project_ID=='VN_NTR_2019')
{
  conversion_factor[,1]<-conversion_factor[,1]/1000
}


if (project_ID=='BI_SNV_2019')
{
  conversion_factor[,1]<-conversion_factor[,1]/1000
}

# This conversion factor is applied to every income variable in the data set.
for (i in income_columns)
{
  for (j in 1:nrow(dat_all))
  {
    dat_all[j,colnames(dat_all)==i]<- as.numeric(dat_all[j,colnames(dat_all)==i])/as.numeric(conversion_factor[j,1])
  }
}
