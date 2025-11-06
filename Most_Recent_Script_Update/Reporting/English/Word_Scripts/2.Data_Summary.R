


quarts <- quantile(dat$TVA_pmae_pday, na.rm = T)
dat$TVA_quartiles <- ifelse(dat$TVA_pmae_pday<=quarts[2], "lowest",
                        ifelse(dat$TVA_pmae_pday>quarts[2] & dat$TVA_pmae_pday<=quarts[3], "lower middle", 
                               ifelse(dat$TVA_pmae_pday>quarts[3] & dat$TVA_pmae_pday<=quarts[4], "upper middle",
                                      ifelse(dat$TVA_pmae_pday>quarts[4] & dat$TVA_pmae_pday<=quarts[5], "upper", NA))))


# Writing a table to display the TVA per FA group quartile, along with the number of households in each quartile
dat$TVA_quartiles <- factor(dat$TVA_quartiles, levels=c("lowest","lower middle","upper middle","upper"))

dat_grp <- group_by(dat, TVA_quartiles)


TVA_and_Cash_Income<-summarise(dat_grp, round(median(TVA_pmae_pday, na.rm=T),2), 
                               round(median(total_income/365/HHsizeMAE, na.rm=T),2))
colnames(TVA_and_Cash_Income) <- c("Quartiles", 
                                   "Median Total Value of Activities ($/person/day)", 
                                   "Median cash income ($/person/day)") 
# whole_pop<- c("All", 
#               round(median(dat$TVA_pmae_pday, na.rm=T),2), 
#               round(median(dat$total_income/365/dat$HHsizeMAE, na.rm=T),2))
# TVA_and_Cash_Income$Quartiles<- as.character(TVA_and_Cash_Income$Quartiles)
# TVA_and_Cash_Income<- rbind(TVA_and_Cash_Income, whole_pop)

write.csv(TVA_and_Cash_Income, "Word_Outputs/2.Data_Summary/1.TVA_and_Cash_Income.csv", row.names = F)




Hungry_Months_and_HDDS_FIES<-summarise(dat_grp, round(median(NrofMonthsFoodInsecure, na.rm=T)), 
                                       round(median(score_HDDS_GoodSeason, na.rm=T)),
                                       round(median(score_HDDS_BadSeason, na.rm=T)))
# round(median(FIES_Status, na.rm=T))

colnames(Hungry_Months_and_HDDS_FIES) <- c("Quartiles", 
                                           "Hungry months (median)", 
                                           "Median Dietary Diversity (Good Season)",
                                           "Median Dietary Diversity (Bad Season)")
#"FIES Status") 

# whole_pop<- c("All", 
#               round(median(dat$NrofMonthsFoodInsecure, na.rm=T)), 
#               round(median(dat$score_HDDS_GoodSeason, na.rm=T)),
#               round(median(dat$score_HDDS_BadSeason, na.rm=T)),
#               round(median(dat$FIES_Status, na.rm=T))
#               )
# Hungry_Months_and_HDDS_FIES$Quartiles<- as.character(Hungry_Months_and_HDDS_FIES$Quartiles)
# Hungry_Months_and_HDDS_FIES<- rbind(Hungry_Months_and_HDDS_FIES, whole_pop)

write.csv(Hungry_Months_and_HDDS_FIES, "Word_Outputs/2.Data_Summary/2.Hungry_Months_and_HDDS_FIES.csv", row.names = F)









Farm_and_Household_Characteristics<-summarise(dat_grp, round(median(HHsizemembers, na.rm=T)), 
                                              round(median(LandOwned, na.rm=T),1),
                                              round(median(LandCultivated, na.rm=T),1)
)

colnames(Farm_and_Household_Characteristics) <- c("Quartiles", 
                                                  "# of HH members", 
                                                  "Median Land Owned (ha)",
                                                  "Median Land Cultivated (ha") 

# whole_pop<- c("All", 
#               round(median(dat$HHsizemembers, na.rm=T)), 
#               round(median(dat$LandOwned, na.rm=T),1),
#               round(median(dat$LandCultivated, na.rm=T),1)
# )
# Farm_and_Household_Characteristics$Quartiles<- as.character(Farm_and_Household_Characteristics$Quartiles)
# Farm_and_Household_Characteristics<- rbind(Farm_and_Household_Characteristics, whole_pop)
write.csv(Farm_and_Household_Characteristics, "Word_Outputs/2.Data_Summary/3.Farm_and_Household_Characteristics.csv", row.names = F)

# NRM_Bio_df<-List_to_True_False(dat_all$biological_methods)
# NRM_Bio_df<-data.frame(colSums(NRM_Bio_df))
# NRM_Bio_df[,1]<-round(100*NRM_Bio_df[,1]/nrow(dat_all),1)
# NRM_Bio_df<-data.frame("Biological Method"=row.names(NRM_Bio_df), "Percentage of HHs Practicing"=NRM_Bio_df[,1]) 
# NRM_Bio_df<-NRM_Bio_df[order(NRM_Bio_df[,2], decreasing = TRUE),]



# NRM_Soil_Water<-List_to_True_False(dat_all$soil_water_cons)
# NRM_Soil_Water<-data.frame(colSums(NRM_Soil_Water))
# NRM_Soil_Water[,1]<-round(100*NRM_Soil_Water[,1]/nrow(dat_all),1)
# NRM_Soil_Water<-data.frame("Soil and Water Conservation"=row.names(NRM_Soil_Water), "Percentage of HHs Practicing"=NRM_Soil_Water[,1]) 
# NRM_Soil_Water<-NRM_Soil_Water[order(NRM_Soil_Water[,2], decreasing = TRUE),]
# 
# 
# 
# NRM_Gully<-List_to_True_False(dat_all$gully_control)
# NRM_Gully<-data.frame(colSums(NRM_Gully))
# NRM_Gully[,1]<-round(100*NRM_Gully[,1]/nrow(dat_all),1)
# NRM_Gully<-data.frame("Gully Control"=row.names(NRM_Gully), "Percentage of HHs Practicing"=NRM_Gully[,1]) 
# NRM_Gully<-NRM_Gully[order(NRM_Gully[,2], decreasing = TRUE),]
# 
# 
# write.csv(NRM_Bio_df, "Word_Outputs/2.Data_Summary/4.NRM_Bio.csv", row.names = F)
# write.csv(NRM_Soil_Water, "Word_Outputs/2.Data_Summary/5.NRM_Soil_and_Water.csv", row.names = F)
# write.csv(NRM_Gully, "Word_Outputs/2.Data_Summary/6.NRM_Gully.csv", row.names = F)
# 


crops_most_important<-List_to_True_False(dat_all$crops_all)
crops_most_important<-data.frame(colSums(crops_most_important))
crops_most_important[,1]<-round(100*crops_most_important[,1]/nrow(dat_all),1)
crops_most_important<-data.frame("Crop"=row.names(crops_most_important), "Percentage of HHs Growing"=crops_most_important[,1]) 
crops_most_important<-crops_most_important[order(crops_most_important[,2], decreasing = TRUE),]

if ("other" %in% crops_most_important$Crop)
{
  crops_most_important<-crops_most_important[-grep("other", crops_most_important$Crop),]
}
Livestock_most_important<-List_to_True_False(dat_all$livestock_all)
Livestock_most_important<-data.frame(colSums(Livestock_most_important))
Livestock_most_important[,1]<-round(100*Livestock_most_important[,1]/nrow(dat_all),1)
Livestock_most_important<-data.frame("Livestock Kept"=row.names(Livestock_most_important), "Percentage of HHs Keeping Animal"=Livestock_most_important[,1]) 
Livestock_most_important<-Livestock_most_important[order(Livestock_most_important[,2], decreasing = TRUE),]
Livestock_most_important<-Livestock_most_important[-grep("other", Livestock_most_important$Livestock.Kept),]

# NTFP_most_important<-List_to_True_False(dat_all$gather_NTFP)
# NTFP_most_important<-data.frame(colSums(NTFP_most_important))
# NTFP_most_important[,1]<-round(100*NTFP_most_important[,1]/nrow(dat_all),1)
# NTFP_most_important<-data.frame("NTFP Gathered"=row.names(NTFP_most_important), "Percentage of HHs Gathering"=NTFP_most_important[,1]) 
# NTFP_most_important<-NTFP_most_important[order(NTFP_most_important[,2], decreasing = TRUE),]
# 
# Food_Trees_most_important<-List_to_True_False(dat_all$food_trees_which)
# Food_Trees_most_important<-data.frame(colSums(Food_Trees_most_important))
# Food_Trees_most_important[,1]<-round(100*Food_Trees_most_important[,1]/nrow(dat_all),1)
# Food_Trees_most_important<-data.frame("Food Tree Used"=row.names(Food_Trees_most_important), "Percentage of HHs Using"=Food_Trees_most_important[,1]) 
# Food_Trees_most_important<-Food_Trees_most_important[order(Food_Trees_most_important[,2], decreasing = TRUE),]
# Food_Trees_most_important<-Food_Trees_most_important[-grep("other", Food_Trees_most_important$Food.Tree.Used),]


write.csv(crops_most_important, "Word_Outputs/2.Data_Summary/3.Crops_Summary.csv", row.names = F)
write.csv(Livestock_most_important, "Word_Outputs/2.Data_Summary/4.Livestock_Summary.csv", row.names = F)
#write.csv(NTFP_most_important, "Word_Outputs/2.Data_Summary/9.NTFP_Summary.csv", row.names = F)
#write.csv(Food_Trees_most_important, "Word_Outputs/2.Data_Summary/10.Food_Tree_Summary.csv", row.names = F)











