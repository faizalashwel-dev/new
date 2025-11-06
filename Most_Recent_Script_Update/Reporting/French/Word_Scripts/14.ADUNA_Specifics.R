dir.create("Word_Outputs/14.ADUNA_Specifics")

Existing_Burkina_Ghana<- paste0(dat$Country, " ",dat$Existing_or_New_Village)
Existing_Burkina_Ghana<- gsub(" Village", "", Existing_Burkina_Ghana)

Existing_Burkina_Ghana<-factor(Existing_Burkina_Ghana, levels=c("Ghana Existing","Ghana New", "Burkina_faso New"), ordered = T)

png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income.png")
boxplot(NTFP_Incomes$shea_fruit, outline=F, main="Shea Fruit Income", ylab="Income ($/HH/Year)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/shea_nut_income.png")
boxplot(NTFP_Incomes$shea_nut, outline=F, main="Shea Nut Income", ylab="Income ($/HH/Year)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income.png")
boxplot(NTFP_Incomes$baobab_fruit, outline=F, main="Baobab Fruit Income", ylab="Income ($/HH/Year)")
dev.off()


NTFP_incomes_group<- group_by(NTFP_Incomes, by=dat$TVA_Quartile)
NTFP_income_summary<-data.frame(summarise(NTFP_incomes_group, median(shea_fruit, na.rm = T), 
          median(shea_nut, na.rm = T),
          median(baobab_fruit, na.rm = T),
          IQR(shea_fruit, na.rm = T),
          IQR(shea_nut, na.rm = T),
          IQR(baobab_fruit, na.rm = T)))

colnames(NTFP_income_summary)<- c("TVA_Quartile",
                                  "Shea Fruit Income (median)",
                                  "Shea Nut Income (median)",
                                  "Baobab Fruit Income (median)",
                                  "Shea Fruit Income (IQR)",
                                  "Shea Nut Income (IQR)",
                                  "Baobab Fruit Income (IQR)")

write.csv(NTFP_income_summary, "Word_Outputs/14.ADUNA_Specifics/NTFP_income_QUARTILES.csv", row.names=F)


png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income_QUARTILES.png")
boxplot(NTFP_Incomes$shea_fruit~dat$TVA_Quartile,outline=F , main="Shea Fruit Income by TVA Quartile", ylab="Income ($/HH/Year)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/shea_nut_income_QUARTILES.png")
boxplot(NTFP_Incomes$shea_nut~dat$TVA_Quartile,outline=F, main="Shea Nut Income by TVA Quartile", ylab="Income ($/HH/Year)" )
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income_QUARTILES.png")
boxplot(NTFP_Incomes$baobab_fruit~dat$TVA_Quartile,outline=F , main="Baobab Fruit Income by TVA Quartile", ylab="Income ($/HH/Year)")
dev.off()




png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Incomes$shea_fruit~Existing_Burkina_Ghana,outline=F, main="Shea Fruit Income by New/Existing", ylab="Income ($/HH/Year)" )
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/shea_nut_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Incomes$shea_nut~Existing_Burkina_Ghana,outline=F , main="Shea Nut Income by New/Existing", ylab="Income ($/HH/Year)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Incomes$baobab_fruit~Existing_Burkina_Ghana,outline=F , main="Baobab Fruit Income by New/Existing", ylab="Income ($/HH/Year)")
dev.off()

NTFP_incomes_group<- group_by(NTFP_Incomes, by=Existing_Burkina_Ghana)
NTFP_income_summary<-data.frame(summarise(NTFP_incomes_group, median(shea_fruit, na.rm = T), 
                                          median(shea_nut, na.rm = T),
                                          median(baobab_fruit, na.rm = T),
                                          IQR(shea_fruit, na.rm = T),
                                          IQR(shea_nut, na.rm = T),
                                          IQR(baobab_fruit, na.rm = T)))

colnames(NTFP_income_summary)<- c("Existing/New Burkina/Ghana",
                                  "Shea Fruit Income (median)",
                                  "Shea Nut Income (median)",
                                  "Baobab Fruit Income (median)",
                                  "Shea Fruit Income (IQR)",
                                  "Shea Nut Income (IQR)",
                                  "Baobab Fruit Income (IQR)")

write.csv(NTFP_income_summary, "Word_Outputs/14.ADUNA_Specifics/NTFP_income_Existing_New.csv", row.names=F)



png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income.png")
boxplot(NTFP_Energy_All$shea_fruit, outline=F, main="Energy Shea Fruit Consumption", ylab="Energy (kcal/HH/day)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income.png")
boxplot(NTFP_Energy_All$baobab_fruit, outline=F, main="Energy Baobab Fruit Consumption", ylab="Energy (kcal/HH/day)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_leaves_income.png")
boxplot(NTFP_Energy_All$baobab_leaves, outline=F, main="Energy Baobab Leaf Consumption", ylab="Energy (kcal/HH/day)")
dev.off()

png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income_QUARTILES.png")
boxplot(NTFP_Energy_All$shea_fruit~dat$TVA_Quartile,outline=F, main="Energy Shea Fruit Consumption by TVA Quartile", ylab="Energy (kcal/HH/day)" )
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income_QUARTILES.png")
boxplot(NTFP_Energy_All$baobab_fruit~dat$TVA_Quartile,outline=F , main="Energy Baobab Fruit Consumption by TVA Quartile", ylab="Energy (kcal/HH/day)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_leaves_income_QUARTILES.png")
boxplot(NTFP_Energy_All$baobab_leaves~dat$TVA_Quartile,outline=F, main="Energy Baobab Leaf Consumption by TVA Quartile", ylab="Energy (kcal/HH/day)" )
dev.off()

png("Word_Outputs/14.ADUNA_Specifics/shea_fruit_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Energy_All$shea_fruit~Existing_Burkina_Ghana,outline=F, main="Energy Shea Fruit Consumption by Existing/Non-Existing", ylab="Energy (kcal/HH/day)" )
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_fruit_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Energy_All$baobab_fruit~Existing_Burkina_Ghana,outline=F , main="Energy Baobab Fruit Consumption by Existing/Non-Existing", ylab="Energy (kcal/HH/day)")
dev.off()
png("Word_Outputs/14.ADUNA_Specifics/baobab_leaves_income_EXISTING_VILLAGES.png")
boxplot(NTFP_Energy_All$baobab_leaves~Existing_Burkina_Ghana,outline=F , main="Energy Baobab Leaf Consumption by Existing/Non-Existing", ylab="Energy (kcal/HH/day)")
dev.off()




Number_of_sellers_QUARTILES<-data.frame("Existing_New"=Existing_Burkina_Ghana, 
                                        "shea_fruit"=!is.na(NTFP_Incomes$shea_fruit),
                                        "shea_nut"=!is.na(NTFP_Incomes$shea_nut),
                                        "baobab_fruit"=!is.na(NTFP_Incomes$baobab_fruit))

shea_fruit_sell<-data.frame(round(100*prop.table(table(Number_of_sellers_QUARTILES$Existing_New, Number_of_sellers_QUARTILES$shea_fruit), margin=1),2))
shea_fruit_sell<-shea_fruit_sell[shea_fruit_sell$Var2==TRUE,]
shea_fruit_sell$Var2<-NULL
colnames(shea_fruit_sell)<- c("Existing/New", "% of HHs")

write.csv(shea_fruit_sell,"Word_Outputs/14.ADUNA_Specifics/Percentage_selling_shea_fruit_EXISTING.csv",row.names = F)

shea_nut_sell<-data.frame(round(100*prop.table(table(Number_of_sellers_QUARTILES$Existing_New, Number_of_sellers_QUARTILES$shea_nut), margin=1),2))
shea_nut_sell<-shea_nut_sell[shea_nut_sell$Var2==TRUE,]
shea_nut_sell$Var2<-NULL
colnames(shea_nut_sell)<- c("Existing/New", "% of HHs")

write.csv(shea_nut_sell,"Word_Outputs/14.ADUNA_Specifics/Percentage_selling_shea_nut_EXISTING.csv",row.names = F)


baobab_fruit_sell<-data.frame(round(100*prop.table(table(Number_of_sellers_QUARTILES$Existing_New, Number_of_sellers_QUARTILES$baobab_fruit), margin=1),2))
baobab_fruit_sell<-baobab_fruit_sell[baobab_fruit_sell$Var2==TRUE,]
baobab_fruit_sell$Var2<-NULL
colnames(baobab_fruit_sell)<- c("Existing/New", "% of HHs")
write.csv(baobab_fruit_sell,"Word_Outputs/14.ADUNA_Specifics/Percentage_selling_baobab_fruit_EXISTING.csv",row.names = F)


  






