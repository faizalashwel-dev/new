Leo_Indicator_Sheet<- read.csv("Outputs/Indicator_Sheets/Malawi_2019/indicator_sheet.csv",na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
Leo_Indicator_Sheet <- data.frame(lapply(Leo_Indicator_Sheet, as.character), stringsAsFactors=FALSE)

numeric_variables_leo<- c("HHsizemembers", 
                          "HHsizeMAE", 
                          "LandOwned",
                          "LandCultivated",
                          "LivestockHoldings",
                          "NrofMonthsFoodInsecure",
                          "FIES_Score",
                          "score_HDDSGoodSeason",
                          "score_HDDS_farmbasedGoodSeason",
                          "score_HDDS_purchasedGoodSeason",
                          "score_HDDS_badseason",
                          "score_HDDS_farmbasedBadSeason",
                          "score_HDDS_purchasedBadSeason",
                          "total_income",
                          "offfarm_income",
                          "farm_income",
                          "valuefarmproduce",
                          "cropsales",
                          "valuecropproduce",
                          "valuecropconsumed",
                          "livestockprodsales",
                          "valuelivestockproduction",
                          "valuelivestockprodconsumed",
                          "FoodAvailability",
                          "FoodSelfSufficiency",
                          "TotalEnergyAvailable",
                          "FAEnergyBought",
                          "FAEnergyOffFarm",
                          "FAEnergyCropConsumption",
                          "FAEnergyCropSales",
                          "FAEnergyLivestockConsumption",
                          "FAEnergyLivestockSales",
                          "FAEnergyFarmBased",
                          "FAMarketOrientation",
                          "FALivestockOrientation",
                          
                          # "NrofMonthsWildFoodCons",
                          "GHGEmissions",
                          "Gender_MaleControl",
                          "Gender_FemaleControl",
                          "Gender_MaleYouthControl",
                          "Gender_FemaleYouthControl",
                          "NFertInput",
                          "CropDiv",
                          "LivestockDiv")

Leo_Indicator_Sheet[,colnames(Leo_Indicator_Sheet)%in%numeric_variables_leo==TRUE]<-lapply(Leo_Indicator_Sheet[,colnames(Leo_Indicator_Sheet)%in%numeric_variables_leo==TRUE], function (x) as.numeric(x))


dat<- Leo_Indicator_Sheet
#dat<- Mark_Indicator_Sheet
Country<-"Malawi"
Year<-"2019"
prop_rat <- length(dat[,1])
cal_line = 2500
pov_line = 1.9


#leo convkcal
convkcal_USD <- ifelse(dat$livestockprodsales>0, (dat$FAEnergyLivestockSales*dat$HHsizeMAE)/(dat$livestockprodsales/365), #if a>0 the convkcal_USD= b/a
                       ifelse(dat$cropsales>0, (dat$FAEnergyCropSales*dat$HHsizeMAE)/(dat$cropsales/365), NA))                   #otherwise its the next ifelse

#mark convkcal
#convkcal_USD <- ifelse(dat$livestockprodsales>0, (dat$FAEnergyLivestockSales)/(dat$livestockprodsales), #if a>0 the convkcal_USD= b/a
#                      ifelse(dat$cropsales>0, (dat$FAEnergyCropSales)/(dat$cropsales), NA))                   #otherwise its the next ifelse



convkcal_USD<-convkcal_USD[complete.cases(convkcal_USD)]                                            #finds which cases of convkcal_USD are complete
convkcal_USD <- rep(convkcal_USD[1], times=length(dat[,1]))                                         #Replaces the first value in convkcal_USD (length dat specifies the number of entries in the data)


X<- cal_line
Y<- convkcal_USD[1] * pov_line
X1 <- cal_line / convkcal_USD[1]
Y1 <- pov_line

TVA_pmae_pday <- rowSums(data.frame((dat$valuecropconsumed/dat$HHsizeMAE/365), (dat$valuelivestockprodconsumed/dat$HHsizeMAE/365), (dat$cropsales/dat$HHsizeMAE/365), (dat$livestockprodsales/dat$HHsizeMAE/365), (dat$offfarm_income/dat$HHsizeMAE/365)), na.rm=T)
dat$TVA_pmae_pday<-TVA_pmae_pday
Cash_inc <-  rowSums(data.frame(dat$cropsales, dat$livestockprodsales, dat$offfarm_income), na.rm=T)/dat$HHsizeMAE/365

# #Marks Graph
#   FSSorder=order(dat$FoodAvailability)
#   #set the y-limit of the barplot
#   ylim=c(0,20*2500)
#   #Barplot of FoodAvailability
#   png("Outputs/Mark_PFA_Barchart.png", height = 800, width = 1000)
#   par(cex=2)
#   barplot((dat$FAEnergyCropConsumption/dat$HHsizeMAE/365)[FSSorder], col="green", ylim=ylim, ylab="Food Availability [kcal per MAE per day]", border=NA)
#   off=(dat$FAEnergyCropConsumption/dat$HHsizeMAE/365)[FSSorder]
#   income_crop=(dat$FAEnergyCropSales)/dat$HHsizeMAE/365
#   barplot(income_crop[FSSorder], col="blue", offset=off,add=TRUE, border=NA)
#   off=off+income_crop[FSSorder]
#   barplot((dat$FAEnergyLivestockConsumption/dat$HHsizeMAE/365)[FSSorder], col="red", offset=off,add=TRUE, border=NA)
#   off=off+(dat$FAEnergyLivestockConsumption/dat$HHsizeMAE/365)[FSSorder]
#   income_lvst=(dat$FAEnergyLivestockSales)/dat$HHsizeMAE/365
#   barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
#   off=off+income_lvst[FSSorder]
#   offfarm_w=(dat$FAEnergyOffFarm)/dat$HHsizeMAE/365
#   barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
#   #off=off+offfarm_w[FSSorder]
#   #barplot((dat$PotEnergyfromNTFP/dat$HHsizeMAE/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
# abline(h=X,lty=2,lwd=3,col="red")
# abline(h=Y,lty=2,lwd=3,col="blue")
# #compute the percentage of food insecure household
# FS.p=round(sum(dat$FoodAvailability<cal_line, na.rm=T)/length(dat$FoodAvailability)*100)
# #legend("topleft",legend=c("NTFP", "off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("purple", "black", "orange", "red", "blue", "green",NA))
# legend("topleft",legend=c("off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("black", "orange", "red", "blue", "green",NA))
# title(paste0("Food Availability\n",Country," ", Year), cex.main=1.5)
# dev.off()

dir.create("Outputs/Test_Graphs/Malawi_2019")

FSSorder=order(dat$FoodAvailability)
#set the y-limit of the barplot
ylim=c(0,20*2500)
#Barplot of FoodAvailability
png("Outputs/Test_Graphs/Malawi_2019/PFA_Barchart.png", height = 800, width = 1000)
par(cex=2)
barplot((dat$FAEnergyCropConsumption)[FSSorder], col="green", ylim=ylim, ylab="Food Availability [kcal per MAE per day]", border=NA)
off=(dat$FAEnergyCropConsumption)[FSSorder]
income_crop=(dat$FAEnergyCropSales)
barplot(income_crop[FSSorder], col="blue", offset=off,add=TRUE, border=NA)
off=off+income_crop[FSSorder]
barplot((dat$FAEnergyLivestockConsumption)[FSSorder], col="red", offset=off,add=TRUE, border=NA)
off=off+(dat$FAEnergyLivestockConsumption)[FSSorder]
income_lvst=(dat$FAEnergyLivestockSales)
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(dat$FAEnergyOffFarm)
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
#off=off+offfarm_w[FSSorder]
#barplot((dat$PotEnergyfromNTFP/dat$HHsizeMAE/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
abline(h=X,lty=2,lwd=3,col="red")
abline(h=Y,lty=2,lwd=3,col="blue")
#compute the percentage of food insecure household
FS.p=round(sum(dat$FoodAvailability<cal_line, na.rm=T)/length(dat$FoodAvailability)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("purple", "black", "orange", "red", "blue", "green",NA))
legend("topleft",legend=c("off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("black", "orange", "red", "blue", "green",NA))
title(paste0("Food Availability\n",Country," ", Year), cex.main=1.5)
dev.off()




FSSorder=order(TVA_pmae_pday)
#set the y-limit of the barplot
ylim=c(0,5)
#Barplot of TVA
png("Outputs/Test_Graphs/Malawi_2019/TVA_Barchart.png", height = 800, width = 1000)
par(cex=2)
barplot((dat$valuecropconsumed/dat$HHsizeMAE/365)[FSSorder], col="green", ylim=ylim, ylab="Total Value of Activities [$ per MAE per day]", border=NA)
off=(dat$valuecropconsumed/dat$HHsizeMAE/365)[FSSorder]
income_crop=(dat$cropsales)/dat$HHsizeMAE/365
barplot(income_crop[FSSorder], col="blue", offset=off,add=TRUE, border=NA)
off=off+income_crop[FSSorder]
barplot((dat$valuelivestockprodconsumed/dat$HHsizeMAE/365)[FSSorder], col="red", offset=off,add=TRUE, border=NA)
off=off+(dat$valuelivestockprodconsumed/dat$HHsizeMAE/365)[FSSorder]
income_lvst=(dat$livestockprodsales)/dat$HHsizeMAE/365
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(dat$offfarm_income)/dat$HHsizeMAE/365
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
#off=off+offfarm_w[FSSorder]
#barplot((dat$Total_income_NTFP/dat$HHsizeMAE/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
abline(h=X1,lty=2,lwd=3,col="red")
abline(h=Y1,lty=2,lwd=3,col="blue")
#compute the percentage of hh with TVA below poverty line
FS.p=round(sum(dat$TVA_pmae_pday<pov_line, na.rm=T)/length(TVA_pmae_pday)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "cons livestock","sold crop", "cons crop", paste(FS.p,"% below poverty line")), fill=c("purple", "black", "orange", "red", "blue", "green",NA))
legend("topleft",legend=c("off farm","sold lvst", "cons livestock","sold crop", "cons crop", paste(FS.p,"% below poverty line")), fill=c("black", "orange", "red", "blue", "green",NA))
title(paste0("Total Value of Activities\n", Country, " ", Year), cex.main=1.5)
dev.off()





FSSorder=order(Cash_inc)
#set the y-limit of the barplot
ylim=c(0,5)
#Barplot of FoodAvailability
png("Outputs/Test_Graphs/Malawi_2019/Cash_Income_Barchart.png", height = 800, width = 1000)
par(cex=2)
income_crop=(dat$cropsales)/dat$HHsizeMAE/365
barplot(income_crop[FSSorder], col="blue", ylim=ylim, ylab="Total Value of Activities [$ per MAE per day]", border=NA)
off=income_crop[FSSorder]
income_lvst=(dat$livestockprodsales)/dat$HHsizeMAE/365
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(dat$offfarm_income)/dat$HHsizeMAE/365
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
#off=off+offfarm_w[FSSorder]
#barplot((dat$Total_income_NTFP/dat$HHsizeMAE/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
#abline(h=X,lty=2,lwd=3,col="red")
abline(h=Y1,lty=2,lwd=3,col="blue")
#compute the percentage of food insecure household
FS.p=round(sum(Cash_inc<pov_line, na.rm=T)/length(Cash_inc)*100)
FS.p2=round(sum(Cash_inc==0 | is.na(Cash_inc))/length(Cash_inc)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "sold crop",paste(FS.p,"% below poverty line"), paste(FS.p2,"% with no cash income")), fill=c("purple", "black", "orange", "blue",NA,NA))
legend("topleft",legend=c("off farm","sold lvst", "sold crop",paste(FS.p,"% below poverty line"), paste(FS.p2,"% with no cash income")), fill=c("black", "orange", "blue",NA,NA))
title(paste0("Actual Cash Incomes\n", Country, " ", Year), cex.main=1.5)
dev.off()







