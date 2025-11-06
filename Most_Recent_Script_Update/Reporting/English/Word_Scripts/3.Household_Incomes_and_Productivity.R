#### TVA, Cash Income and Food Availability Graphs ####
dat$TVA_quartiles <- factor(dat$TVA_quartiles, levels=c("lowest","lower middle","upper middle","upper"))
dat$TVA_quartiles<-dat$TVA_quartiles                           # Adding FA groups to the data frame "dat"

FS1_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]
FS2_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]
FS3_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]
FS4_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]

TVA_pmae_pday<- dat$TVA_pmae_pday

convkcal_USD <- ifelse(as.numeric(dat$livestockprodsales)>0, as.numeric(dat$FAEnergyLivestockSales)/as.numeric(dat$livestockprodsales), #if a>0 the convkcal_USD= b/a
                       ifelse(as.numeric(dat$cropsales)>0, as.numeric(dat$FAEnergyCropSales)/as.numeric(dat$cropsales), NA))                   #otherwise its the next ifelse

convkcal_USD<-convkcal_USD[complete.cases(convkcal_USD)]                                            #finds which cases of convkcal_USD are complete
convkcal_USD <- rep(convkcal_USD[1], times=length(dat[,1]))                                         #Replaces the first value in convkcal_USD (length dat specifies the number of entries in the data)

# sure we don't need both of these - either cutoffs OR X and Y
cutoffs<-c(cal_line, convkcal_USD[1] * pov_line)                                                    #Creates a vector for the cutoffs, this includes cal_line and the first value of convkcal_US
X<- cal_line
Y<- convkcal_USD[1] * pov_line
X1 <- cal_line / convkcal_USD[1]
Y1 <- pov_line


#totE<-dat$PotFoodAvailIncludingNTFP
totE<-as.numeric(dat$FoodAvailability)
contrib_livestock_cons<-as.numeric(dat$FAEnergyLivestockConsumption)/totE
contrib_livestock_sold<-as.numeric(dat$FAEnergyLivestockSales)/totE
contrib_crops_cons<-as.numeric(dat$FAEnergyCropConsumption)/totE
contrib_crops_sold<-as.numeric(dat$FAEnergyCropSales)/totE
contrib_offfarm<-as.numeric(dat$FAEnergyOffFarm)/totE
#contrib_NTFP<-(as.numeric(dat$FAEnergy_NTFP_Consumed)/totE) + (as.numeric(dat$FAEnergy_NTFP_Sold)/totE)


Contr_FoodCrops_cons<-c(mean(contrib_crops_cons[FS1_index],na.rm=TRUE), mean(contrib_crops_cons[FS2_index],na.rm=TRUE), mean(contrib_crops_cons[FS3_index],na.rm=TRUE), mean(contrib_crops_cons[FS4_index],na.rm=TRUE))
Contr_FoodCrops_sold<-c(mean(contrib_crops_sold[FS1_index],na.rm=TRUE),mean(contrib_crops_sold[FS2_index],na.rm=TRUE),mean(contrib_crops_sold[FS3_index],na.rm=TRUE),mean(contrib_crops_sold[FS4_index],na.rm=TRUE))
Contr_Offfarm<-c(mean(contrib_offfarm[FS1_index],na.rm=TRUE),mean(contrib_offfarm[FS2_index],na.rm=TRUE),mean(contrib_offfarm[FS3_index],na.rm=TRUE),mean(contrib_offfarm[FS4_index],na.rm=TRUE))
Contr_Livestock_cons<-c(mean(contrib_livestock_cons[FS1_index],na.rm=TRUE),mean(contrib_livestock_cons[FS2_index],na.rm=TRUE),mean(contrib_livestock_cons[FS3_index],na.rm=TRUE),mean(contrib_livestock_cons[FS4_index],na.rm=TRUE))
Contr_Livestock_sold<-c(mean(contrib_livestock_sold[FS1_index],na.rm=TRUE),mean(contrib_livestock_sold[FS2_index],na.rm=TRUE),mean(contrib_livestock_sold[FS3_index],na.rm=TRUE),mean(contrib_livestock_sold[FS4_index],na.rm=TRUE))
#Contr_NTFP <- c(mean(contrib_NTFP[FS1_index],na.rm=TRUE),mean(contrib_NTFP[FS2_index],na.rm=TRUE),mean(contrib_NTFP[FS3_index],na.rm=TRUE),mean(contrib_NTFP[FS4_index],na.rm=TRUE)) #

#total<-Contr_FoodCrops_cons+Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_cons+Contr_Livestock_sold+Contr_NTFP
total<-Contr_FoodCrops_cons+Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_cons+Contr_Livestock_sold
Contr_FoodCrops_cons<-Contr_FoodCrops_cons/total
Contr_FoodCrops_sold<-Contr_FoodCrops_sold/total
Contr_Offfarm<-Contr_Offfarm/total
Contr_Livestock_cons<-Contr_Livestock_cons/total
Contr_Livestock_sold<-Contr_Livestock_sold/total
#Contr_NTFP<-Contr_NTFP/total # 

#temp<-data.frame(rbind(Contr_NTFP, Contr_Offfarm, Contr_Livestock_sold, Contr_Livestock_cons, Contr_FoodCrops_sold, Contr_FoodCrops_cons)*100)
temp<-data.frame(rbind(Contr_Offfarm, Contr_Livestock_sold, Contr_Livestock_cons, Contr_FoodCrops_sold, Contr_FoodCrops_cons)*100)
colnames(temp)<-levels(dat$TVA_quartiles)
rownames(temp)<-gsub("Contr_","",rownames(temp))
write.csv(temp, "Word_Outputs/3.Household_Incomes_and_Productivity/6.Prop_PFA_Table.csv")

Width<-length(contrib_crops_cons[FS1_index])/length(contrib_crops_cons)
Width<-c(Width,length(contrib_crops_cons[FS2_index])/length(contrib_crops_cons))
Width<-c(Width,length(contrib_crops_cons[FS3_index])/length(contrib_crops_cons))
Width<-c(Width,length(contrib_crops_cons[FS4_index])/length(contrib_crops_cons))

png("Word_Outputs/3.Household_Incomes_and_Productivity/6.Prop_PFA_Sources.png")
par(mfrow=c(1,1),mar=c(8,5,4,8), mgp=c(3,3,1))
barplot(100*Contr_FoodCrops_cons, width=Width,cex.axis=1.5,cex.names=1.2,names.arg =c('Lowest', 'Lower\nMiddle', 'Upper\nMiddle', 'Upper'), col='green',border=TRUE, ylim=c(0,100), main="Calorie Supply in %" , xlab="")
off<-100*Contr_FoodCrops_cons
barplot(100*Contr_FoodCrops_sold, width=Width, cex.axis=1.5,col="blue", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_FoodCrops_sold
barplot(100*Contr_Livestock_cons, width=Width, cex.axis=1.5,col="orange", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Livestock_cons
barplot(100*Contr_Livestock_sold, width=Width, cex.axis=1.5,col="red", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Livestock_sold
barplot(100*Contr_Offfarm, width=Width, cex.axis=1.5,col="black", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Offfarm
#barplot(100*Contr_NTFP, width=Width, cex.axis=1.5,col="purple", offset=off,add=TRUE, border=TRUE)
#legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('purple','black','red','orange','blue','green'),c('NTFPs', 'Off-Farm income','Livestock sold','Livestock cons','Food Crops sold','Food Crops cons'),bg='white')
legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('black','red','orange','blue','green'),c('Off-Farm income','Livestock sold','Livestock cons','Food Crops sold','Food Crops cons'),bg='white')
dev.off()

#### Add in Absolute values calorie values too (code from Panel Survey)

#### Source each chart in an individual script as it will then be easier to turn on or off as needed.

#----------------
#FA distribution graph
#----------------

FSSorder=order(as.numeric(dat$FoodAvailability))
#set the y-limit of the barplot
ylim=c(0,20*2500)
#Barplot of FoodAvailability
png("Word_Outputs/3.Household_Incomes_and_Productivity/3.PFA_Barchart.png", height = 800, width = 1000)
par(cex=2)
barplot((as.numeric(dat$FAEnergyCropConsumption))[FSSorder], col="green", ylim=ylim, ylab="Food Availability [kcal per MAE per day]", border=NA)
off=(as.numeric(dat$FAEnergyCropConsumption))[FSSorder]
income_crop=(as.numeric(dat$FAEnergyCropSales))
barplot(income_crop[FSSorder], col="blue", offset=off,add=TRUE, border=NA)
off=off+income_crop[FSSorder]
barplot((as.numeric(dat$FAEnergyLivestockConsumption))[FSSorder], col="red", offset=off,add=TRUE, border=NA)
off=off+(as.numeric(dat$FAEnergyLivestockConsumption))[FSSorder]
income_lvst=(as.numeric(dat$FAEnergyLivestockSales))
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(as.numeric(dat$FAEnergyOffFarm))
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
off=off+offfarm_w[FSSorder]
#barplot(rowSums(data.frame(as.numeric(dat$FAEnergy_NTFP_Consumed),as.numeric(dat$FAEnergy_NTFP_Sold)), na.rm=T)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
abline(h=X,lty=2,lwd=3,col="red")
abline(h=Y,lty=2,lwd=3,col="blue")
#compute the percentage of food insecure household
FS.p=round(sum(as.numeric(dat$FoodAvailability)<cal_line, na.rm=T)/length(dat$FoodAvailability)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("purple", "black", "orange", "red", "blue", "green",NA))
legend("topleft",legend=c("off farm","sold lvst", "cons livestock", "sold crop", "cons crop", paste(FS.p, "% below calorie line")), fill=c("black", "orange", "red", "blue", "green",NA))

title(paste0('Food Availability\n', Country, "_", Year), cex.main=1.5)
dev.off()

########## Income Measured in TVA ##############

#contribution graphs by quartile
FS1_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]
FS2_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]
FS3_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]
FS4_index<-dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]

totE<-TVA_pmae_pday
totE[which(totE==0)]<-NA
contrib_livestock_cons<-as.numeric(dat$valuelivestockprodconsumed)/totE
contrib_livestock_sold<-as.numeric(dat$livestockprodsales)/totE
contrib_crops_cons<-as.numeric(dat$valuecropconsumed)/totE
contrib_crops_sold<-as.numeric(dat$cropsales)/totE
contrib_offfarm<-as.numeric(dat$offfarm_income)/totE
#contrib_NTFP<-(as.numeric(dat$NTFP_Income)+as.numeric(dat$value_NTFP_consumed))/totE  


Contr_FoodCrops_cons<-c(mean(contrib_crops_cons[FS1_index],na.rm=TRUE), mean(contrib_crops_cons[FS2_index],na.rm=TRUE), mean(contrib_crops_cons[FS3_index],na.rm=TRUE), mean(contrib_crops_cons[FS4_index],na.rm=TRUE))
Contr_FoodCrops_sold<-c(mean(contrib_crops_sold[FS1_index],na.rm=TRUE),mean(contrib_crops_sold[FS2_index],na.rm=TRUE),mean(contrib_crops_sold[FS3_index],na.rm=TRUE),mean(contrib_crops_sold[FS4_index],na.rm=TRUE))
Contr_Offfarm<-c(mean(contrib_offfarm[FS1_index],na.rm=TRUE),mean(contrib_offfarm[FS2_index],na.rm=TRUE),mean(contrib_offfarm[FS3_index],na.rm=TRUE),mean(contrib_offfarm[FS4_index],na.rm=TRUE))
Contr_Livestock_cons<-c(mean(contrib_livestock_cons[FS1_index],na.rm=TRUE),mean(contrib_livestock_cons[FS2_index],na.rm=TRUE),mean(contrib_livestock_cons[FS3_index],na.rm=TRUE),mean(contrib_livestock_cons[FS4_index],na.rm=TRUE))
Contr_Livestock_sold<-c(mean(contrib_livestock_sold[FS1_index],na.rm=TRUE),mean(contrib_livestock_sold[FS2_index],na.rm=TRUE),mean(contrib_livestock_sold[FS3_index],na.rm=TRUE),mean(contrib_livestock_sold[FS4_index],na.rm=TRUE))
#Contr_NTFP <- c(mean(contrib_NTFP[FS1_index],na.rm=TRUE),mean(contrib_NTFP[FS2_index],na.rm=TRUE),mean(contrib_NTFP[FS3_index],na.rm=TRUE),mean(contrib_NTFP[FS4_index],na.rm=TRUE)) #

#total<-Contr_FoodCrops_cons+Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_cons+Contr_Livestock_sold+Contr_NTFP
total<-Contr_FoodCrops_cons+Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_cons+Contr_Livestock_sold
total[total=="NaN"]<-NA
Contr_FoodCrops_cons<-Contr_FoodCrops_cons/total
Contr_FoodCrops_sold<-Contr_FoodCrops_sold/total
Contr_Offfarm<-Contr_Offfarm/total
Contr_Livestock_cons<-Contr_Livestock_cons/total
Contr_Livestock_sold<-Contr_Livestock_sold/total
#Contr_NTFP<-Contr_NTFP/total # 

#temp<-data.frame(rbind(Contr_NTFP, Contr_Offfarm, Contr_Livestock_sold, Contr_Livestock_cons, Contr_FoodCrops_sold, Contr_FoodCrops_cons)*100)
temp<-data.frame(rbind(Contr_Offfarm, Contr_Livestock_sold, Contr_Livestock_cons, Contr_FoodCrops_sold, Contr_FoodCrops_cons)*100)
colnames(temp)<-levels(dat$TVA_quartiles)
rownames(temp)<-gsub("Contr_","",rownames(temp))
write.csv(temp, "Word_Outputs/3.Household_Incomes_and_Productivity/5.Prop_TVA_Table.csv")

Width<-length(contrib_crops_cons[FS1_index])/length(contrib_crops_cons)
Width<-c(Width,length(contrib_crops_cons[FS2_index])/length(contrib_crops_cons))
Width<-c(Width,length(contrib_crops_cons[FS3_index])/length(contrib_crops_cons))
Width<-c(Width,length(contrib_crops_cons[FS4_index])/length(contrib_crops_cons))

png("Word_Outputs/3.Household_Incomes_and_Productivity/5.Prop_TVA_Sources.png")
par(mfrow=c(1,1),mar=c(8,5,4,8), mgp=c(3,3,1))
barplot(100*Contr_FoodCrops_cons, width=Width,cex.axis=1.5,cex.names=1.2,names.arg =c('Lowest', 'Lower\nMiddle', 'Upper\nMiddle', 'Upper'), col='green',border=TRUE, ylim=c(0,100), main="Total Value of Activities in  %" , xlab="")
off<-100*Contr_FoodCrops_cons
barplot(100*Contr_FoodCrops_sold, width=Width, cex.axis=1.5,col="blue", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_FoodCrops_sold
barplot(100*Contr_Livestock_cons, width=Width, cex.axis=1.5,col="orange", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Livestock_cons
barplot(100*Contr_Livestock_sold, width=Width, cex.axis=1.5,col="red", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Livestock_sold
barplot(100*Contr_Offfarm, width=Width, cex.axis=1.5,col="black", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Offfarm
#barplot(100*Contr_NTFP, width=Width, cex.axis=1.5,col="purple", offset=off,add=TRUE, border=TRUE)
#legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('purple','black','red','orange','blue','green'),c('NTFPs', 'Off-Farm income','Livestock sold','Livestock cons','Food Crops sold','Food Crops cons'),bg='white')
legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('black','red','orange','blue','green'),c('Off-Farm income','Livestock sold','Livestock cons','Food Crops sold','Food Crops cons'),bg='white')
dev.off()


#### Total Value of Activities version

#Order households per Food Security ratio
FSSorder=order(TVA_pmae_pday)
#set the y-limit of the barplot
ylim=c(0,5)
#Barplot of TVA
png("Word_Outputs/3.Household_Incomes_and_Productivity/1.TVA_Barchart.png", height = 800, width = 1000)
par(cex=2)
barplot((as.numeric(dat$valuecropconsumed)/as.numeric(dat$HHsizeMAE)/365)[FSSorder], col="green", ylim=ylim, ylab="Total Value of Activities [$ per MAE per day]", border=NA)
off=(as.numeric(dat$valuecropconsumed)/as.numeric(dat$HHsizeMAE)/365)[FSSorder]
income_crop=(as.numeric(dat$cropsales))/as.numeric(dat$HHsizeMAE)/365
barplot(income_crop[FSSorder], col="blue", offset=off,add=TRUE, border=NA)
off=off+income_crop[FSSorder]
barplot((as.numeric(dat$valuelivestockprodconsumed)/as.numeric(dat$HHsizeMAE)/365)[FSSorder], col="red", offset=off,add=TRUE, border=NA)
off=off+(as.numeric(dat$valuelivestockprodconsumed)/as.numeric(dat$HHsizeMAE)/365)[FSSorder]
income_lvst=(as.numeric(dat$livestockprodsales))/as.numeric(dat$HHsizeMAE)/365
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(as.numeric(dat$offfarm_income))/as.numeric(dat$HHsizeMAE)/365
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
off=off+offfarm_w[FSSorder]
#barplot(((as.numeric(dat$NTFP_Income)+as.numeric(dat$value_NTFP_consumed))/as.numeric(dat$HHsizeMAE)/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
abline(h=X1,lty=2,lwd=3,col="red")
abline(h=Y1,lty=2,lwd=3,col="blue")
#compute the percentage of hh with TVA below poverty line
FS.p=round(sum(TVA_pmae_pday<pov_line, na.rm=T)/length(TVA_pmae_pday)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "cons livestock","sold crop", "cons crop", paste(FS.p,"% below poverty line")), fill=c("purple", "black", "orange", "red", "blue", "green",NA))
legend("topleft",legend=c("off farm","sold lvst", "cons livestock","sold crop", "cons crop", paste(FS.p,"% below poverty line")), fill=c("black", "orange", "red", "blue", "green",NA))
title(paste0("Total Value of Activities\n", Country, " ", Year), cex.main=1.5)
dev.off()


#### Cash Income version ####

#Cash_inc <-  rowSums(data.frame(as.numeric(dat$cropsales), as.numeric(dat$livestockprodsales), as.numeric(dat$offfarm_income), as.numeric(dat$NTFP_Income)), na.rm=T)/as.numeric(dat$HHsizeMAE)/365
Cash_inc <-  rowSums(data.frame(dat$cropsales, dat$livestockprodsales, dat$offfarm_income), na.rm=T)/dat$HHsizeMAE/365



#Order households per Food Security ratio
FSSorder=order(Cash_inc)
#set the y-limit of the barplot
ylim=c(0,5)
#Barplot of FoodAvailability
png("Word_Outputs/3.Household_Incomes_and_Productivity/2.Cash_Income_Barchart.png", height = 800, width = 1000)
par(cex=2)
income_crop=(as.numeric(dat$cropsales))/as.numeric(dat$HHsizeMAE)/365
barplot(income_crop[FSSorder], col="blue", ylim=ylim, ylab="Cash Income ($/MAE/day)", border=NA)
off=income_crop[FSSorder]
income_lvst=(as.numeric(dat$livestockprodsales))/as.numeric(dat$HHsizeMAE)/365
barplot(income_lvst[FSSorder], col="orange", offset=off,add=TRUE, border=NA)
off=off+income_lvst[FSSorder]
offfarm_w=(as.numeric(dat$offfarm_income))/as.numeric(dat$HHsizeMAE)/365
barplot(offfarm_w[FSSorder], col="black", offset=off,add=TRUE, border=NA)
off=off+offfarm_w[FSSorder]
#barplot((as.numeric(dat$NTFP_Income)/as.numeric(dat$HHsizeMAE)/365)[FSSorder], col="purple", offset=off,add=TRUE, border=NA)
abline(h=X,lty=2,lwd=3,col="red")
abline(h=Y1,lty=2,lwd=3,col="blue")
#compute the percentage of food insecure household
FS.p=round(sum(Cash_inc<pov_line, na.rm=T)/length(Cash_inc)*100)
FS.p2=round(sum(Cash_inc==0 | is.na(Cash_inc))/length(Cash_inc)*100)
#legend("topleft",legend=c("NTFP", "off farm","sold lvst", "sold crop",paste(FS.p,"% below poverty line"), paste(FS.p2,"% with no cash income")), fill=c("purple", "black", "orange", "blue",NA,NA))
legend("topleft",legend=c("off farm","sold lvst", "sold crop",paste(FS.p,"% below poverty line"), paste(FS.p2,"% with no cash income")), fill=c("black", "orange", "blue",NA,NA))
title(paste0("Actual Cash Incomes\n", Country, " ", Year), cex.main=1.5)
dev.off()



totE<-as.numeric(dat$total_income)
contrib_livestock_sold<-as.numeric(dat$livestockprodsales)/totE
contrib_crops_sold<-as.numeric(dat$cropsales)/totE
contrib_offfarm<-as.numeric(dat$offfarm_income)/totE
#contrib_NTFP<- (as.numeric(dat$NTFP_Income)/totE)


Contr_FoodCrops_sold<-c(mean(contrib_crops_sold[FS1_index],na.rm=TRUE),mean(contrib_crops_sold[FS2_index],na.rm=TRUE),mean(contrib_crops_sold[FS3_index],na.rm=TRUE),mean(contrib_crops_sold[FS4_index],na.rm=TRUE))
Contr_Offfarm<-c(mean(contrib_offfarm[FS1_index],na.rm=TRUE),mean(contrib_offfarm[FS2_index],na.rm=TRUE),mean(contrib_offfarm[FS3_index],na.rm=TRUE),mean(contrib_offfarm[FS4_index],na.rm=TRUE))
Contr_Livestock_sold<-c(mean(contrib_livestock_sold[FS1_index],na.rm=TRUE),mean(contrib_livestock_sold[FS2_index],na.rm=TRUE),mean(contrib_livestock_sold[FS3_index],na.rm=TRUE),mean(contrib_livestock_sold[FS4_index],na.rm=TRUE))
#Contr_NTFP <- c(mean(contrib_NTFP[FS1_index],na.rm=TRUE),mean(contrib_NTFP[FS2_index],na.rm=TRUE),mean(contrib_NTFP[FS3_index],na.rm=TRUE),mean(contrib_NTFP[FS4_index],na.rm=TRUE)) #

#total<-Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_sold+Contr_NTFP
total<-Contr_FoodCrops_sold+Contr_Offfarm+Contr_Livestock_sold
Contr_FoodCrops_sold<-Contr_FoodCrops_sold/total
Contr_Offfarm<-Contr_Offfarm/total
Contr_Livestock_sold<-Contr_Livestock_sold/total
#Contr_NTFP<-Contr_NTFP/total # 

#temp<-data.frame(rbind(Contr_NTFP, Contr_Offfarm, Contr_Livestock_sold, Contr_FoodCrops_sold)*100)
temp<-data.frame(rbind(Contr_Offfarm, Contr_Livestock_sold, Contr_FoodCrops_sold)*100)
colnames(temp)<-levels(dat$TVA_quartiles)
rownames(temp)<-gsub("Contr_","",rownames(temp))
write.csv(temp, "Word_Outputs/3.Household_Incomes_and_Productivity/7.Prop_Cash_Income_Table.csv")

Width<-length(Contr_FoodCrops_sold[FS1_index])/length(Contr_FoodCrops_sold)
Width<-c(Width,length(Contr_FoodCrops_sold[FS2_index])/length(Contr_FoodCrops_sold))
Width<-c(Width,length(Contr_FoodCrops_sold[FS3_index])/length(Contr_FoodCrops_sold))
Width<-c(Width,length(Contr_FoodCrops_sold[FS4_index])/length(Contr_FoodCrops_sold))

png("Word_Outputs/3.Household_Incomes_and_Productivity/7.Prop_Cash_Sources.png")
par(mfrow=c(1,1),mar=c(8,5,4,8), mgp=c(3,3,1))
barplot(100*Contr_FoodCrops_cons, width=Width,cex.axis=1.5,cex.names=1.2,names.arg =c('Lowest', 'Lower\nMiddle', 'Upper\nMiddle', 'Upper'), col='green',border=TRUE, ylim=c(0,100), main="Total Value of Activities in  %" , xlab="")
off<-100*Contr_FoodCrops_cons
barplot(100*Contr_FoodCrops_sold, width=Width, cex.axis=1.5,cex.names=1.2,col="blue",names.arg =c('Lowest', 'Lower\nMiddle', 'Upper\nMiddle', 'Upper'), ylim=c(0,100),main="Cash Income (PPP$)  %" , xlab="", border=TRUE)
off<-100*Contr_FoodCrops_sold
barplot(100*Contr_Livestock_sold, width=Width, cex.axis=1.5,col="red", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Livestock_sold
barplot(100*Contr_Offfarm, width=Width, cex.axis=1.5,col="black", offset=off,add=TRUE, border=TRUE)
off<-off+100*Contr_Offfarm
#barplot(100*Contr_NTFP, width=Width, cex.axis=1.5,col="green", offset=off,add=TRUE, border=TRUE)
#legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('green','black','red','blue'),c('NTFPs', 'Off-Farm income','Livestock sold','Food Crops sold'),bg='white')
legend(x=1.22, y=75, xpd = T, pch=21, col = c('black','black','black','black','black'),pt.bg= c('black','red','orange','blue','green'),c('Off-Farm income','Livestock sold','Livestock cons','Food Crops sold','Food Crops cons'),bg='white')
dev.off()


#### Quartile Summary Graphs ####
dat_grp$FoodAvailability<- as.numeric(dat_grp$FoodAvailability)
dat$FoodAvailability<-as.numeric(dat$FoodAvailability)

Population_by_Quartile<- summarise(dat_grp, 
                                   round(median(TVA_pmae_pday, na.rm=T),2), 
                                            round(median(FoodAvailability, na.rm=T)))

Population_by_Quartile<-Population_by_Quartile[!is.na(Population_by_Quartile$TVA_quartiles),]
colnames(Population_by_Quartile) <- c("Quartiles", 
                                   "Median Total Value of Activities ($/person/day)", 
                                   "Median Food Availability  (kcal/person/day)") 
# whole_pop<- c("All", 
#               round(median(dat$TVA_pmae_pday, na.rm=T),2), 
#               round(median(dat$FoodAvailability, na.rm=T))
# )
# Population_by_Quartile$Quartiles<- as.character(Population_by_Quartile$Quartiles)
# Population_by_Quartile<- rbind(Population_by_Quartile, whole_pop)
write.csv(Population_by_Quartile, "Word_Outputs/3.Household_Incomes_and_Productivity/4.Population_by_Quartile.csv", row.names = F)

#### Village Quartile Summaries ####



village_summary<-data.frame(table(dat$Village))
colnames(village_summary)<-c("Village", "Number_of_HHs")
village_summary<-data.frame("Village"=village_summary$Village, "Number_of_HHs"=village_summary$Number_of_HHs)
write.csv(village_summary, "Word_Outputs/3.Household_Incomes_and_Productivity/8.Village_Summary.csv")

dat$TVA_quartiles<-factor(dat$TVA_quartiles, levels=c("lowest", "lower middle", "upper middle", "upper"))

png("Word_Outputs/3.Household_Incomes_and_Productivity/9.Village_Quartile_Summary.png")
par(mar=c(10,6,3,1))
barplot(table(dat$TVA_quartiles, dat$Village), 
        las=2, 
        main="Wealth Quartiles per Village", 
        ylab = "Number of Households", 
        col=c("darkred", "orange","darkgreen","darkblue" ),
        legend.text = TRUE,
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.02, 0)))

dev.off()

write.csv(table(dat$TVA_quartiles, dat$Village), "Word_Outputs/3.Household_Incomes_and_Productivity/9.Village_Quartiles_Summary.csv")
#------
#----------------

png("Word_Outputs/3.Household_Incomes_and_Productivity/10.Incomes_by_Quartile.png")
par(mfrow=c(1,1),mgp=c(3,2,0), mar=c(8,3,3,3))
boxplot(list((dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]/dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]/365), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]/dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]/365), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]/dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]/365), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]]/dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]]/365)),
          #   (dat$total_income/dat$HHsizemembers/365)), 
        outline = F,  col = "lightgray", ylab="US $", main="Income in $ per person per day", names=c("Lower", "Lower-Middle", "Upper-Middle", "Upper"), las=2) 
dev.off()




#---------------------------------------------------------
Incomes_By_Quartile<- summarise(dat_grp, 
                                   round(median(total_income/365/HHsizemembers, na.rm=T),2), 
                                   round(IQR(total_income/365/HHsizemembers, na.rm=T),2))

Incomes_By_Quartile<-Incomes_By_Quartile[!is.na(Incomes_By_Quartile$TVA_quartiles),]
colnames(Incomes_By_Quartile) <- c("Quartiles", 
                                      "Median Total Cash Income ($/person/day)", 
                                      "IQR Total Cash Income ($/person/day)") 
# whole_pop<- c("All", 
#               round(median(dat$total_income/365/dat$HHsizemembers, na.rm=T),2), 
#               round(IQR(dat$total_income/365/dat$HHsizemembers, na.rm=T),2)
# )
# Incomes_By_Quartile$Quartiles<- as.character(Incomes_By_Quartile$Quartiles)
# Incomes_By_Quartile<- rbind(Incomes_By_Quartile, whole_pop)
write.csv(Incomes_By_Quartile, "Word_Outputs/3.Household_Incomes_and_Productivity/10.Incomes_By_Quartile.csv", row.names = F)
#---------------------------------------------------------


#### Average cash incomes from different sources ($ per household per year)####

png("Word_Outputs/3.Household_Incomes_and_Productivity/11.Income_Breakdown.png")
par(mfrow=c(2,2),mgp=c(3,2,0))

boxplot(list((dat$cropsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$cropsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$cropsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$cropsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$cropsales)), 
        outline = F,  col = "lightgray",  main="Crop Income (US $)", names=c("low", "low-mid", "up-mid", "upper"), las=2) 

boxplot(list((dat$livestockprodsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$livestockprodsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$livestockprodsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$livestockprodsales[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$livestockprodsales)), 
        outline = F,  col = "lightgray",  main="Livestock Income (US $)", names=c("low", "low-mid", "up-mid", "upper"), las=2) 

boxplot(list((dat$offfarm_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$offfarm_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$offfarm_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$offfarm_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$offfarm_income)), 
        outline = F,  col = "lightgray",  main="Off-farm Income (US $)", names=c("low", "low-mid", "up-mid", "upper"), las=2) 

# boxplot(list((dat$NTFP_Income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
#              (dat$NTFP_Income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
#              (dat$NTFP_Income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
#              (dat$NTFP_Income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]]),
#              (dat$NTFP_Income)), 
#         outline = F,  col = "lightgray",  main="NTFP Income (US $)", names=c("low", "low-mid", "up-mid", "upper", "all"), las=2) 
dev.off()


#####---------

Income_Break_down<- summarise(dat_grp, 
                              round(median(cropsales, na.rm=T),2), 
                              round(IQR(cropsales, na.rm=T),2),
                              round(median(livestockprodsales, na.rm=T),2), 
                              round(IQR(livestockprodsales, na.rm=T),2),
                              round(median(offfarm_income, na.rm=T),2), 
                              round(IQR(offfarm_income, na.rm=T),2)
                             # round(median(NTFP_Income, na.rm=T),2),
                             # round(IQR(NTFP_Income, na.rm=T),2)
)

Income_Break_down<-Income_Break_down[!is.na(Income_Break_down$TVA_quartiles),]
colnames(Income_Break_down) <- c("Quartiles", 
                                 "Crop Sales (Median)", 
                                 "Crop Sales (IQR)",
                                 "Livestock Sales (Median)", 
                                 "Livestock Sales (IQR)",
                                 "Off-farm Income (Median)", 
                                 "Off-farm Income (IQR)"
                                 #"NTFP Sales (Median)", 
                                 #"NTFP Sales (IQR)") 
)
# whole_pop<- c("All", 
#               round(median(dat$cropsales, na.rm=T),2), 
#               round(IQR(dat$cropsales, na.rm=T),2),
#               round(median(dat$livestockprodsales, na.rm=T),2), 
#               round(IQR(dat$livestockprodsales, na.rm=T),2),
#               round(median(dat$offfarm_income, na.rm=T),2), 
#               round(IQR(dat$offfarm_income, na.rm=T),2)
#               #round(median(dat$NTFP_Income, na.rm=T),2), 
#               #round(IQR(dat$NTFP_Income, na.rm=T),2)
# )
# 
# 
# Income_Break_down$Quartiles<- as.character(Income_Break_down$Quartiles)
# Income_Break_down<- rbind(Income_Break_down, whole_pop)

Income_Break_down<- data.frame(t(Income_Break_down))
colnames(Income_Break_down)<- c("lowest", "lower middle", "upper middle", "upper")
Income_Break_down<-Income_Break_down[-which(row.names(Income_Break_down)=="Quartiles"),]
Income_Break_down<-Income_Break_down[-grep("IQR",row.names(Income_Break_down)),]

temp_row_names<- row.names(Income_Break_down)
Income_Break_down<-data.frame(lapply(Income_Break_down, function (x) 100*round(as.numeric(as.character(x))/sum(as.numeric(as.character(x)),na.rm = T),2)))
row.names(Income_Break_down)<- temp_row_names

write.csv(Income_Break_down, "Word_Outputs/3.Household_Incomes_and_Productivity/11.Income_Break_down_table.csv")

#####---------

dat_grp$FoodAvailability<-as.numeric(dat_grp$FoodAvailability)
dat_grp$FAEnergyCropSales<-as.numeric(dat_grp$FAEnergyCropSales)
dat_grp$FAEnergyCropConsumption<-as.numeric(dat_grp$FAEnergyCropConsumption)
dat_grp$FAEnergyLivestockConsumption<-as.numeric(dat_grp$FAEnergyLivestockConsumption)
dat_grp$FAEnergyLivestockSales<-as.numeric(dat_grp$FAEnergyLivestockSales)
dat_grp$FAEnergyOffFarm<-as.numeric(dat_grp$FAEnergyOffFarm)
#dat_grp$FAEnergy_NTFP_Consumed<-as.numeric(dat_grp$FAEnergy_NTFP_Consumed)
#dat_grp$FAEnergy_NTFP_Sold<-as.numeric(dat_grp$FAEnergy_NTFP_Sold)


Food_Availability_Break_down<- summarise(dat_grp, 
                                round(median(FAEnergyCropSales, na.rm=T),2), 
                                round(IQR(FAEnergyCropSales, na.rm=T),2),
                                round(median(FAEnergyCropConsumption, na.rm=T),2), 
                                round(IQR(FAEnergyCropConsumption, na.rm=T),2),
                                round(median(FAEnergyLivestockSales, na.rm=T),2), 
                                round(IQR(FAEnergyLivestockSales, na.rm=T),2),
                                round(median(FAEnergyLivestockConsumption, na.rm=T),2), 
                                round(IQR(FAEnergyLivestockConsumption, na.rm=T),2),
                                round(median(FAEnergyOffFarm, na.rm=T),2), 
                                round(IQR(FAEnergyOffFarm, na.rm=T),2)
                               # round(median(FAEnergy_NTFP_Sold, na.rm=T),2), 
                                #round(IQR(FAEnergy_NTFP_Sold, na.rm=T),2),
                                #round(median(FAEnergy_NTFP_Consumed, na.rm=T),2), 
                                #round(IQR(FAEnergy_NTFP_Consumed, na.rm=T),2)
                          
                                )

Food_Availability_Break_down<-Food_Availability_Break_down[!is.na(Food_Availability_Break_down$TVA_quartiles),]
colnames(Food_Availability_Break_down) <- c("Quartiles", 
                                   "Crop Sales (Median)", 
                                   "Crop Sales (IQR)",
                                   "Crop Consumption (Median)", 
                                   "Crop Consumption (IQR)",
                                   "Livestock Sales (Median)", 
                                   "Livestock Sales (IQR)",
                                 "Livestock Consumption (Median)", 
                                 "Livestock Consumption (IQR)", 
                                 "Off-Farm Income (Median)", 
                                 "Off-Farm Income (IQR)")
                                 # "NTFP Sales (Median)", 
                                 # "NTFP Sales (IQR)", 
                                 # "NTFP Consumption (Median)", 
                                 # "NTFP Consumption (IQR)") 
                                 
                                 
         
dat$FoodAvailability<-as.numeric(dat$FoodAvailability)
dat$FAEnergyCropSales<-as.numeric(dat$FAEnergyCropSales)
dat$FAEnergyCropConsumption<-as.numeric(dat$FAEnergyCropConsumption)
dat$FAEnergyLivestockConsumption<-as.numeric(dat$FAEnergyLivestockConsumption)
dat$FAEnergyLivestockSales<-as.numeric(dat$FAEnergyLivestockSales)
dat$FAEnergyOffFarm<-as.numeric(dat$FAEnergyOffFarm)
#dat$FAEnergy_NTFP_Consumed<-as.numeric(dat$FAEnergy_NTFP_Consumed)
#dat$FAEnergy_NTFP_Sold<-as.numeric(dat$FAEnergy_NTFP_Sold)                          
# whole_pop<- c("All", 
#               round(median(dat$FAEnergyCropSales, na.rm=T),2), 
#               round(IQR(dat$FAEnergyCropSales, na.rm=T),2),
#               round(median(dat$FAEnergyCropConsumption, na.rm=T),2), 
#               round(IQR(dat$FAEnergyCropConsumption, na.rm=T),2),
#               round(median(dat$FAEnergyLivestockSales, na.rm=T),2), 
#               round(IQR(dat$FAEnergyLivestockSales, na.rm=T),2),
#               round(median(dat$FAEnergyLivestockConsumption, na.rm=T),2), 
#               round(IQR(dat$FAEnergyLivestockConsumption, na.rm=T),2),
#               round(median(dat$FAEnergyOffFarm, na.rm=T),2), 
#               round(IQR(dat$FAEnergyOffFarm, na.rm=T),2),
#               round(median(dat$FAEnergy_NTFP_Sold, na.rm=T),2), 
#               round(IQR(dat$FAEnergy_NTFP_Sold, na.rm=T),2),
#               round(median(dat$FAEnergy_NTFP_Consumed, na.rm=T),2), 
#               round(IQR(dat$FAEnergy_NTFP_Consumed, na.rm=T),2)
# )
#
#
# Food_Availability_Break_down$Quartiles<- as.character(Food_Availability_Break_down$Quartiles)
# Food_Availability_Break_down<- rbind(Food_Availability_Break_down, whole_pop)

Food_Availability_Break_down<- data.frame(t(Food_Availability_Break_down))
colnames(Food_Availability_Break_down)<- c("lowest", "lower middle", "upper middle", "upper")
Food_Availability_Break_down<-Food_Availability_Break_down[-which(row.names(Food_Availability_Break_down)=="Quartiles"),]
Food_Availability_Break_down<-Food_Availability_Break_down[-grep("IQR",row.names(Food_Availability_Break_down)),]

temp_row_names<- row.names(Food_Availability_Break_down)
Food_Availability_Break_down<-data.frame(lapply(Food_Availability_Break_down, function (x) 100*round(as.numeric(as.character(x))/sum(as.numeric(as.character(x)),na.rm = T),2)))
row.names(Food_Availability_Break_down)<- temp_row_names

write.csv(Food_Availability_Break_down, "Word_Outputs/3.Household_Incomes_and_Productivity/12.Food_Availability_Break_down.csv")


#### --------



#####---------
png("Word_Outputs/3.Household_Incomes_and_Productivity/13.Total_income_by_Quartile.png")
par(mfrow=c(1,1),mgp=c(3,2,0))
boxplot(list((dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$total_income[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]]),
             (dat$total_income)), 
        outline = F,  col = "lightgray", ylab="US $", main="Income in $ per HH per Year", names=c("Lower", "Lower-Middle", "Upper-Middle", "Upper", "All"), las=2) 
dev.off()






Total_Income_by_Quartile<- summarise(dat_grp, 
                                round(median(total_income, na.rm=T),2), 
                                round(IQR(total_income, na.rm=T),2))

Total_Income_by_Quartile<-Total_Income_by_Quartile[!is.na(Total_Income_by_Quartile$TVA_quartiles),]
colnames(Total_Income_by_Quartile) <- c("Quartiles", 
                                   "Median Total Cash Income", 
                                   "IQR Total Cash Income") 
# whole_pop<- c("All",
#               round(median(dat$total_income, na.rm=T),2),
#               round(IQR(dat$total_income, na.rm=T),2)
# )
# Total_Income_by_Quartile$Quartiles<- as.character(Total_Income_by_Quartile$Quartiles)
# Total_Income_by_Quartile<- rbind(Total_Income_by_Quartile, whole_pop)
write.csv(Total_Income_by_Quartile, "Word_Outputs/3.Household_Incomes_and_Productivity/13.Total_Income_by_Quartile.csv", row.names = F)





