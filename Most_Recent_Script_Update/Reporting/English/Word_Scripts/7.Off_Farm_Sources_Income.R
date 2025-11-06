temp<-data.frame(prop.table(table(dat_all$offfarm_incomes_any)))

png("Word_Outputs/7.Off_Farm_Sources_Income/1.Off_Farm_Incomes.png", height = 1000, width = 1200)
barplot(temp$Freq*100, main = "Any Off-Farm Incomes", names.arg = temp$Var1, ylab = "Percentage of HHs")
dev.off()

png("Word_Outputs/7.Off_Farm_Sources_Income/2.Off_farm_hist.png",height = 600, width = 1200)
par(mfrow=c(1,1),cex=1.6)
h=hist(dat$offfarm_income[dat$offfarm_income<500], plot=F)
h$density = h$counts/nrow(dat)*100
plot(h,freq=FALSE, main="Off-farm Income", xlab="$/HH/Year", col="lightgray", ylab="% of households")

#hist(dat$LandOwned[FA_groups==levels(FA_groups)[1]], main=levels(FA_groups)[1], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[2]], main=levels(FA_groups)[2], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[3]], main=levels(FA_groups)[3], xlab=NULL, col="lightgray")
dev.off()

off_farm_income_table<-data.frame(rbind(round(h$mids),round(h$density,2)), row.names = c("Off-farm Income $", "Off-farm Income % hh"))
colnames(off_farm_income_table)<-NULL

write.table(off_farm_income_table, file = "Word_Outputs/7.Off_Farm_Sources_Income/2.Off_Farm_Incomes_table.csv",sep=",", col.names = F)






off_farm_summary<-List_to_True_False(dat_all$offfarm_incomes)
off_farm_summary<-data.frame(colSums(off_farm_summary))
off_farm_summary[,1]<-round(100*off_farm_summary[,1]/nrow(dat_all),1)
off_farm_summary<-data.frame("Off-farm Incomes"=row.names(off_farm_summary), "Percentage of HHs"=off_farm_summary[,1]) 
off_farm_summary<-off_farm_summary[order(off_farm_summary[,2], decreasing = TRUE),]
off_farm_summary<-off_farm_summary[-grep("other", off_farm_summary$Off.farm.Incomes),]

write.csv(off_farm_summary, "Word_Outputs/7.Off_Farm_Sources_Income/4.Off_Farm_Incomes_sources.csv", row.names = F)


png("Word_Outputs/7.Off_Farm_Sources_Income/4.Off_Farm_Income_Sources.png", height = 1000, width = 1200)
barplot(off_farm_summary$Percentage.of.HHs, main = "Off-Farm Income Sources", names.arg = off_farm_summary$Off.farm.Incomes, ylab = "Percentage of HHs")
dev.off()





