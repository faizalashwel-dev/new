temp<-data.frame(prop.table(table(dat_all$offfarm_incomes_any)))

png("Word_Outputs/7.Revenu_Hors_d_Exploitation/1.Graphique_Revenu_Hors_d'Exploitation.png", height = 1000, width = 1200)
barplot(temp$Freq*100, main = "Ont-ils un Revenu Hors dExploitation", names.arg = temp$Var1, ylab = "% des ménages")
dev.off()

png("Word_Outputs/7.Revenu_Hors_d_Exploitation/2.Histogramme_Revenu_Hors_d_exploitation.png",height = 600, width = 1200)
par(mfrow=c(1,1),cex=1.6)
h=hist(dat$offfarm_income[dat$offfarm_income<500], plot=F)
h$density = h$counts/nrow(dat)*100
plot(h,freq=FALSE, main="Revenu Hors d'Exploitation", xlab="$/menage/an", col="lightgray", ylab="% de ménages")

#hist(dat$LandOwned[FA_groups==levels(FA_groups)[1]], main=levels(FA_groups)[1], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[2]], main=levels(FA_groups)[2], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[3]], main=levels(FA_groups)[3], xlab=NULL, col="lightgray")
dev.off()

off_farm_income_table<-data.frame(rbind(round(h$mids),round(h$density,2)), row.names = c("Revenu Hors d'Exploitation PPP$", "Revenu Hors d'Exploitation % des ménages"))
colnames(off_farm_income_table)<-NULL

write.table(off_farm_income_table, file = "Word_Outputs/7.Revenu_Hors_d_Exploitation/2.Revenu_Hors_d_Exploitation.csv",sep=",", col.names = F)






off_farm_summary<-List_to_True_False(dat_all$offfarm_incomes)
off_farm_summary<-data.frame(colSums(off_farm_summary))
off_farm_summary[,1]<-round(100*off_farm_summary[,1]/nrow(dat_all),1)
off_farm_summary<-data.frame("Off-farm Incomes"=row.names(off_farm_summary), "Percentage of HHs"=off_farm_summary[,1]) 
off_farm_summary<-off_farm_summary[order(off_farm_summary[,2], decreasing = TRUE),]
off_farm_summary<-off_farm_summary[-grep("other", off_farm_summary$Off.farm.Incomes),]

colnames(off_farm_summary)<- c("Revenu Hors d'Exploitation", "% des Ménages")

write.csv(off_farm_summary, "Word_Outputs/7.Revenu_Hors_d_Exploitation/3.Sources_de_Revenu_Hors_d_Exploitation.csv", row.names = F)


png("Word_Outputs/7.Revenu_Hors_d_Exploitation/3.Off_Farm_Income_Sources.png", height = 1000, width = 1200)
barplot(off_farm_summary$`% des Ménages`, main = "Sources de Revenu Hors d'Exploitation", names.arg = off_farm_summary$`Revenu Hors d'Exploitation`, ylab = "% des ménages")
dev.off()





