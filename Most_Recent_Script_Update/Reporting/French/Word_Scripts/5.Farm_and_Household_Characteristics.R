png("Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/1.Graphique_Nombre_en_Menage.png")
par(mar=c(7,5,7,5),mfrow=c(1,2),mgp=c(3,2,0))

boxplot(list((dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$HHsizemembers[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$HHsizemembers)), 
        outline = F,  col = "lightgray", ylab="Membre de Ménage", main="Membre de Ménage", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

      #  outline = F,  col = "lightgray", ylab="Members", main="Household Members", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$HHsizeMAE[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$HHsizeMAE[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$HHsizeMAE[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$HHsizeMAE[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$HHsizeMAE)), 
        outline = F,  col = "lightgray", ylab="EHA", main="Équivalent Homme Adulte \n (en termesde besoin calorique)", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

      #  outline = F,  col = "lightgray", ylab="MAE", main="Male Adult Equivalent \n (in terms of calorie requirement)", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 
dev.off()

###-------

HH_Size<- summarise(dat_grp, 
                         round(median(HHsizemembers, na.rm=T),2), 
                         round(IQR(HHsizemembers, na.rm=T),2),
                         round(median(HHsizeMAE, na.rm=T),2), 
                         round(IQR(HHsizeMAE, na.rm=T),2)
                        
)

HH_Size<-HH_Size[!is.na(HH_Size$TVA_quartiles),]
colnames(HH_Size) <- c("Quartiles", 
                            "Membre de Ménage (Médian)", 
                            "Membre de Ménage (Gamme Interquartile)",
                            "EHA (Médian)", 
                            "EHA (Gamme Interquartile)"
                           ) 
# whole_pop<- c("All", 
#               round(median(dat$HHsizemembers, na.rm=T),2), 
#               round(IQR(dat$HHsizemembers, na.rm=T),2),
#               round(median(dat$HHsizeMAE, na.rm=T),2), 
#               round(IQR(dat$HHsizeMAE, na.rm=T),2)
#              
# )
# 
# 
# HH_Size$Quartiles<- as.character(HH_Size$Quartiles)
# HH_Size<- rbind(HH_Size, whole_pop)
write.csv(HH_Size, "Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/1.Nombre_en_Menage.csv", row.names = F)
#----------------



png("Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/2.Land_Size.png")
par(mfrow=c(1,2),mgp=c(3,2,0))

boxplot(list((dat$LandOwned[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$LandOwned[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$LandOwned[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$LandOwned[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$LandOwned)), 
        outline = F,  col = "lightgray", ylab="ha", main="Terres Possédé", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

      #  outline = F,  col = "lightgray", ylab="ha", main="Land Owned", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$LandCultivated[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$LandCultivated[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$LandCultivated[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$LandCultivated[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$LandCultivated)), 
        outline = F,  col = "lightgray", ylab="ha", main="Terres Cultivé", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

       # outline = F,  col = "lightgray", ylab="ha", main="Land Cultivated", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 
dev.off()

###-------

Land_size<- summarise(dat_grp, 
                    round(median(LandOwned, na.rm=T),2), 
                    round(IQR(LandOwned, na.rm=T),2),
                    round(median(LandCultivated, na.rm=T),2), 
                    round(IQR(LandCultivated, na.rm=T),2)
                    
)

Land_size<-Land_size[!is.na(Land_size$TVA_quartiles),]
colnames(Land_size) <- c("Quartiles", 
                       "Terres Possédé (Médian)", 
                       "Terres Possédé (Gamme Interquartile)",
                       "Land Cultivated (Médian)", 
                       "Land Cultivated (Gamme Interquartile)"
) 
# whole_pop<- c("All", 
#               round(median(dat$LandOwned, na.rm=T),2), 
#               round(IQR(dat$LandOwned, na.rm=T),2),
#               round(median(dat$LandCultivated, na.rm=T),2), 
#               round(IQR(dat$LandCultivated, na.rm=T),2)
#               
# )
# 
# 
# Land_size$Quartiles<- as.character(Land_size$Quartiles)
# Land_size<- rbind(Land_size, whole_pop)
write.csv(Land_size, "Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/2.Superficie_des_Terres.csv", row.names = F)
#----------------

if (sum(dat_all$land_tenure!='NA')>0)
{
Land_tenure<-List_to_True_False(dat_all$land_tenure)
Land_tenure<-data.frame(colSums(Land_tenure))
Land_tenure[,1]<-round(100*Land_tenure[,1]/nrow(dat_all),1)
Land_tenure<-data.frame("Land Tenure"=row.names(Land_tenure), "Percentage of HHs"=Land_tenure[,1])
colnames(Land_tenure)<- c("Régime Foncier", "% des Ménages")
Land_tenure<-Land_tenure[order(Land_tenure[,2], decreasing = TRUE),]


write.csv(Land_tenure, "Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/3.Regime_Foncier.csv", row.names = F)


png("Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/3.Graphique_Regime_Foncier.png", width = 500, height = 500)
par(mar=c(7,3,3,3),mfrow=c(1,1), cex=1)
barplot(Land_tenure$`% des Ménages`,main="Régime Foncier selin les Ménages", ylab="% des Ménages", ylim=c(0,100),names.arg = gsub("_"," ",Land_tenure$`Régime Foncier`), las=2)
dev.off()
}

###------



temp_owned<-dat$LandOwned[dat$LandOwned<30]
temp_cultivated<-dat$LandCultivated[dat$LandCultivated<30]
png("Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/4.Graphique_Terres_Possede_et_Terres_Cultive.png",height = 600, width = 1200)
par(mfrow=c(1,2),cex=1.1)

if (max(temp_cultivated, na.rm = T)>max(temp_owned, na.rm=T))
{
  
  h1=hist(temp_cultivated, plot=F, breaks=5)
  h1$density = h1$counts/nrow(dat)*100
  h=hist(temp_owned, plot=F, breaks = h1$breaks)
  h$density = h$counts/nrow(dat)*100
  
}
if (max(temp_owned, na.rm = T)>max(temp_cultivated, na.rm=T))
{

  
  h=hist(temp_owned, plot=F, breaks = 5)
  h$density = h$counts/nrow(dat)*100
  
  h1=hist(temp_cultivated, plot=F, breaks=h$breaks)
  h1$density = h1$counts/nrow(dat)*100
  
  
 
  
  
}

if (max(temp_owned, na.rm = T)==max(temp_cultivated, na.rm=T))
{

  
  h=hist(temp_owned, plot=F, breaks = 5)
  h$density = h$counts/nrow(dat)*100
  
  h1=hist(temp_cultivated, plot=F, breaks=h$breaks)
  h1$density = h1$counts/nrow(dat)*100
  
  
 
  
  
}

#h=hist(temp_owned, plot=F, breaks = 5)
#h$density = h$counts/nrow(dat)*100
plot(h,freq=FALSE, main="Terres Possedé", xlab="ha", col="lightgray", ylab="% des Ménages")
#h1=hist(temp_cultivated, plot=F, breaks=h$breaks)
#h1$density = h1$counts/nrow(dat)*100
plot(h1,freq=FALSE, main="Terres Cultivé", xlab="ha", col="lightgray", ylab="% des Ménages")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[1]], main=levels(FA_groups)[1], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[2]], main=levels(FA_groups)[2], xlab=NULL, col="lightgray")
#hist(dat$LandOwned[FA_groups==levels(FA_groups)[3]], main=levels(FA_groups)[3], xlab=NULL, col="lightgray")
dev.off()

Land_owned_hist_summary<-data.frame(rbind(round(h$mids),round(h$density,2)), row.names = c("Superficie de Terre (ha)", "Terres Possédé % hh"))
colnames(Land_owned_hist_summary)<-NULL
Land_cult_hist_summary<-data.frame(rbind(round(h1$mids),round(h1$density,2)), row.names = c("Superficie de Terre (ha)", "Terres Cultivé % hh"))
colnames(Land_cult_hist_summary)<-NULL

write.table(Land_owned_hist_summary, file = "Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/5.Terres_Possede.csv",sep=",", col.names = F)

write.table(Land_cult_hist_summary, file="Word_Outputs/5.Charecteristiques_de_Ferme_et_Menage/6.Terres_Cultive.csv", sep=",",col.names = F)


####-----------------------

# eat_crop<-data.frame(t(data.frame(lapply(crop_use, function(x) round((100*length(grep("eat", x))/nrow(dat)),2)))))
# sell_crop<-data.frame(t(data.frame(lapply(crop_use, function(x) round((100*length(grep("sell", x))/nrow(dat)),2)))))
# save_seed_crop<-data.frame(t(data.frame(lapply(crop_use, function(x) round((100*length(grep("saveseed", x))/nrow(dat)),2)))))
# givetrade_crop<-data.frame(t(data.frame(lapply(crop_use, function(x) round((100*length(grep("givetrade", x))/nrow(dat)),2)))))
# feed_lstk_crop<-data.frame(t(data.frame(lapply(crop_use, function(x) round((100*length(grep("feed", x))/nrow(dat)),2)))))
# 
# crop_use_temp<- data.frame(matrix(nrow=0, ncol=3))
# 
# colnames(crop_use_temp)<- c("crop", "use", "percentage of hh")
# temp<- data.frame(row.names(eat_crop), rep("eat", length(row.names(eat_crop))), eat_crop[,1])
# colnames(temp)<- c("crop", "use", "percentage of hh")
# crop_use_temp<-rbind(crop_use_temp, temp)
# colnames(crop_use_temp)<- c("crop", "use", "percentage of hh")
# 
# temp<- data.frame(row.names(sell_crop), rep("sell", length(row.names(sell_crop))), sell_crop[,1])
# colnames(temp)<- c("crop", "use", "percentage of hh")
# crop_use_temp<-rbind(crop_use_temp, temp)
# 
# temp<- data.frame(row.names(save_seed_crop), rep("saveseed", length(row.names(save_seed_crop))), save_seed_crop[,1])
# colnames(temp)<- c("crop", "use", "percentage of hh")
# crop_use_temp<-rbind(crop_use_temp, temp)
# 
# temp<- data.frame(row.names(givetrade_crop), rep("givetrade", length(row.names(givetrade_crop))), givetrade_crop[,1])
# colnames(temp)<- c("crop", "use", "percentage of hh")
# crop_use_temp<-rbind(crop_use_temp, temp)
# 
# 
# temp<- data.frame(row.names(feed_lstk_crop), rep("feed_lstk", length(row.names(feed_lstk_crop))), feed_lstk_crop[,1])
# colnames(temp)<- c("crop", "use", "percentage of hh")
# crop_use_temp<-rbind(crop_use_temp, temp)
# 
# png("Word_Outputs/3.Household_Incomes_and_Productivity/17.Crop_Use.png", width = 500, height = 500)
# plot<-ggplot(crop_use_temp, aes(x=crop, y=`percentage of hh`, fill=use))
# plot<-plot+geom_bar(stat="identity")
# plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot
# dev.off()
# 











