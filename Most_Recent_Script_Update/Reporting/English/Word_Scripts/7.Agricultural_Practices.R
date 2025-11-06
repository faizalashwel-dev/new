###------------------------------
#### Crop Use #####
unique_crop_use<-c()

crop_use<-dat_crop_use

for (i in 1:ncol(crop_use))
{
  unique_crop_use<-c(unique_crop_use, unique(unlist(strsplit(crop_use[,i], " "))))
}
unique_crop_use<- unique(unique_crop_use)
unique_crop_use<-unique_crop_use[-which(is.na(unique_crop_use))]

crop_use_proportions<- data.frame(matrix(NA, nrow=length(unique_crop_use), ncol=ncol(crop_use)))
colnames(crop_use_proportions)<-colnames(crop_use)
row.names(crop_use_proportions)<- unique_crop_use
for (i in 1:ncol(crop_use))
{
  #temp<- data.frame(prop.table(table(crop_use[,i])))
  
  temp<- data.frame(table(unlist(strsplit(crop_use[,i], " "))))
  temp[,2]<- 100*temp[,2]/nrow(dat_all)
  for (uses in temp[,1])
  {
    crop_use_proportions[uses,i]<- temp[temp[,1]==uses,2]
  }
}

crop_use_proportions<- data.frame("Use"=row.names(crop_use_proportions), crop_use_proportions, row.names = NULL)

crop_use_proportions_plot<- data.frame("Use"=rep(crop_use_proportions$Use, ncol(crop_use_proportions)-1))


percentage_hh<-c()
crop<- c()

for (i in 2:ncol(crop_use_proportions))
{
  percentage_hh<-c(percentage_hh, crop_use_proportions[,i])
  crop<-c(crop, rep(colnames(crop_use_proportions)[i], nrow(crop_use_proportions)))
}
crop_use_proportions_plot$crop<-crop
crop_use_proportions_plot$percentage_hh<-percentage_hh
crop_use_proportions_plot<-crop_use_proportions_plot[-which(crop_use_proportions_plot$Use=="NA"),]



plot<-ggplot(crop_use_proportions_plot, aes(x = crop, y = percentage_hh, fill = Use)) + geom_bar(stat="identity", position = "dodge") 
#plot<- plot+theme_classic() 
plot<-plot+theme( axis.line = element_line(size=0), axis.text.x=element_text(colour="black", angle = 90, vjust = 0.5, hjust= 1), axis.text.y=element_text(colour="black") ) 
plot<-plot+guides(fill=guide_legend(title="Crop Use")) 
plot<-plot+ylab("% of households")  + xlab("") 
plot<-plot+ggtitle("Households Reporting Uses of Crops")
plot
ggsave("Word_Outputs/7.Agricultural_Practices/1.Crops_use.png")
###------------------------------
#### Determining most common crops grown #####


crops_most_important<-List_to_True_False(dat_all$crops_all)
crops_most_important<-data.frame(colSums(crops_most_important))
crops_most_important[,1]<-round(100*crops_most_important[,1]/nrow(dat_all),1)
crops_most_important<-data.frame("Crop"=row.names(crops_most_important), "Percentage of HHs Growing"=crops_most_important[,1]) 
crops_most_important<-crops_most_important[order(crops_most_important[,2], decreasing = TRUE),]

if ("other" %in% crops_most_important$Crop)
{
crops_most_important<-crops_most_important[-grep("other", crops_most_important$Crop),]
}
write.csv(crops_most_important, "Word_Outputs/7.Agricultural_Practices/2.Crops_Summary.csv", row.names = F)


png("Word_Outputs/7.Agricultural_Practices/2.Crop_Summary.png", height = 1000, width = 1200)
par(cex=1, mar=c(10,5,5,5))
barplot(crops_most_important$Percentage.of.HHs.Growing, names.arg = crops_most_important$Crop, main = "All Crops Grown", las=2)
dev.off()
###------------------------------
#### Crop Residue_Use ####
unique_crop_residue_use<-c()

crop_residue_use<-dat_crop_residue_use

for (i in 1:ncol(crop_residue_use))
{
  unique_crop_residue_use<-c(unique_crop_residue_use, unique(unlist(strsplit(crop_residue_use[,i], " "))))
}
unique_crop_residue_use<- unique(unique_crop_residue_use)
unique_crop_residue_use<-unique_crop_residue_use[-which(is.na(unique_crop_residue_use))]

crop_residue_use_proportions<- data.frame(matrix(NA, nrow=length(unique_crop_residue_use), ncol=ncol(crop_residue_use)))
colnames(crop_residue_use_proportions)<-colnames(crop_residue_use)
row.names(crop_residue_use_proportions)<- unique_crop_residue_use
for (i in 1:ncol(crop_residue_use))
{
  #temp<- data.frame(prop.table(table(crop_use[,i])))
  
  temp<- data.frame(table(unlist(strsplit(crop_residue_use[,i], " "))))
  temp[,2]<- 100*temp[,2]/nrow(dat_all)
  for (uses in temp[,1])
  {
    crop_residue_use_proportions[uses,i]<- temp[temp[,1]==uses,2]
  }
}

crop_residue_use_proportions<- data.frame("Use"=row.names(crop_residue_use_proportions), crop_residue_use_proportions, row.names = NULL)

crop_residue_use_proportions_plot<- data.frame("Use"=rep(crop_residue_use_proportions$Use, ncol(crop_residue_use_proportions)-1))


percentage_hh<-c()
crop<- c()

for (i in 2:ncol(crop_residue_use_proportions))
{
  percentage_hh<-c(percentage_hh, crop_residue_use_proportions[,i])
  crop<-c(crop, rep(colnames(crop_residue_use_proportions)[i], nrow(crop_residue_use_proportions)))
}
crop_residue_use_proportions_plot$crop<-crop
crop_residue_use_proportions_plot$percentage_hh<-percentage_hh
crop_residue_use_proportions_plot<-crop_residue_use_proportions_plot[-which(crop_residue_use_proportions_plot$Use=="NA"),]



#png("Crops_Residue_Uses.png", height = 1000, width = 1200)
plot<-ggplot(crop_residue_use_proportions_plot, aes(x = crop, y = percentage_hh, fill = Use)) + geom_bar(stat="identity", position = "dodge") 
#plot<- plot+theme_classic() 
plot<-plot+theme( axis.line = element_line(size=0), axis.text.x=element_text(colour="black", angle = 90, vjust = 0.5, hjust= 1), axis.text.y=element_text(colour="black") ) 
plot<-plot+guides(fill=guide_legend(title="Crop Residue Use")) 
plot<-plot+ylab("% of households")  + xlab("") 
plot<-plot+ggtitle("Crop Residue Uses")
plot
ggsave("Word_Outputs/7.Agricultural_Practices/3.Crops_Residue_Uses.png")

###------------------------------
#### Yield and Harvests ####

imp_crop_yields<- dat_crop[,grep("Harvested", colnames(dat_crop))]
colnames(imp_crop_yields)<- gsub("Harvested_", "", colnames(imp_crop_yields))
imp_crop_yields<- data.frame(lapply(imp_crop_yields, function (x) as.numeric(as.character(x))))


imp_crop_harvests<- dat_crop[,grep("Yield", colnames(dat_crop))]
colnames(imp_crop_harvests)<- gsub("Yield_", "", colnames(imp_crop_harvests))
imp_crop_harvests<- data.frame(lapply(imp_crop_harvests, function (x) as.numeric(as.character(x))))


png("Word_Outputs/7.Agricultural_Practices/4.Average_crop_yields.png", height = 700, width = 700); par(mar=c(9,4,4,2), cex=1.6)
boxplot(data.frame(imp_crop_yields), las=2, col="lightgray", ylab="Yields (tons/ha)",  main="Crop Yields\n(per hectare)", outline=F)
dev.off()



# png("Word_Outputs/7.Agricultural_Practices/4.Average_crop_harvest.png", height = 700, width = 700); par(mar=c(9,4,4,2), cex=1.6)
# boxplot(data.frame(imp_crop_harvests), las=2, col="lightgray", ylab="Harvest (tons/ha)",  main="Crop Yields\n(per hectare)", outline=F)
# dev.off()




median_yield<-data.frame(t(data.frame(lapply(imp_crop_yields, function(x) round(median(x, na.rm = T),2)))))
IQR_yield<-data.frame(t(data.frame(lapply(imp_crop_yields, function(x) round(IQR(x, na.rm = T),2)))))

median_harvest<-data.frame(t(data.frame(lapply(imp_crop_harvests, function(x) round(median(x, na.rm = T),2)))))
IQR_harvest<-data.frame(t(data.frame(lapply(imp_crop_harvests, function(x) round(IQR(x, na.rm = T),2)))))

crop_yield_df<- data.frame("Crop"=row.names(median_yield),"median_yield"=median_yield[,1], "IQR_yield"=IQR_yield[,1],"median_harvest"=median_harvest[,1],"IQR_harvest"=IQR_harvest[,1])

write.csv(crop_yield_df, "Word_Outputs/7.Agricultural_Practices/4.crop_yields_harvest_table.csv", row.names = F)

###------------------------------


Livestock_most_important<-List_to_True_False(dat_all$livestock_all)
Livestock_most_important<-data.frame(colSums(Livestock_most_important))
Livestock_most_important[,1]<-round(100*Livestock_most_important[,1]/nrow(dat_all),1)
Livestock_most_important<-data.frame("Livestock Kept"=row.names(Livestock_most_important), "Percentage of HHs Keeping Animal"=Livestock_most_important[,1]) 
Livestock_most_important<-Livestock_most_important[order(Livestock_most_important[,2], decreasing = TRUE),]
if ("other" %in% Livestock_most_important$Livestock.Kept)
{
Livestock_most_important<-Livestock_most_important[-grep("other", Livestock_most_important$Livestock.Kept),]
}
write.csv(Livestock_most_important, "Word_Outputs/7.Agricultural_Practices/5.Livestock_summary_table.csv", row.names = F)


png("Word_Outputs/7.Agricultural_Practices/5.Livestock_summary.png", height = 1000, width = 1200)
par(cex=1, mar=c(10,5,5,5))
barplot(Livestock_most_important$Percentage.of.HHs.Keeping.Animal, names.arg = Livestock_most_important$Livestock.Kept, main = "All Livestock kept", las=2, ylab = "Percentage of HHs", ylim=c(0,100))
dev.off()


livestock_heads<-dat_lvstk[,grep("Whole_Livestock_Kept_Number", colnames(dat_lvstk))]
colnames(livestock_heads)<-gsub("Whole_Livestock_Kept_Number_","",colnames(livestock_heads))

if("other" %in% colnames(livestock_heads))
{
livestock_heads_temp<-livestock_heads[, -grep("other",colnames(livestock_heads))]
}else{livestock_heads_temp<-livestock_heads}

livestock_heads_temp<-data.frame(lapply(livestock_heads_temp, function(x) as.numeric(as.character(x))))

png("Word_Outputs/7.Agricultural_Practices/6.Livestock_Heads.png", height = 1000, width = 1200)
boxplot(livestock_heads_temp, main = "Livestock Heads", las=2, ylab = "Number of Livestock Heads",  outline=F)
dev.off()

heads_median<-t(data.frame(lapply(livestock_heads_temp, function(x) median(x, na.rm=T))))
heads_IQR<-t(data.frame(lapply(livestock_heads_temp, function(x) IQR(x, na.rm=T))))

livestock_heads_summary<- data.frame("Livestock"=row.names(heads_median), "Heads (Median)"=heads_median[,1],"Heads (IQR)"=heads_IQR[,1])
colnames(livestock_heads_summary)<- c("Livestock", "Heads (Median)","Heads (IQR)" )

write.csv(livestock_heads_summary, "Word_Outputs/7.Agricultural_Practices/6.livestock_heads_table.csv", row.names = F)



