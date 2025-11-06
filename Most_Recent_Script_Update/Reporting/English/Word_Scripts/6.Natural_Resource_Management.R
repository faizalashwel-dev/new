NRM_Soil_Water<-List_to_True_False(dat_all$)
NRM_Soil_Water<-data.frame(colSums(NRM_Soil_Water))
NRM_Soil_Water[,1]<-round(100*NRM_Soil_Water[,1]/nrow(dat_all),1)
NRM_Soil_Water<-data.frame("Soil and Water Conservation"=row.names(NRM_Soil_Water), "Percentage of HHs Practicing"=NRM_Soil_Water[,1]) 
NRM_Soil_Water<-NRM_Soil_Water[order(NRM_Soil_Water[,2], decreasing = TRUE),]

# Any_Soil_Water<- c("Any", sum(NRM_Soil_Water[NRM_Soil_Water$Soil.and.Water.Conservation!="None",colnames(NRM_Soil_Water)=="Percentage.of.HHs.Practicing"], na.rm = T))
# NRM_Soil_Water$Soil.and.Water.Conservation<- as.character(NRM_Soil_Water$Soil.and.Water.Conservation)
# NRM_Soil_Water<- rbind(NRM_Soil_Water, as.character(Any_Soil_Water))

 colnames(NRM_Soil_Water)<-c("Soil and Water Conservation", "% of HHs")

png("Word_Outputs/6.Natural_Resource_Management/1.NRM_Soil.png", width = 500, height = 500)
par(mar=c(8,3,3,3),mfrow=c(1,1), cex=1.1)
barplot(as.numeric(as.character(NRM_Soil_Water$`% of HHs`)),main="Soil and Water Conservation", ylab="% of households", ylim=c(0,100),names.arg = gsub("_"," ",NRM_Soil_Water$`Soil and Water Conservation`), las=2)
dev.off()

write.csv(NRM_Soil_Water, "Word_Outputs/6.Natural_Resource_Management/1.NRM_Soil_table.csv", row.names = F)


NRM_Bio_df<-List_to_True_False(dat_all$biological_methods)
NRM_Bio_df<-data.frame(colSums(NRM_Bio_df))
NRM_Bio_df[,1]<-round(100*NRM_Bio_df[,1]/nrow(dat_all),1)
NRM_Bio_df<-data.frame("Biological Method"=row.names(NRM_Bio_df), "Percentage of HHs Practicing"=NRM_Bio_df[,1]) 
NRM_Bio_df<-NRM_Bio_df[order(NRM_Bio_df[,2], decreasing = TRUE),]

# Any_Bio<- c("Any", sum(NRM_Bio_df[NRM_Bio_df$Biological.Method!="None",colnames(NRM_Bio_df)=="Percentage.of.HHs.Practicing"], na.rm = T))
# NRM_Bio_df$Biological.Method<- as.character(NRM_Bio_df$Biological.Method)
# NRM_Bio_df<- rbind(NRM_Bio_df, as.character(Any_Bio))

 colnames(NRM_Bio_df)<-c("Biological Method", "% of HHs")

png("Word_Outputs/6.Natural_Resource_Management/2.NRM_Bio.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1.4)
barplot(NRM_Bio_df$`% of HHs`,main="Biological practices", ylab="% of households", ylim=c(0,100),names.arg = gsub("_"," ",NRM_Bio_df$`Biological Method`), las=2)
dev.off()

write.csv(NRM_Bio_df, "Word_Outputs/6.Natural_Resource_Management/2.NRM_Bio_table.csv", row.names = F)



NRM_Gully<-List_to_True_False(dat_all$gully_control)
NRM_Gully<-data.frame(colSums(NRM_Gully))
NRM_Gully[,1]<-round(100*NRM_Gully[,1]/nrow(dat_all),1)
NRM_Gully<-data.frame("Gully Control"=row.names(NRM_Gully), "Percentage of HHs Practicing"=NRM_Gully[,1]) 
NRM_Gully<-NRM_Gully[order(NRM_Gully[,2], decreasing = TRUE),]

colnames(NRM_Gully)<-c("Gully Control", "% of HHs")


png("Word_Outputs/6.Natural_Resource_Management/3.NRM_Gully.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1.4)
barplot(NRM_Gully$`% of HHs`,main="Gully Control", ylab="% of households", ylim=c(0,100),names.arg = gsub("_"," ",NRM_Gully$`Gully Control`), las=2)
dev.off()

write.csv(NRM_Gully, "Word_Outputs/6.Natural_Resource_Management/3.NRM_Gully_table.csv", row.names = F)

