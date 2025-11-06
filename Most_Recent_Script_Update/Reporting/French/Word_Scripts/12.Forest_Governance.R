Forest_gov_df<- data.frame("access_equality"=dat_all$access_equality,
                           "access_permission"=dat_all$access_permission,
                           "management_organisations"=dat_all$management_organisations,
                           "management_training"=dat_all$management_training)

#write.csv(Forest_gov_df, "Pietros_Trip/Forest_Gov/Forest_gov.csv")



png("Word_Outputs/12.Forest_Governance/1.Forest_Access_Summary.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(100*prop.table(table(dat_all$access_equality)), main="Reported Access to Forest", ylab = "Percentage of HHs", las=2)
dev.off()

write.csv(round(100*prop.table(table(dat_all$access_equality)),2), "Word_Outputs/12.Forest_Governance/1.Forest_Access_Summary_table.csv", row.names = F)


#### Forest Governance Structures ####
#### Forest Gov Management organisations ####
Helped_Management<-List_to_True_False(dat_all$management_organisations)
Helped_Management$Any<- apply(Helped_Management, 1, function (x) sum(x, na.rm = T)>0)
Helped_Management<-data.frame(colSums(Helped_Management, na.rm=T))
Helped_Management[,1]<-round(100*Helped_Management[,1]/nrow(dat_all),1)
Helped_Management<-data.frame("Management_Structure"=row.names(Helped_Management), "Percentage_of_HHs_Helping"=Helped_Management[,1]) 
Helped_Management<-Helped_Management[order(Helped_Management[,2], decreasing = TRUE),]
Helped_Management$Management_Structure<- gsub("decentral_tech_structure", "decentral", as.character(Helped_Management$Management_Structure))

png("Word_Outputs/12.Forest_Governance/2.Management_Structures.png")
par(mar=c(7,4,4,2), cex=1)
barplot(Helped_Management[,2], main="% of HHs Helping \n Forest Governance Structures",names.arg = Helped_Management[,1], las=2, ylab="% of households")
dev.off()

write.csv(Helped_Management, "Word_Outputs/12.Forest_Governance/2.Management_Structures_table.csv", row.names=TRUE)

training_df<-data.frame(round(100*prop.table(table(dat_all$management_training, dat$Village), margin=2),2))
training_df<-training_df[training_df$Var1=="Y",]
training_df<-training_df[,-c(1)]
colnames(training_df)<- c("Village", "Percentage_of_HHs_with_Training")
training_df$Control<- training_df$Village %in% control_villages


plot<-ggplot(training_df, aes(x=Village, y=Percentage_of_HHs_with_Training, fill=Control))
plot<-plot+geom_bar(stat="identity")

plot<-plot+theme_minimal()
plot<-plot+ ggtitle("Percentage of HHs with Forest Management Training by Village") +
  xlab("Village") + ylab("% of HHs with Training")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/12.Forest_Governance/3.Training_in_Forest_Management.png"))

write.csv(training_df,"Word_Outputs/12.Forest_Governance/3.Training_in_Forest_Management.csv")


# village_summary_forest_access<-data.frame(round(100*prop.table(table(dat_all$access_equality, dat$Village), margin=2),2))
# colnames(village_summary_forest_access)<- c("Degree_of_Access", "Village", "Percentage_of_HHs_in_Village")
# village_summary_forest_access$Control<- village_summary_forest_access$Village %in% control_villages
# 
# dir.create("Word_Outputs/12.Forest_Governance/Forest_Access_per_Village")
# 
# 
# for (i in unique(village_summary_forest_access$Village))
# {
#   temp<-village_summary_forest_access[village_summary_forest_access$Village==i,]
#   
#   if (unique(temp$Control)==T)
#   {
#     temp_control<-"Control"
#   }
#   
#   if (unique(temp$Control)==F)
#   {
#     temp_control<-"Not Control"
#   }
#   
#   plot<-ggplot(temp, aes(x=Degree_of_Access, y=Percentage_of_HHs_in_Village))
#   plot<-plot+geom_bar(stat="identity")
#   
#   plot<-plot+theme_minimal()
#   plot<-plot+ ggtitle(paste0("Forest Access in ",i, " (",temp_control, ")")) +
#     xlab("Degree_of_Access") + ylab("% of HHs from Village")
#   plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   plot
#   ggsave(paste0("Word_Outputs/12.Forest_Governance/Forest_Access_per_Village/",i,".png"))
# }




access_permission<-data.frame(round(100*prop.table(table(dat_all$access_permission, dat$Village), margin=2),2))
colnames(access_permission)<- c("Ask_Permission", "Village", "Percentage_of_HHs_in_Village")
access_permission<- access_permission[access_permission$Ask_Permission=="Y",]
access_permission<- access_permission[,-which(colnames(access_permission)=="Ask_Permission")]
access_permission$Control<- access_permission$Village %in% control_villages



plot<-ggplot(access_permission, aes(x=Village, y=Percentage_of_HHs_in_Village, fill=Control))
plot<-plot+geom_bar(stat="identity")

plot<-plot+theme_minimal()
plot<-plot+ ggtitle("Proportion of HHs who have to ask for Permission \n to Access Forest") +
  xlab("Village") + ylab("% of HHs from Village")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/12.Forest_Governance/4.Ask_Permission_Summary.png"))


write.csv(access_permission, "Word_Outputs/12.Forest_Governance/4.Ask_Permission_Summary_Table.csv")



Park_Value<-data.frame(round(100*prop.table(table(dat_all$park_w_value)),2))
colnames(Park_Value)<-c("Response", "% of HHs")
Park_Value$Response<- gsub("N", "Not Valueable", Park_Value$Response)
Park_Value$Response<- gsub("Y", "Valueable", Park_Value$Response)
png("Word_Outputs/12.Forest_Governance/5.Park_Valuable.png")
par(mar=c(7,4,4,2), cex=1)
barplot(Park_Value$`% of HHs`, main="Reported Value of Forest",names.arg = Park_Value$Response, ylab = "% of HHs", las=1)
dev.off()

write.csv(Park_Value, "Word_Outputs/12.Forest_Governance/5.Park_Valuable_table.csv", row.names = F)

Park_protect<-data.frame(round(100*prop.table(table(dat_all$park_w_protect)),2))
colnames(Park_protect)<-c("Response", "% of HHs")
Park_protect$Response<- gsub("N", "should not be protected", Park_protect$Response)
Park_protect$Response<- gsub("Y", "should protect", Park_protect$Response)

png("Word_Outputs/12.Forest_Governance/6.Park_protect.png")
par(mar=c(7,4,4,2), cex=1)
barplot(Park_protect$`% of HHs`, main="Reported Value of Forest",names.arg = Park_protect$Response, ylab = "% of HHs", las=1)
dev.off()

write.csv(Park_protect, "Word_Outputs/12.Forest_Governance/6.Park_protect_table.csv", row.names = F)

