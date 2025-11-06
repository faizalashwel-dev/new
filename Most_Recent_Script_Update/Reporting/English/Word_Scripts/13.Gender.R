png("Word_Outputs/9.Gender/5.Gender_Control.png", height = 600, width = 900)
par(mar=c(5,3,3,2),mfrow=c(2,2), cex=0.9)
boxplot(dat$Gender_MaleControl~dat$TVA_quartiles, col="lightgray", outline=F, main="Male Adult Control", names=qnames, las=2)
boxplot(dat$Gender_FemaleControl~dat$TVA_quartiles, col="lightgray", outline=F, main="Female Adult Control", names=qnames, las=2)
boxplot(dat$Gender_MaleYouthControl~dat$TVA_quartiles, col="lightgray", outline=F, main="Male Youth Control", names=qnames, las=2)
boxplot(dat$Gender_FemaleYouthControl~dat$TVA_quartiles, col="lightgray", outline=F, main="Female Youth Control", names=qnames, las=2)
dev.off()


#### VCC ####
# VCC_Sheet<-read.csv("VCC_Sheet.csv")
# VCC_Sheet<-data.frame(lapply(VCC_Sheet, function(x) factor(x, levels = c("none", "little", "equal", "more_than"))))
# detach("package:dplyr")
# library(plyr)
# VCC_Sheet_numeric<-data.frame(lapply(VCC_Sheet, function(x)  mapvalues(x,  from=c("none", "little", "equal", "more_than"), to=c(0,1,2,3))))
# detach("package:plyr")
# library(dplyr)
# 
# VCC_Sheet_numeric<- data.frame(lapply(VCC_Sheet_numeric, function (x) as.numeric(as.character(x))))
# 
# voice_sheet<- VCC_Sheet_numeric[,grep("voice", colnames(VCC_Sheet_numeric))]
# choice_sheet<- VCC_Sheet_numeric[,grep("choice", colnames(VCC_Sheet_numeric))]
# control_sheet<- VCC_Sheet_numeric[,grep("control", colnames(VCC_Sheet_numeric))]
# 
# hh_sheet<- VCC_Sheet_numeric[,grep("hh", colnames(VCC_Sheet_numeric))]
# com_sheet<- VCC_Sheet_numeric[,grep("comm", colnames(VCC_Sheet_numeric))]
# 
# 
# 
# voice_score<- as.numeric(rowMeans(voice_sheet, na.rm=T))
# choice_score<- as.numeric(rowMeans(choice_sheet, na.rm=T))
# control_score<- as.numeric(rowMeans(control_sheet, na.rm=T))
# 
# vcc_scores<- data.frame("Voice"=voice_score/3, "Choice"=choice_score/3, "Control"=control_score/3)
# 
# 
# temp<-rbind(apply(vcc_scores, 2, function (x) quantile(x, c(1/4), na.rm = T)),
#             apply(vcc_scores, 2, function (x) median(x, na.rm = T)),
#             apply(vcc_scores, 2, function (x) quantile(x,c(3/4), na.rm = T)),
#             apply(vcc_scores, 2, function (x) IQR(x, na.rm = T))
#             )
# temp<-data.frame(temp)
# row.names(temp)<- c("1st Quartile",
#                     "Median",
#                     "3nd Quartile",
#                     "IQR")
# write.csv(temp, "Word_Outputs/13.Gender/1.VCC_Summary_table.csv")
# 
# # x<-apply(vcc_scores, 2, function (x) min(x, na.rm = T))
# # x<-apply(vcc_scores, 2, function (x) max(x, na.rm = T))
# 
# 
# quantile(vcc_scores$Voice, c(1/4), na.rm = T)
# 
# 
# png("Word_Outputs/13.Gender/1.VCC_Summary.png")
# par(mar=c(3,3,3,3), cex=1)
# boxplot(vcc_scores, outline = F, main="Reported VCC Scores (0-1)", ylab="Score")
# dev.off()
# 
# 
# 
# 
# dat$beneficiary_aduna[is.na(dat$beneficiary_aduna)]<-FALSE
# beneficiary_vcc<- data.frame("Score"=c(voice_score/3,choice_score/3, control_score/3),
#                              "Variable"=c(rep("Voice",length(voice_score)),rep("Choice",length(choice_score)),rep("Control",length(control_score))))
# 
# beneficiary_vcc$beneficiary_aduna<- rep(dat$beneficiary_aduna, 3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot<-ggplot(beneficiary_vcc, aes(x=Variable, y=Score, fill=beneficiary_aduna))
# plot<-plot+geom_boxplot(outlier.shape=NA)
# plot<-plot+theme_minimal()
# plot<-plot+ ggtitle(paste0("Score for Different Aspects of VCC")) +
#   ylab("Score (0-1)")+ xlab("")
# plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot
# 
# ggsave(paste0("Word_Outputs/13.Gender/2.VCC_beneficiary_aduna.png"))
# 
# 
# 
# 
# 
# 
# hh_score<- as.numeric(rowMeans(hh_sheet, na.rm=T))
# com_score<- as.numeric(rowMeans(com_sheet, na.rm=T))
# 
# House_Community_scores<- data.frame("Household"=hh_score/3, "Community"=com_score/3)
# 
# 
# temp<-rbind(apply(House_Community_scores, 2, function (x) quantile(x, c(1/4), na.rm = T)),
#             apply(House_Community_scores, 2, function (x) median(x, na.rm = T)),
#             apply(House_Community_scores, 2, function (x) quantile(x,c(3/4), na.rm = T)),
#             apply(House_Community_scores, 2, function (x) IQR(x, na.rm = T))
# )
# temp<-data.frame(temp)
# row.names(temp)<- c("1st Quartile",
#                     "Median",
#                     "3nd Quartile",
#                     "IQR")
# write.csv(temp, "Word_Outputs/13.Gender/3.Household_Comm_Summary_table.csv")
# 
# 
# png("Word_Outputs/13.Gender/3.Household_Comm_Summary.png")
# par(mar=c(7,4,4,2), cex=1.4)
# boxplot(House_Community_scores, outline = F, main="Reported VCC Scores for (0-1)\n for HH and Community Level", ylab="Score")
# dev.off()
# 
# 
# 
# 
# voice_sheet_hh<- VCC_Sheet_numeric[,grep("voice_hh", colnames(VCC_Sheet_numeric))]
# choice_sheet_hh<- VCC_Sheet_numeric[,grep("choice_hh", colnames(VCC_Sheet_numeric))]
# control_sheet_hh<- VCC_Sheet_numeric[,grep("control_hh", colnames(VCC_Sheet_numeric))]
# 
# voice_sheet_comm<- VCC_Sheet_numeric[,grep("voice_comm", colnames(VCC_Sheet_numeric))]
# choice_sheet_comm<- VCC_Sheet_numeric[,grep("choice_comm", colnames(VCC_Sheet_numeric))]
# control_sheet_comm<- VCC_Sheet_numeric[,grep("control_comm", colnames(VCC_Sheet_numeric))]
# 
# 
# voice_hh_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(voice_sheet_hh, na.rm=T)))
# voice_hh_score$Variable<-rep("Voice", nrow(voice_hh_score))
# voice_hh_score$Level<-rep("Household", nrow(voice_hh_score))
# 
# choice_hh_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(choice_sheet_hh, na.rm=T)))
# choice_hh_score$Variable<-rep("Choice", nrow(choice_hh_score))
# choice_hh_score$Level<-rep("Household", nrow(choice_hh_score))
# 
# control_hh_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(control_sheet_hh, na.rm=T)))
# control_hh_score$Variable<-rep("Control", nrow(control_hh_score))
# control_hh_score$Level<-rep("Household", nrow(control_hh_score))
# 
# voice_comm_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(voice_sheet_comm, na.rm=T)))
# voice_comm_score$Variable<-rep("Voice", nrow(voice_comm_score))
# voice_comm_score$Level<-rep("Community", nrow(voice_comm_score))
# 
# choice_comm_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(choice_sheet_comm, na.rm=T)))
# choice_comm_score$Variable<-rep("Choice", nrow(choice_comm_score))
# choice_comm_score$Level<-rep("Community", nrow(choice_comm_score))
# 
# control_comm_score<- data.frame("Score"=as.numeric((1/3)*rowMeans(control_sheet_comm, na.rm=T)))
# control_comm_score$Variable<-rep("Control", nrow(control_comm_score))
# control_comm_score$Level<-rep("Community", nrow(control_comm_score))
# 
# vcc_summary_sheet<- rbind(voice_hh_score, 
#                           choice_hh_score,
#                           control_hh_score,
#                           voice_comm_score,
#                           choice_comm_score,
#                           control_comm_score)
# 
# 
# plot<-ggplot(vcc_summary_sheet, aes(x=Variable, y=Score, fill=Level))
# plot<-plot+geom_boxplot(outlier.shape=NA)
# plot<-plot+theme_minimal()
# plot<-plot+ ggtitle(paste0("Score for Different Aspects of VCC")) +
#   ylab("Score (0-1)")+ xlab("")
# plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot
# 
# 
# ggsave(paste0("Word_Outputs/13.Gender/4.VCC_Breakdown_Summary.png"))
# 
# 
