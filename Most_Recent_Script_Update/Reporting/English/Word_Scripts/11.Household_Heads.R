# HH_heads_df<-data.frame(table(dat$HouseholdType))
# 
# png("Word_Outputs/11.Household_Heads/1.HH_Heads_Marital.png")
# par(mar=c(7,4,4,2), cex=1.4)
# barplot(HH_heads_df$Freq/prop_rat*100, main="Household Head Marital Status", las=2, ylab="% of households", names.arg = HH_heads_df$Var1)
# dev.off()
# 
# HH_heads_all<-data.frame("Marital_Status"=dat$HouseholdType, "TVA_quartile"= dat$TVA_Quartile)
# 
# HH_heads_all
# HH_heads_lower<-data.frame(prop.table(table(HH_heads_all$Marital_Status[HH_heads_all$TVA_quartile=="lowest"])))
# HH_heads_lower$quartile<- rep("lowest", nrow(HH_heads_lower))
# 
# HH_heads_lower_middle<-data.frame(prop.table(table(HH_heads_all$Marital_Status[HH_heads_all$TVA_quartile=="lower middle"])))
# HH_heads_lower_middle$quartile<- rep("lower middle", nrow(HH_heads_lower_middle))
# 
# HH_heads_upper_middle<-data.frame(prop.table(table(HH_heads_all$Marital_Status[HH_heads_all$TVA_quartile=="upper middle"])))
# HH_heads_upper_middle$quartile<- rep("upper middle", nrow(HH_heads_upper_middle))
# 
# 
# HH_heads_upper<-data.frame(prop.table(table(HH_heads_all$Marital_Status[HH_heads_all$TVA_quartile=="upper"])))
# HH_heads_upper$quartile<- rep("upper", nrow(HH_heads_upper))
# 
# HH_heads<-data.frame(prop.table(table(HH_heads_all$Marital_Status)))
# HH_heads$quartile<- rep("All", nrow(HH_heads))
# 
# 
# Marital_status_by_Quartile<-rbind(HH_heads_lower,HH_heads_lower_middle,HH_heads_upper_middle,HH_heads_upper,HH_heads)
# colnames(Marital_status_by_Quartile)<- c("Marital Status", "Percentage of HHs", "Quartile")
# Marital_status_by_Quartile$`Percentage of HHs`<- round(100*Marital_status_by_Quartile$`Percentage of HHs`,2)
# 
# Marital_status_by_Quartile$Quartile<- factor(Marital_status_by_Quartile$Quartile, levels= c("lowest", "lower middle", "upper middle", "upper", "All"))
# 
# plot<-ggplot(Marital_status_by_Quartile, aes(x=Quartile, y=`Percentage of HHs`, fill=`Marital Status`))
# plot<-plot+geom_bar(stat="identity")
# plot<-plot+theme_minimal()
# 
# plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
# plot<- plot+ ggtitle("Marital Status by Quartile")
# plot
# ggsave("Word_Outputs/11.Household_Heads/2.Marital_Status_By_Quartile.png")

qnames<- levels(dat$TVA_quartiles)
prop_rat<- nrow(dat)

dat$HouseholdType<- factor(dat$HouseholdType, levels = c('man_single', 'woman_single', 'couple', 'polygamous'))

number_relation_types<- length(unique(as.character(dat$HouseholdType)))

colours<- c('darkred', 'orange', 'darkgreen', 'darkblue')

 png("Word_Outputs/11.Household_Heads/1.Household_details.png", height = 800, width = 900)
 par(mfrow=c(2,2), cex=1, mar=c(5,3,3,3))
# barplot(table(dat$HouseholdType)/prop_rat*100, main="Household head marital status",  names.arg=c("couple", "sinlge\nman", "single\nwoman"), ylab = "% of households")
 barplot(table(dat$HouseholdType)/prop_rat*100, main="Household head marital status",   ylab = "% of households")
 barplot(prop.table(table(dat$HouseholdType,dat$TVA_quartiles), margin=2)*100, legend.text = levels(dat$HouseholdType), args.legend = list(x= "right"), xlab="Quartiles", ylab="% of households", names.arg = qnames, main="Marital Status by Quartile", las=2, col = colours[c(1:number_relation_types)])
 aaa<-hist(dat_all$age_malehead[dat_all$age_malehead<100], breaks=seq(0,100,by=10), plot = F)
 plot(aaa,ylab="% of households", main="Age of Male Head", col="gray", las=2, xlab = 'Age')
 #barplot(aaa$counts/prop_rat*100, names.arg=c("0-10", "11-20", "21-30", "31-40","41-50","51-60","61-70","71-80","81-90"), ylab="% of households", main="Age of Male Head", col="gray", las=2)
 aaa<-hist(dat_all$age_femalehead[dat_all$age_femalehead<100],  plot=F, breaks=seq(0,100,by=10))
 #barplot(aaa$counts/prop_rat*100, names.arg=c("0-10", "11-20", "21-30", "31-40","41-50","51-60","61-70","71-80","81-90"), ylab="% of households", main="Age of Female Head", col="gray", las=2)
 plot(aaa,ylab="% of households", main="Age of Female Head", col="gray", las=2, xlab = 'Age')
 
 dev.off()
dat$Head_EducationLevel <-factor(dat$Head_EducationLevel, levels=c("No_school", "adult_education", "koranic_school", "primary","secondary","postsecondary"))

png("Word_Outputs/11.Household_Heads/2.Head_Education.png")
par(cex=1, mar=c(10,3,3,3))
barplot(table(dat$Head_EducationLevel)/prop_rat*100, main="Education level of Household Head", las=2, ylab="% of households")
dev.off()

write.csv(round(rbind(t(table(dat$Head_EducationLevel, dat$TVA_quartiles))/prop_rat*100, colSums(t(table(dat$Head_EducationLevel, dat$TVA_quartiles))/prop_rat*100)),2), "Word_Outputs/11.Household_Heads/2.Education_table.csv")



