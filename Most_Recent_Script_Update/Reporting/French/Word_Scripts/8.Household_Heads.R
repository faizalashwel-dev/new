# HH_heads_df<-data.frame(table(dat$HouseholdType))
# 
# png("Word_Outputs/8.Chef_de_Menage/1.HH_Heads_Marital.png")
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
# ggsave("Word_Outputs/8.Chef_de_Menage/2.Marital_Status_By_Quartile.png")

qnames<- levels(dat$TVA_quartiles)
prop_rat<- nrow(dat)


dat$HouseholdType<- gsub('woman_single', 'femme_célibataire', dat$HouseholdType)
dat$HouseholdType<- gsub('man_single', 'homme_célibataire', dat$HouseholdType)
dat$HouseholdType<- gsub('couple', 'couple', dat$HouseholdType)
dat$HouseholdType<- gsub('polygamous', 'polygame', dat$HouseholdType)


dat$HouseholdType<- factor(dat$HouseholdType, levels = c('homme_célibataire', 'femme_célibataire', 'couple', 'polygame'))





number_relation_types<- length(unique(as.character(dat$HouseholdType)))

colours<- c('darkred', 'orange', 'darkgreen', 'darkblue')

 png("Word_Outputs/8.Chef_de_Menage/1.Details_dse_Menages.png", height = 800, width = 900)
 par(mfrow=c(2,2), cex=1, mar=c(5,3,3,3))
# barplot(table(dat$HouseholdType)/prop_rat*100, main="Household head marital status",  names.arg=c("couple", "sinlge\nman", "single\nwoman"), ylab = "% of households")
 barplot(table(dat$HouseholdType)/prop_rat*100, main="État civil de Chef de Ménage",   ylab = "% des ménage")
 barplot(prop.table(table(dat$HouseholdType,dat$TVA_quartiles), margin=2)*100, legend.text = levels(dat$HouseholdType), args.legend = list(x= "right"), xlab="Quartiles", ylab="% des ménages", names.arg = qnames, main="État Civil par Quartile", las=2, col = colours[c(1:number_relation_types)])
 aaa<-hist(dat_all$age_malehead[which(dat_all$age_malehead<100&&dat_all$age_malehead>0)], breaks=seq(0,100,by=10), plot = F)
 plot(aaa,ylab="% des ménage", main="Âge de Chef Mâle", col="gray", las=2, xlab = 'Âge')
 #barplot(aaa$counts/prop_rat*100, names.arg=c("0-10", "11-20", "21-30", "31-40","41-50","51-60","61-70","71-80","81-90"), ylab="% of households", main="Age of Male Head", col="gray", las=2)
 aaa<-hist(dat_all$age_femalehead[which(dat_all$age_femalehead<100&&dat_all$age_femalehead>0)],  plot=F, breaks=seq(0,100,by=10))
 #barplot(aaa$counts/prop_rat*100, names.arg=c("0-10", "11-20", "21-30", "31-40","41-50","51-60","61-70","71-80","81-90"), ylab="% of households", main="Age of Female Head", col="gray", las=2)
 plot(aaa,ylab="% des ménage", main="Âge de Chef Femelle", col="gray", las=2, xlab = 'Âge')
 
 dev.off()
 

dat$Head_EducationLevel<-gsub('No_school','Pas_d_Ecole',dat$Head_EducationLevel)
dat$Head_EducationLevel<-gsub('adult_education','Education_des_Adultes',dat$Head_EducationLevel)
dat$Head_EducationLevel<-gsub('koranic_school','Ecole_Coranique',dat$Head_EducationLevel)
dat$Head_EducationLevel<-gsub('primary','Ecole_Primaire',dat$Head_EducationLevel)
dat$Head_EducationLevel<-gsub('secondary','Ecole_Secondaire',dat$Head_EducationLevel)
dat$Head_EducationLevel<-gsub('postsecondary','Ecole_Postsecondaire',dat$Head_EducationLevel)

 
dat$Head_EducationLevel <-factor(dat$Head_EducationLevel, levels=c("Pas_d_Ecole", "Education_des_Adultes", "Ecole_Coranique", "Ecole_Primaire","Ecole_Secondaire","Ecole_Postsecondaire"))

png("Word_Outputs/8.Chef_de_Menage/2.Graphique_Education_de_Chef_de_Menage.png")
par(cex=1, mar=c(10,3,3,3))
barplot(table(dat$Head_EducationLevel)/prop_rat*100, main="Niveau d'Éducation\ndu Chef de Ménage", las=2, ylab="% de Ménages")
dev.off()

write.csv(round(t(table(dat$Head_EducationLevel, dat$TVA_quartiles))/prop_rat*100,2), "Word_Outputs/8.Chef_de_Menage/2.Graphique_Education_de_Chef_de_Menage.csv")



