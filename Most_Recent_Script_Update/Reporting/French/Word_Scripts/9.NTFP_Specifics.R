#### Collection of Specific NTFPs ####

NTFP_most_important<-List_to_True_False(dat_all$gather_NTFP)

i<-"balanites"

temp<-data.frame(prop.table(table(NTFP_most_important[,i], dat$Village), margin = 2))
temp<-temp[temp$Var1==T,]
temp$Freq<-round(temp$Freq*100,2)
temp$NTFP<- rep(i, nrow(temp))
temp$Existing_Villages<- temp$Var2%in% Existing_Villages
temp$Var1<-NULL
colnames(temp)<- c("Village", "Percentage_of_HHs", "NTFP", "Existing_Village")


plot<-ggplot(temp, aes(x=Village, y=Percentage_of_HHs, fill=Existing_Village))
plot<-plot+geom_bar(stat="identity")

plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Households Collecting ",unique(temp$NTFP))) +
  xlab("Village") + ylab("% of HHs from Village Collecting")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/1.",i,".png"))


temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/1.",i,"_table.csv"), row.names = F)




#### Incomes from specific NTFPs####
NTFP_income_village<-data.frame(NTFP_Incomes, "Village"=dat$Village)

#NTFP_income_grouped<- group_by(NTFP_income_grouped, Village)

i<-grep("balanites_fruit", colnames(NTFP_income_village))

temp<-data.frame("Village"=NTFP_income_village$Village, "Income"=NTFP_income_village[,i])

temp$NTFP<- rep(colnames(NTFP_income_village)[i], nrow(temp))
temp$Existing_Villages<- temp$Village%in% Existing_Villages

plot<-ggplot(temp, aes(x=Village, y=Income, fill=Existing_Villages))
plot<-plot+geom_boxplot(outlier.shape=NA)

plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Income from ",unique(temp$NTFP), " $/HH/Year")) +
  xlab("Village") + ylab("Income in $/HH/Year")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/2.",i,"income_.png"))

temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/2.",i,"income_table.csv"), row.names = F)


# i<-grep("balanites_nut", colnames(NTFP_income_village))
# 
# temp<-data.frame("Village"=NTFP_income_village$Village, "Income"=NTFP_income_village[,i])
# 
# temp$NTFP<- rep(colnames(NTFP_income_village)[i], nrow(temp))
# temp$Existing_Villages<- temp$Village%in% Existing_Villages
# 
# plot<-ggplot(temp, aes(x=Village, y=Income, fill=Existing_Villages))
# plot<-plot+geom_boxplot(outlier.shape=NA)
# 
# plot<-plot+theme_minimal()
# plot<-plot+ ggtitle(paste0("Income from ",unique(temp$NTFP), " $/HH/Year")) +
#   xlab("Village") + ylab("Income in $/HH/Year")
# plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot
# ggsave(paste0("Word_Outputs/9.NTFP_Specifics/2.",i,"income_.png"))
# 
# temp$NTFP<-NULL
# write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/2.",i,"income_table.csv"), row.names = F)
# 


#### NTFP energy ####
NTFP_Energy_village<-data.frame(Energy_Per_NTFP, "Village"=dat$Village)
#NTFP_income_grouped<- group_by(NTFP_income_grouped, Village)

i<-grep("balanites", colnames(NTFP_Energy_village))



temp<-data.frame("Village"=NTFP_Energy_village$Village, "Energy"=NTFP_Energy_village[,i])

temp$NTFP<- rep(colnames(NTFP_Energy_village)[i], nrow(temp))
temp$Existing_Villages<- temp$Village%in% Existing_Villages

plot<-ggplot(temp, aes(x=Village, y=Energy, fill=Existing_Villages))
plot<-plot+geom_boxplot(outlier.shape=NA)
plot<-plot+coord_cartesian(ylim=c(0,20000))
plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Energy from ",unique(temp$NTFP), " Consumption")) +
  xlab("Village") + ylab("kcal/MAE/day")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/3.",i,"energy_.png"))

temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/3.",i,"energy_table.csv"), row.names = F)

#### Baobab leaves income by Village ###

i<-grep("baobab_leaves", colnames(NTFP_income_village))

temp<-data.frame("Village"=NTFP_income_village$Village, "Income"=NTFP_income_village[,i])

temp$NTFP<- rep(colnames(NTFP_income_village)[i], nrow(temp))
temp$Existing_Villages<- temp$Village%in% Existing_Villages

plot<-ggplot(temp, aes(x=Village, y=Income, fill=Existing_Villages))
plot<-plot+geom_boxplot(outlier.shape=NA)

plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Income from ",unique(temp$NTFP), " $/HH/Year")) +
  xlab("Village") + ylab("Income in $/HH/Year")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/4.",i,"income_.png"))

temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/4.",i,"income_table.csv"), row.names = F)

#### Baobab energy income per village ####
NTFP_Energy_village<-data.frame(Energy_Per_NTFP, "Village"=dat$Village)
#NTFP_income_grouped<- group_by(NTFP_income_grouped, Village)

i<-grep("baobab_leaves", colnames(NTFP_Energy_village))



temp<-data.frame("Village"=NTFP_Energy_village$Village, "Energy"=NTFP_Energy_village[,i])

temp$NTFP<- rep(colnames(NTFP_Energy_village)[i], nrow(temp))
temp$Existing_Villages<- temp$Village%in% Existing_Villages

plot<-ggplot(temp, aes(x=Village, y=Energy, fill=Existing_Villages))
plot<-plot+geom_boxplot(outlier.shape=NA)
plot<-plot+coord_cartesian(ylim=c(0,20000))
plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Energy from ",unique(temp$NTFP), " Consumption")) +
  xlab("Village") + ylab("kcal/MAE/day")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/5.",i,"energy_.png"))

temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/5.",i,"energy_table.csv"), row.names = F)


#### Gum arabic HH collecting ####

# NTFP_most_important<-List_to_True_False(dat_all$gather_NTFP)
# 
# i<-grep("arabic_gum", colnames(NTFP_most_important))
# 
# temp<-data.frame(prop.table(table(NTFP_most_important[,i], dat$Village), margin = 2))
# temp<-temp[temp$Var1==T,]
# temp$Freq<-round(temp$Freq*100,2)
# temp$NTFP<- rep(colnames(NTFP_most_important)[i], nrow(temp))
# temp$Existing_Villages<- temp$Var2%in% Existing_Villages
# temp$Var1<-NULL
# colnames(temp)<- c("Village", "Percentage_of_HHs", "NTFP", "Existing_Villages")
# 
# 
# plot<-ggplot(temp, aes(x=Village, y=Percentage_of_HHs, fill=Existing_Villages))
# plot<-plot+geom_bar(stat="identity")
# 
# plot<-plot+theme_minimal()
# plot<-plot+ ggtitle(paste0("Households Collecting ",unique(temp$NTFP))) +
#   xlab("Village") + ylab("% of HHs from Village Collecting")
# plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot
# ggsave(paste0("Word_Outputs/9.NTFP_Specifics/6.",i,"collected_.png"))
# 
# temp$NTFP<-NULL
# write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/6.",i,"collected_table.csv"), row.names = F)
# 

#### Gumarabic Income #### 

i<-grep("baobab_leaves", colnames(NTFP_income_village))

temp<-data.frame("Village"=NTFP_income_village$Village, "Income"=NTFP_income_village[,i])

temp$NTFP<- rep(colnames(NTFP_income_village)[i], nrow(temp))
temp$Existing_Villages<- temp$Village%in% Existing_Villages

plot<-ggplot(temp, aes(x=Village, y=Income, fill=Existing_Villages))
plot<-plot+geom_boxplot(outlier.shape=NA)

plot<-plot+theme_minimal()
plot<-plot+ ggtitle(paste0("Income from ",unique(temp$NTFP), " $/HH/Year")) +
  xlab("Village") + ylab("Income in $/HH/Year")
plot<-plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
ggsave(paste0("Word_Outputs/9.NTFP_Specifics/7.",i,"income_.png"))

temp$NTFP<-NULL
write.csv(temp,paste0("Word_Outputs/9.NTFP_Specifics/7.",i,"income_table.csv"), row.names = F)

####
