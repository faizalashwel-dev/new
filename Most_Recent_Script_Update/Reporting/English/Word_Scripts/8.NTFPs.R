prop_rat<-nrow(dat)

dat_all$manage_trees <- gsub("Only_felling", "Only Felling", dat_all$manage_trees)
dat_all$manage_trees <- factor(dat_all$manage_trees, ordered = T, levels= c("Y", "N", "Only Felling"))
png("Word_Outputs/8.NTFPs/1.manage_trees_on_farm.png"); par(cex=1.4)
barplot(table(dat_all$manage_trees)[table(dat_all$manage_trees)>0]/prop_rat*100, main="Manage Trees on Farm?", ylab="% of households")
dev.off()

mange_trees_temp<-data.frame(table(dat_all$manage_trees)[table(dat_all$manage_trees)>0]/prop_rat*100)
mange_trees_temp<- data.frame("Manage/Use Trees?"=mange_trees_temp[,1], "Percentage of HHs"=round(mange_trees_temp[,2],1))
write.csv(mange_trees_temp, "Word_Outputs/8.NTFPs/1.manage_trees_on_farm_table.csv", row.names = F)


tree_uses<- List_to_True_False(dat_all$tree_uses)
png("Word_Outputs/8.NTFPs/2.Tree_Uses.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(colSums(tree_uses, na.rm=T)/prop_rat*100, main="Uses of trees", las=2, ylab="% of households")
dev.off()

Tree_Use_temp<-data.frame(colSums(tree_uses, na.rm=T))
Tree_Use_temp[,1]<-round(100*Tree_Use_temp[,1]/nrow(dat),1)
Tree_Use<- data.frame("Use"=row.names(Tree_Use_temp), "Percentage of HHs"=Tree_Use_temp[,1])
write.csv(Tree_Use, "Word_Outputs/8.NTFPs/2.Tree_Use_Table.csv", row.names = F)


food_trees_owned_df<-List_to_True_False(dat_all$food_trees_which)

Food_Trees_most_important<-List_to_True_False(dat_all$food_trees_which)
Food_Trees_most_important<-data.frame(colSums(Food_Trees_most_important, na.rm = T))
Food_Trees_most_important[,1]<-round(100*Food_Trees_most_important[,1]/nrow(dat_all),1)
Food_Trees_most_important<-data.frame("Food Tree Used"=row.names(Food_Trees_most_important), "Percentage of HHs Using"=Food_Trees_most_important[,1]) 
Food_Trees_most_important<-Food_Trees_most_important[order(Food_Trees_most_important[,2], decreasing = TRUE),]
Food_Trees_most_important<-Food_Trees_most_important[-grep("other", Food_Trees_most_important$Food.Tree.Used),]

png("Word_Outputs/8.NTFPs/3.Food_Trees_Owned.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(Food_Trees_most_important[,2], main="Reported Food Trees",names.arg = Food_Trees_most_important[,1], las=2, ylab="% of households")
dev.off()


write.csv(Food_Trees_most_important, "Word_Outputs/8.NTFPs/3.Food_Trees_Owned_Table.csv", row.names = F)




NTFP_most_important<-List_to_True_False(dat_all$gather_NTFP)
NTFP_most_important<-data.frame(colSums(NTFP_most_important, na.rm=T))
NTFP_most_important[,1]<-round(100*NTFP_most_important[,1]/nrow(dat_all),1)
NTFP_most_important<-data.frame("NTFP Gathered"=row.names(NTFP_most_important), "Percentage of HHs Gathering"=NTFP_most_important[,1]) 
NTFP_most_important<-NTFP_most_important[order(NTFP_most_important[,2], decreasing = TRUE),]

png("Word_Outputs/8.NTFPs/4.NTFP_Summary_Graph.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(NTFP_most_important[,2], main="Reported_NTFPs_Collected",names.arg = NTFP_most_important[,1], las=2, ylab="% of households")
dev.off()


write.csv(NTFP_most_important, "Word_Outputs/8.NTFPs/4.NTFP_Summary_Table.csv", row.names = F)

NTFP_Incomes<-dat_NTFP_incomes
NTFP_Incomes<- data.frame(lapply(NTFP_Incomes, function (x) as.numeric(as.character(x))))
colnames(NTFP_Incomes)<-gsub("Income_", "", colnames(NTFP_Incomes))

Median_NTFP_Incomes<-data.frame(t(data.frame(lapply(NTFP_Incomes, function(x) round(median(x, na.rm = T),2)))))
Median_NTFP_Incomes<- data.frame("NTFP"=row.names(Median_NTFP_Incomes), "Median Income"=Median_NTFP_Incomes[,1])
IQR_NTFP_Incomes<-data.frame(t(data.frame(lapply(NTFP_Incomes, function(x) round(IQR(x, na.rm = T),2)))))

Income_True_False<- data.frame(lapply(NTFP_Incomes, function(x) x>0))
Income_True_False<- colSums(Income_True_False, na.rm = T)
Income_True_False<- data.frame(t(data.frame(lapply(Income_True_False, function(x) round(100*x/nrow(dat),2)))))
Income_True_False<- data.frame("NTFP"=row.names(Income_True_False), "Percentage of HHs Getting Income"=Income_True_False[,1])

png("Word_Outputs/8.NTFPs/5.NTFP_Income.png")
par(mar=c(9,4,4,2), cex=1.2)
barplot(Median_NTFP_Incomes[,2], main="Median NTFP Income",names.arg = Median_NTFP_Incomes[,1], las=2, ylab="Income $/Year")
dev.off()

NTFP_income_summary<-data.frame(Median_NTFP_Incomes, "IQR"=IQR_NTFP_Incomes[,1])

write.csv(NTFP_income_summary, "Word_Outputs/8.NTFPs/5.NTFP_Income_Table.csv", row.names = F)


png("Word_Outputs/8.NTFPs/6.NTFP_Income_True_False.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(Income_True_False[,2], main="Percentage of HHs Getting Income from NTFPs",names.arg = Income_True_False[,1], las=2, ylab="Percentage of HHs")
dev.off()


write.csv(Median_NTFP_Incomes, "Word_Outputs/8.NTFPs/6.NTFP_Income_True_False_Table.csv", row.names = F)



NTFP_collected_All<-List_to_True_False(dat_all$gather_NTFP)
NTFP_collected_All$TVA_Quartile<-dat$TVA_Quartile

NTFP_collected_lower<-NTFP_collected_All[NTFP_collected_All$TVA_Quartile=="lowest",]
Number_in_quartile<-nrow(NTFP_collected_lower)
NTFP_collected_lower<-NTFP_collected_lower[,-grep("TVA", colnames(NTFP_collected_lower))]
NTFP_collected_lower<-data.frame(colSums(NTFP_collected_lower,na.rm=T))
NTFP_collected_lower[,1]<-round(100*NTFP_collected_lower[,1]/Number_in_quartile,1)
NTFP_collected_lower<-data.frame("NTFP Gathered"=row.names(NTFP_collected_lower), "Percentage of HHs Gathering"=NTFP_collected_lower[,1]) 
NTFP_collected_lower<-NTFP_collected_lower[order(NTFP_collected_lower[,2], decreasing = TRUE),]
NTFP_collected_lower$quartile<- rep("lowest", nrow(NTFP_collected_lower))
  
NTFP_collected_lower_middle<-NTFP_collected_All[NTFP_collected_All$TVA_Quartile=="lower middle",]
Number_in_quartile<-nrow(NTFP_collected_lower_middle)
NTFP_collected_lower_middle<-NTFP_collected_lower_middle[,-grep("TVA", colnames(NTFP_collected_lower_middle))]
NTFP_collected_lower_middle<-data.frame(colSums(NTFP_collected_lower_middle,na.rm=T))
NTFP_collected_lower_middle[,1]<-round(100*NTFP_collected_lower_middle[,1]/Number_in_quartile,1)
NTFP_collected_lower_middle<-data.frame("NTFP Gathered"=row.names(NTFP_collected_lower_middle), "Percentage of HHs Gathering"=NTFP_collected_lower_middle[,1]) 
NTFP_collected_lower_middle<-NTFP_collected_lower_middle[order(NTFP_collected_lower_middle[,2], decreasing = TRUE),]
NTFP_collected_lower_middle$quartile<- rep("lower middle", nrow(NTFP_collected_lower_middle))


NTFP_collected_upper_middle<-NTFP_collected_All[NTFP_collected_All$TVA_Quartile=="upper middle",]
Number_in_quartile<-nrow(NTFP_collected_upper_middle)
NTFP_collected_upper_middle<-NTFP_collected_upper_middle[,-grep("TVA", colnames(NTFP_collected_upper_middle))]
NTFP_collected_upper_middle<-data.frame(colSums(NTFP_collected_upper_middle,na.rm=T))
NTFP_collected_upper_middle[,1]<-round(100*NTFP_collected_upper_middle[,1]/Number_in_quartile,1)
NTFP_collected_upper_middle<-data.frame("NTFP Gathered"=row.names(NTFP_collected_upper_middle), "Percentage of HHs Gathering"=NTFP_collected_upper_middle[,1]) 
NTFP_collected_upper_middle<-NTFP_collected_upper_middle[order(NTFP_collected_upper_middle[,2], decreasing = TRUE),]
NTFP_collected_upper_middle$quartile<- rep("upper middle", nrow(NTFP_collected_upper_middle))


NTFP_collected_upper<-NTFP_collected_All[NTFP_collected_All$TVA_Quartile=="upper",]
Number_in_quartile<-nrow(NTFP_collected_upper)
NTFP_collected_upper<-NTFP_collected_upper[,-grep("TVA", colnames(NTFP_collected_upper))]
NTFP_collected_upper<-data.frame(colSums(NTFP_collected_upper,na.rm=T))
NTFP_collected_upper[,1]<-round(100*NTFP_collected_upper[,1]/Number_in_quartile,1)
NTFP_collected_upper<-data.frame("NTFP Gathered"=row.names(NTFP_collected_upper), "Percentage of HHs Gathering"=NTFP_collected_upper[,1]) 
NTFP_collected_upper<-NTFP_collected_upper[order(NTFP_collected_upper[,2], decreasing = TRUE),]
NTFP_collected_upper$quartile<- rep("upper", nrow(NTFP_collected_upper))

NTFP_collected_Whole_pop<-NTFP_collected_All
Number_in_quartile<-nrow(NTFP_collected_Whole_pop)
NTFP_collected_Whole_pop<-NTFP_collected_Whole_pop[,-grep("TVA", colnames(NTFP_collected_Whole_pop))]
NTFP_collected_Whole_pop<-data.frame(colSums(NTFP_collected_Whole_pop,na.rm=T))
NTFP_collected_Whole_pop[,1]<-round(100*NTFP_collected_Whole_pop[,1]/Number_in_quartile,1)
NTFP_collected_Whole_pop<-data.frame("NTFP Gathered"=row.names(NTFP_collected_Whole_pop), "Percentage of HHs Gathering"=NTFP_collected_Whole_pop[,1]) 
NTFP_collected_Whole_pop<-NTFP_collected_Whole_pop[order(NTFP_collected_Whole_pop[,2], decreasing = TRUE),]
NTFP_collected_Whole_pop$quartile<- rep("all", nrow(NTFP_collected_Whole_pop))

NTFP_collected_by_quartile<-rbind(NTFP_collected_lower, NTFP_collected_lower_middle, NTFP_collected_upper_middle, NTFP_collected_upper, NTFP_collected_Whole_pop)
NTFP_collected_by_quartile$quartile<- factor(NTFP_collected_by_quartile$quartile, levels = c("lowest", "lower middle", "upper middle", "upper", "all"))

plot<-ggplot(NTFP_collected_by_quartile, aes(x=NTFP.Gathered, y=Percentage.of.HHs.Gathering, fill=quartile))
plot<-plot+geom_bar(stat="identity", position = "dodge")
plot<-plot+theme_minimal()

plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
plot<- plot+ ggtitle("NTFP Collection by Quartile")
plot
ggsave("Word_Outputs/8.NTFPs/7.NTFP_Collection_by_Quartile.png")


write.csv(NTFP_collected_by_quartile, "Word_Outputs/8.NTFPs/7.NTFP_Collection_by_Quartile_Table.csv", row.names = F)


#### NTFP Incomes by Quartile ####
NTFP_income_All<-NTFP_Incomes
NTFP_income_All$TVA_Quartile<- dat$TVA_Quartile

NTFP_income_lower<-NTFP_income_All[NTFP_income_All$TVA_Quartile=="lowest",]
Number_in_quartile<-nrow(NTFP_income_lower)
NTFP_income_lower<-NTFP_income_lower[,-grep("TVA", colnames(NTFP_income_lower))]
NTFP_income_lower<-data.frame(t(data.frame(lapply(NTFP_income_lower, function(x) median(x,na.rm = T)))))
NTFP_income_lower<-data.frame("NTFP Gathered"=row.names(NTFP_income_lower), "Income dollar/year"=NTFP_income_lower[,1]) 
#NTFP_income_lower<-NTFP_income_lower[order(NTFP_income_lower[,2], decreasing = TRUE),]
NTFP_income_lower$quartile<- rep("lowest", nrow(NTFP_income_lower))

NTFP_income_lower_middle<-NTFP_income_All[NTFP_income_All$TVA_Quartile=="lower middle",]
Number_in_quartile<-nrow(NTFP_income_lower_middle)
NTFP_income_lower_middle<-NTFP_income_lower_middle[,-grep("TVA", colnames(NTFP_income_lower_middle))]
NTFP_income_lower_middle<-data.frame(t(data.frame(lapply(NTFP_income_lower_middle, function(x) median(x,na.rm = T)))))
NTFP_income_lower_middle<-data.frame("NTFP Gathered"=row.names(NTFP_income_lower_middle), "Income dollar/year"=NTFP_income_lower_middle[,1]) 
#NTFP_income_lower_middle<-NTFP_income_lower_middle[order(NTFP_income_lower_middle[,2], decreasing = TRUE),]
NTFP_income_lower_middle$quartile<- rep("lower middle", nrow(NTFP_income_lower_middle))

NTFP_income_upper_middle<-NTFP_income_All[NTFP_income_All$TVA_Quartile=="upper middle",]
Number_in_quartile<-nrow(NTFP_income_upper_middle)
NTFP_income_upper_middle<-NTFP_income_upper_middle[,-grep("TVA", colnames(NTFP_income_upper_middle))]
NTFP_income_upper_middle<-data.frame(t(data.frame(lapply(NTFP_income_upper_middle, function(x) median(x,na.rm = T)))))
NTFP_income_upper_middle<-data.frame("NTFP Gathered"=row.names(NTFP_income_upper_middle), "Income dollar/year"=NTFP_income_upper_middle[,1]) 
#NTFP_income_upper_middle<-NTFP_income_upper_middle[order(NTFP_income_upper_middle[,2], decreasing = TRUE),]
NTFP_income_upper_middle$quartile<- rep("upper middle", nrow(NTFP_income_upper_middle))

NTFP_income_upper<-NTFP_income_All[NTFP_income_All$TVA_Quartile=="upper",]
Number_in_quartile<-nrow(NTFP_income_upper)
NTFP_income_upper<-NTFP_income_upper[,-grep("TVA", colnames(NTFP_income_upper))]
NTFP_income_upper<-data.frame(t(data.frame(lapply(NTFP_income_upper, function(x) median(x,na.rm = T)))))
NTFP_income_upper<-data.frame("NTFP Gathered"=row.names(NTFP_income_upper), "Income dollar/year"=NTFP_income_upper[,1]) 
#NTFP_income_upper<-NTFP_income_upper[order(NTFP_income_upper[,2], decreasing = TRUE),]
NTFP_income_upper$quartile<- rep("upper", nrow(NTFP_income_upper))

NTFP_income_Whole_pop<-NTFP_income_All
Number_in_quartile<-nrow(NTFP_income_Whole_pop)
NTFP_income_Whole_pop<-NTFP_income_Whole_pop[,-grep("TVA", colnames(NTFP_income_Whole_pop))]
NTFP_income_Whole_pop<-data.frame(t(data.frame(lapply(NTFP_income_Whole_pop, function(x) median(x,na.rm = T)))))
NTFP_income_Whole_pop[,1]<-round(100*NTFP_income_Whole_pop[,1]/Number_in_quartile,1)
NTFP_income_Whole_pop<-data.frame("NTFP Gathered"=row.names(NTFP_income_Whole_pop), "Income dollar/year"=NTFP_income_Whole_pop[,1]) 
#NTFP_income_Whole_pop<-NTFP_income_Whole_pop[order(NTFP_income_Whole_pop[,2], decreasing = TRUE),]
NTFP_income_Whole_pop$quartile<- rep("all", nrow(NTFP_income_Whole_pop))

NTFP_income_by_quartile<-rbind(NTFP_income_lower, NTFP_income_lower_middle, NTFP_income_upper_middle, NTFP_income_upper, NTFP_income_Whole_pop)
NTFP_income_by_quartile$quartile<- factor(NTFP_income_by_quartile$quartile, levels = c("lowest", "lower middle", "upper middle", "upper", "all"))


plot<-ggplot(NTFP_income_by_quartile, aes(x=NTFP.Gathered, y=Income.dollar.year, fill=quartile))
plot<-plot+geom_bar(stat="identity", position = "dodge")
plot<-plot+theme_minimal()

plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
plot<- plot+ ggtitle("NTFP Income by Quartile")
plot
ggsave("Word_Outputs/8.NTFPs/8.NTFP_Incomes_by_Quartile.png")

write.csv(NTFP_income_by_quartile, "Word_Outputs/8.NTFPs/8.NTFP_Incomes_by_Quartile_Table.csv", row.names = F)



#### NTFP Energies ####
Energy_Per_NTFP<- dat_NTFP_energies
Energy_Per_NTFP<-data.frame(lapply(Energy_Per_NTFP, function(x) as.numeric(as.character(x))/dat$HHsizeMAE))
colnames(Energy_Per_NTFP)<-gsub("Energy_", "", colnames(Energy_Per_NTFP))


NTFP_Energy_All<-Energy_Per_NTFP
NTFP_Energy_All$TVA_Quartile<- dat$TVA_Quartile

NTFP_Energy_lower<-NTFP_Energy_All[NTFP_Energy_All$TVA_Quartile=="lowest",]
Number_in_quartile<-nrow(NTFP_Energy_lower)
NTFP_Energy_lower<-NTFP_Energy_lower[,-grep("TVA", colnames(NTFP_Energy_lower))]
NTFP_Energy_lower<-data.frame(t(data.frame(lapply(NTFP_Energy_lower, function(x) median(x,na.rm = T)))))
NTFP_Energy_lower<-data.frame("NTFP Gathered"=row.names(NTFP_Energy_lower), "Energy (kcal/MAE/day)"=NTFP_Energy_lower[,1]) 
#NTFP_Energy_lower<-NTFP_Energy_lower[order(NTFP_Energy_lower[,2], decreasing = TRUE),]
NTFP_Energy_lower$quartile<- rep("lowest", nrow(NTFP_Energy_lower))

NTFP_Energy_lower_middle<-NTFP_Energy_All[NTFP_Energy_All$TVA_Quartile=="lower middle",]
Number_in_quartile<-nrow(NTFP_Energy_lower_middle)
NTFP_Energy_lower_middle<-NTFP_Energy_lower_middle[,-grep("TVA", colnames(NTFP_Energy_lower_middle))]
NTFP_Energy_lower_middle<-data.frame(t(data.frame(lapply(NTFP_Energy_lower_middle, function(x) median(x,na.rm = T)))))
NTFP_Energy_lower_middle<-data.frame("NTFP Gathered"=row.names(NTFP_Energy_lower_middle), "Energy (kcal/MAE/day)"=NTFP_Energy_lower_middle[,1]) 
#NTFP_Energy_lower_middle<-NTFP_Energy_lower_middle[order(NTFP_Energy_lower_middle[,2], decreasing = TRUE),]
NTFP_Energy_lower_middle$quartile<- rep("lower middle", nrow(NTFP_Energy_lower_middle))

NTFP_Energy_upper_middle<-NTFP_Energy_All[NTFP_Energy_All$TVA_Quartile=="upper middle",]
Number_in_quartile<-nrow(NTFP_Energy_upper_middle)
NTFP_Energy_upper_middle<-NTFP_Energy_upper_middle[,-grep("TVA", colnames(NTFP_Energy_upper_middle))]
NTFP_Energy_upper_middle<-data.frame(t(data.frame(lapply(NTFP_Energy_upper_middle, function(x) median(x,na.rm = T)))))
NTFP_Energy_upper_middle<-data.frame("NTFP Gathered"=row.names(NTFP_Energy_upper_middle), "Energy (kcal/MAE/day)"=NTFP_Energy_upper_middle[,1]) 
#NTFP_Energy_upper_middle<-NTFP_Energy_upper_middle[order(NTFP_Energy_upper_middle[,2], decreasing = TRUE),]
NTFP_Energy_upper_middle$quartile<- rep("upper middle", nrow(NTFP_Energy_upper_middle))

NTFP_Energy_upper<-NTFP_Energy_All[NTFP_Energy_All$TVA_Quartile=="upper",]
Number_in_quartile<-nrow(NTFP_Energy_upper)
NTFP_Energy_upper<-NTFP_Energy_upper[,-grep("TVA", colnames(NTFP_Energy_upper))]
NTFP_Energy_upper<-data.frame(t(data.frame(lapply(NTFP_Energy_upper, function(x) median(x,na.rm = T)))))
NTFP_Energy_upper<-data.frame("NTFP Gathered"=row.names(NTFP_Energy_upper), "Energy (kcal/MAE/day)"=NTFP_Energy_upper[,1]) 
#NTFP_Energy_upper<-NTFP_Energy_upper[order(NTFP_Energy_upper[,2], decreasing = TRUE),]
NTFP_Energy_upper$quartile<- rep("upper", nrow(NTFP_Energy_upper))

NTFP_Energy_Whole_pop<-NTFP_Energy_All
Number_in_quartile<-nrow(NTFP_Energy_Whole_pop)
NTFP_Energy_Whole_pop<-NTFP_Energy_Whole_pop[,-grep("TVA", colnames(NTFP_Energy_Whole_pop))]
NTFP_Energy_Whole_pop<-data.frame(t(data.frame(lapply(NTFP_Energy_Whole_pop, function(x) median(x,na.rm = T)))))
NTFP_Energy_Whole_pop[,1]<-round(100*NTFP_Energy_Whole_pop[,1]/Number_in_quartile,1)
NTFP_Energy_Whole_pop<-data.frame("NTFP Gathered"=row.names(NTFP_Energy_Whole_pop), "Energy (kcal/MAE/day)"=NTFP_Energy_Whole_pop[,1]) 
#NTFP_Energy_Whole_pop<-NTFP_Energy_Whole_pop[order(NTFP_Energy_Whole_pop[,2], decreasing = TRUE),]
NTFP_Energy_Whole_pop$quartile<- rep("all", nrow(NTFP_Energy_Whole_pop))



NTFP_Energy_by_quartile<-rbind(NTFP_Energy_lower, NTFP_Energy_lower_middle, NTFP_Energy_upper_middle, NTFP_Energy_upper, NTFP_Energy_Whole_pop)
NTFP_Energy_by_quartile$quartile<- factor(NTFP_Energy_by_quartile$quartile, levels = c("lowest", "lower middle", "upper middle", "upper", "all"))

colnames(NTFP_Energy_by_quartile)<- c("NTFP Gathered", "Energy (kcal/MAE/day)", "Quartile")

plot<-ggplot(NTFP_Energy_by_quartile, aes(x=`NTFP Gathered`, y=`Energy (kcal/MAE/day)`, fill=Quartile))
plot<-plot+geom_bar(stat="identity", position = "dodge")
plot<-plot+theme_minimal()

plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
plot<- plot+ ggtitle("NTFP Energy by Quartile")
plot
ggsave("Word_Outputs/8.NTFPs/9.NTFP_Energy_by_Quartile.png")

write.csv(NTFP_Energy_by_quartile, "Word_Outputs/8.NTFPs/9.NTFP_Energy_by_Quartile_Table.csv", row.names = F)

write.csv(Energy_Per_NTFP, "Word_Outputs/8.NTFPs/10.NTFP_Energy_Table.csv", row.names = F)

png("Word_Outputs/8.NTFPs/10.NTFP_Energy.png")
par(mar=c(7,4,4,2), cex=1.4)
boxplot(Energy_Per_NTFP, main="NTFPs Gathered", las=2, ylab="Energy (kcal/MAE/day)", outline=F)
dev.off()

#### Wildfoods ####

Wild_foods_gather<- data.frame(round(100*prop.table(table(dat_all$gather_wildfoods)), 2))
colnames(Wild_foods_gather)<- c("Gather_Wildfoods", "% of HHs")

png("Word_Outputs/8.NTFPs/11.Gather_Wild_Foods.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(Wild_foods_gather[,-which(colnames(Wild_foods_gather)=="Gather_Wildfoods")], main="% of HHs Gathering Wildfoods",  ylab="% of HHs", names.arg = Wild_foods_gather$Gather_Wildfoods)
dev.off()

write.csv(Wild_foods_gather,"Word_Outputs/8.NTFPs/11.Gather_Wild_Foods_table.csv" , row.names = F)



Wild_foods_all<-List_to_True_False(dat_all$wildfoods)
Wild_foods_all<-data.frame(colSums(Wild_foods_all, na.rm=T))
Wild_foods_all[,1]<-round(100*Wild_foods_all[,1]/nrow(dat_all),1)
Wild_foods_all<-data.frame("Wildfoods Gathered"=row.names(Wild_foods_all), "Percentage of HHs Gathering"=Wild_foods_all[,1]) 
Wild_foods_all<-Wild_foods_all[order(Wild_foods_all[,2], decreasing = TRUE),]

png("Word_Outputs/8.NTFPs/12.Wildfoods_Summary.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(Wild_foods_all[,2], main="Wildfoods Gathered",names.arg = Wild_foods_all[,1], las=2, ylab="% of households")
dev.off()

colnames(Wild_foods_all)<- c("Wildfood Gathered", "% of HHs Gathering")
write.csv(Wild_foods_all, "Word_Outputs/8.NTFPs/12.Wildfoods_Summary_Table.csv", row.names = F)

wildfood_collected_when<- List_to_True_False(dat_all$wildfood_collect_when)
wildfood_collected_when<-wildfood_collected_when[,as.character(months[which(months%in%colnames(wildfood_collected_when))])]
wildfood_collected_when<-colSums(wildfood_collected_when,na.rm=T)
wildfood_collected_when_ordered <- c(rep(0,12))
names(wildfood_collected_when_ordered) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in 1:length(wildfood_collected_when)) {
  pos <- which(names(wildfood_collected_when)[i] == names(wildfood_collected_when_ordered))
  wildfood_collected_when_ordered[pos]<-wildfood_collected_when[i]
}

wildfood_collected_when_ordered<-data.frame(t(data.frame(lapply(wildfood_collected_when_ordered, function(x) round(100*x/nrow(dat),2)))))

png("Word_Outputs/8.NTFPs/13.Wildfoods_Collect_When.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1.4)
barplot(wildfood_collected_when_ordered[,1], las=2,names.arg = row.names(wildfood_collected_when_ordered), main="Wildfoods_Collected_When", ylab="% of households")
dev.off()

wildfood_collected_when_ordered<- data.frame(row.names(wildfood_collected_when_ordered), wildfood_collected_when_ordered[,1])
colnames(wildfood_collected_when_ordered)<- c("Month", "% of HHs Collecting Wildfoods")

write.csv(wildfood_collected_when_ordered,"Word_Outputs/8.NTFPs/13.Wildfoods_Collect_When_table.csv", row.names=F)




# uses_shea_All<- List_to_True_False(dat_all$shea_use)
# uses_shea_All$TVA_Quartile<- dat$TVA_Quartiles
# 
# uses_shea_lower<-uses_shea_All[uses_shea_All$TVA_Quartile=="lowest",]
# Number_in_quartile<-nrow(uses_shea_lower)
# uses_shea_lower<-uses_shea_lower[,-grep("TVA", colnames(uses_shea_lower))]
# uses_shea_lower<-data.frame(colSums(uses_shea_lower,na.rm=T))
# uses_shea_lower[,1]<-round(100*uses_shea_lower[,1]/Number_in_quartile,1)
# uses_shea_lower<-data.frame("Use"=row.names(uses_shea_lower), "Percentage of HHs"=uses_shea_lower[,1]) 
# uses_shea_lower<-uses_shea_lower[order(uses_shea_lower[,2], decreasing = TRUE),]
# uses_shea_lower$quartile<- rep("lowest", nrow(uses_shea_lower))
# 
# uses_shea_lower_middle<-uses_shea_All[uses_shea_All$TVA_Quartile=="lower middle",]
# Number_in_quartile<-nrow(uses_shea_lower_middle)
# uses_shea_lower_middle<-uses_shea_lower_middle[,-grep("TVA", colnames(uses_shea_lower_middle))]
# uses_shea_lower_middle<-data.frame(colSums(uses_shea_lower_middle,na.rm=T))
# uses_shea_lower_middle[,1]<-round(100*uses_shea_lower_middle[,1]/Number_in_quartile,1)
# uses_shea_lower_middle<-data.frame("Use"=row.names(uses_shea_lower_middle), "Percentage of HHs"=uses_shea_lower_middle[,1]) 
# uses_shea_lower_middle<-uses_shea_lower_middle[order(uses_shea_lower_middle[,2], decreasing = TRUE),]
# uses_shea_lower_middle$quartile<- rep("lower middle", nrow(uses_shea_lower_middle))
# 
# 
# uses_shea_upper_middle<-uses_shea_All[uses_shea_All$TVA_Quartile=="upper middle",]
# Number_in_quartile<-nrow(uses_shea_upper_middle)
# uses_shea_upper_middle<-uses_shea_upper_middle[,-grep("TVA", colnames(uses_shea_upper_middle))]
# uses_shea_upper_middle<-data.frame(colSums(uses_shea_upper_middle,na.rm=T))
# uses_shea_upper_middle[,1]<-round(100*uses_shea_upper_middle[,1]/Number_in_quartile,1)
# uses_shea_upper_middle<-data.frame("Use"=row.names(uses_shea_upper_middle), "Percentage of HHs"=uses_shea_upper_middle[,1]) 
# uses_shea_upper_middle<-uses_shea_upper_middle[order(uses_shea_upper_middle[,2], decreasing = TRUE),]
# uses_shea_upper_middle$quartile<- rep("upper middle", nrow(uses_shea_upper_middle))
# 
# 
# uses_shea_upper<-uses_shea_All[uses_shea_All$TVA_Quartile=="upper",]
# Number_in_quartile<-nrow(uses_shea_upper)
# uses_shea_upper<-uses_shea_upper[,-grep("TVA", colnames(uses_shea_upper))]
# uses_shea_upper<-data.frame(colSums(uses_shea_upper,na.rm=T))
# uses_shea_upper[,1]<-round(100*uses_shea_upper[,1]/Number_in_quartile,1)
# uses_shea_upper<-data.frame("Use"=row.names(uses_shea_upper), "Percentage of HHs"=uses_shea_upper[,1]) 
# uses_shea_upper<-uses_shea_upper[order(uses_shea_upper[,2], decreasing = TRUE),]
# uses_shea_upper$quartile<- rep("upper", nrow(uses_shea_upper))
# 
# uses_shea_by_quartile<-rbind(uses_shea_lower, uses_shea_lower_middle, uses_shea_upper_middle, uses_shea_upper)
# uses_shea_by_quartile$quartile<- factor(uses_shea_by_quartile$quartile, levels = c("lowest", "lower middle", "upper middle", "upper"))
# 
# plot<-ggplot(uses_shea_by_quartile, aes(x=Use, y=Percentage.of.HHs, fill=quartile))
# plot<-plot+geom_bar(stat="identity", position = "dodge")
# plot<-plot+theme_minimal()
# 
# plot<-plot+theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))
# plot<- plot+ ggtitle("Shea Use by Quartile")
# plot
# ggsave("Word_Outputs/8.NTFPs/17.shea_use_by_quartile.png")
# 
# 
# 


##### #####
#### ADD FIREWOOD AND TREES ###

Woody_Materials<-List_to_True_False(dat_all$gathered_materials)
Woody_Materials<-data.frame(colSums(Woody_Materials, na.rm=T))
Woody_Materials[,1]<-round(100*Woody_Materials[,1]/nrow(dat_all),1)
Woody_Materials<-data.frame("Non_Edible_Forest_Product"=row.names(Woody_Materials), "Percentage_of_HHs_Gathering"=Woody_Materials[,1]) 
Woody_Materials<-Woody_Materials[order(Woody_Materials[,2], decreasing = TRUE),]

png("Word_Outputs/8.NTFPs/14.Non_Edible_Forest_Product_Gathered.png")
par(mar=c(7,4,4,2), cex=1.4)
barplot(Woody_Materials[,2], main="Reported Non-Edible\nForest Product Gathered",names.arg = Woody_Materials[,1], las=2, ylab="% of households")
dev.off()

colnames(Woody_Materials)<- gsub("_", " ", colnames(Woody_Materials))
write.csv(Woody_Materials, "Word_Outputs/8.NTFPs/14.Non_Edible_Forest_Product_Gathered_table.csv", row.names = F)






  