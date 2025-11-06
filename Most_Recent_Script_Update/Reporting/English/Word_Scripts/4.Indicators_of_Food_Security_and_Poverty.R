

png("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/1.Welfare_Indicators_per_Quartile.png")
par(mar=c(6,3,3,3),mfrow=c(2,2),mgp=c(3,2,0))

boxplot(list((dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$NrofMonthsFoodInsecure)), 
        #outline = F,  col = "lightgray", ylab="Number of Months", main="Hungry Months", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="Number of Months", main="Hungry Months", names=c("Low", "Low-Mid", "Up-Mid", "Up"), las=2) 


boxplot(list((dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$FIES_Score)), 
      #  outline = F,  col = "lightgray", ylab="FIES Score", main="FIES Status", names=c("Low", "Low-Mid", "Up-Mid", "Up"), las=2) 
outline = F,  col = "lightgray", ylab="FIES Score", main="FIES Status", names=c("Low", "Low-Mid", "Up-Mid", "Up"), las=2) 

boxplot(list((dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_GoodSeason)), 
      #  outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Up"), las=2) 

boxplot(list((dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$score_HDDS_BadSeason)), 
       # outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Up"), las=2) 

dev.off()



####---------------
Welfare_Inds<- summarise(dat_grp, 
                              round(median(NrofMonthsFoodInsecure, na.rm=T),2), 
                              round(IQR(NrofMonthsFoodInsecure, na.rm=T),2),
                              round(median(FIES_Score, na.rm=T),2), 
                              round(IQR(FIES_Score, na.rm=T),2),
                              round(median(score_HDDS_GoodSeason, na.rm=T),2), 
                              round(IQR(score_HDDS_GoodSeason, na.rm=T),2),
                              round(median(score_HDDS_BadSeason, na.rm=T),2), 
                              round(IQR(score_HDDS_BadSeason, na.rm=T),2)
)

colnames(Welfare_Inds) <- c("Quartiles", 
                                 "Hungry Months (Median)", 
                                 "Hungry Months (IQR)",
                                 "FIES Status (Median)", 
                                 "FIES Status (IQR)",
                                 "Dietary Diversity Good Season (Median)", 
                                 "Dietary Diversity Good Season (IQR)",
                                 "Dietary Diversity Bad Season (Median)", 
                                 "Dietary Diversity Bad Season (IQR)") 
# whole_pop<- c("All", 
#               median(as.numeric(as.character(dat$NrofMonthsFoodInsecure)), na.rm=T), 
#               IQR(as.numeric(as.character(dat$NrofMonthsFoodInsecure)), na.rm=T),
#               round(median(dat$FIES_Score, na.rm=T),2), 
#               round(IQR(dat$FIES_Score, na.rm=T),2),
#               round(median(dat$score_HDDS_GoodSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_GoodSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_BadSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_BadSeason, na.rm=T),2)
# )
# 
# 
# Welfare_Inds$Quartiles<- as.character(Welfare_Inds$Quartiles)
# Welfare_Inds<- rbind(Welfare_Inds, whole_pop)
write.csv(Welfare_Inds, "Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/1.Welfare_Indicators_per_Quartile_Table.csv", row.names = F)
#----------------

FIES_ordered<-data.frame(table(dat$FIES_Score))
FIES_ordered<- data.frame("FIES_Score"=row.names(FIES_ordered), "Number of HHs"=round(FIES_ordered[,2]/nrow(dat)*100))
write.csv(FIES_ordered,"Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/2.FIES_table.csv", sep = ",", col.names = F)

png("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/2.FIES.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1)
barplot(FIES_ordered$Number.of.HHs/nrow(dat)*100,names.arg = FIES_ordered$FIES_Score, main="FIES Score", ylab="% of households")
dev.off()



dat$NrofMonthsFoodInsecure<- factor(dat$NrofMonthsFoodInsecure, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), ordered = T)

Number_of_Hungry_Months<-data.frame(round(100*prop.table(table(dat$NrofMonthsFoodInsecure)),2))
colnames(Number_of_Hungry_Months)<- c("Number of Months", "% of HHs")
png("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/3.Number_Hungry_Months.png", width = 500, height = 500)
par(mar=c(3,3,3,3),mfrow=c(1,1), cex=1.4)
barplot(Number_of_Hungry_Months$`% of HHs`, names.arg = Number_of_Hungry_Months$`Number of Months`, main="Number of Hungry Months", ylab="% of households")
dev.off()

write.csv(Number_of_Hungry_Months, "Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/3.Number_Hungry_Months_table.csv", row.names = F)



months<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

temp<- List_to_True_False(dat_all$foodshortagetime_months_which)
temp<-temp[,as.character(months[which(months%in%colnames(temp))])]
lean_months<-colSums(temp,na.rm=T)
lean_months_ordered <- c(rep(0,12))
names(lean_months_ordered) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for (i in 1:length(lean_months)) {
  pos <- which(names(lean_months)[i] == names(lean_months_ordered))
  lean_months_ordered[pos]<-lean_months[i]
}

lean_months_ordered<-data.frame(lean_months_ordered)
lean_months_ordered<- data.frame("Hungry_Months"=row.names(lean_months_ordered), "% of HHs"=round(lean_months_ordered[,1]/nrow(dat)*100))
colnames(lean_months_ordered)<- c("Month", "% of HHs")
write.csv(lean_months_ordered,"Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/4.Hungry_Months_table.csv", row.names = F)

png("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/4.Hungry_Months.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1.4)
barplot(lean_months_ordered$`% of HHs`, las=2, main="Reported Hungry Months", ylab="% of households", names.arg = lean_months_ordered$Month)
dev.off()




#--------------------------------------------------------------

png("Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/6.HDDS_per_Quartile.png")
par(mar=c(6,3,3,3),mfrow=c(2,3),mgp=c(3,2,0))

boxplot(list((dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_GoodSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

      #  outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$score_HDDS_BadSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

      #  outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_farmbasedGoodSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Farmbased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

#        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Farmbased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_farmbasedBadSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Farmbased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

       # outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Farmbased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_purchasedGoodSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Purchased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

#        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Purchased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_purchasedBadSeason)), 
        outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Purchased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper"), las=2) 

       # outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity\n Purchased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

dev.off()

HDDS_scores<- summarise(dat_grp, 
                        round(median(score_HDDS_GoodSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_GoodSeason, na.rm=T),2),
                        round(median(score_HDDS_BadSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_BadSeason, na.rm=T),2),
                        round(median(score_HDDS_farmbasedGoodSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_farmbasedGoodSeason, na.rm=T),2),
                        round(median(score_HDDS_farmbasedBadSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_farmbasedBadSeason, na.rm=T),2),
                        round(median(score_HDDS_purchasedGoodSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_purchasedGoodSeason, na.rm=T),2),
                        round(median(score_HDDS_purchasedBadSeason, na.rm=T),2), 
                        round(IQR(score_HDDS_purchasedBadSeason, na.rm=T),2)
)

HDDS_scores<-HDDS_scores[!is.na(HDDS_scores$TVA_quartiles),]
colnames(HDDS_scores) <- c("Quartiles", 
                            "Dietary Diversity Good Season (Median)", 
                            "Dietary Diversity Good Season (IQR)",
                            "Dietary Diversity Bad Season (Median)", 
                            "Dietary Diversity Bad Season (IQR)",
                            "Dietary Diversity Farmbased Good Season (Median)", 
                            "Dietary Diversity Farmbased Good Season (IQR)",
                            "Dietary Diversity Farmbased Bad Season (Median)", 
                            "Dietary Diversity Farmbased Bad Season (IQR)",
                            "Dietary Diversity Purchased Good Season (Median)", 
                            "Dietary Diversity Purchased Good Season (IQR)",
                            "Dietary Diversity Purchased Bad Season (Median)", 
                            "Dietary Diversity Purchased Bad Season (IQR)")

# whole_pop<- c("All", 
#               round(median(dat$score_HDDS_GoodSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_GoodSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_BadSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_BadSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_farmbasedGoodSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_farmbasedGoodSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_farmbasedBadSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_farmbasedBadSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_purchasedGoodSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_purchasedGoodSeason, na.rm=T),2),
#               round(median(dat$score_HDDS_purchasedBadSeason, na.rm=T),2), 
#               round(IQR(dat$score_HDDS_purchasedBadSeason, na.rm=T),2)
# )
# 
# 
# HDDS_scores$Quartiles<- as.character(HDDS_scores$Quartiles)
# HDDS_scores<- rbind(HDDS_scores, whole_pop)
write.csv(HDDS_scores, "Word_Outputs/4.Indicators_of_Food_Security_and_Poverty/6.HDDS_per_Quartile_Table.csv", row.names = F)



