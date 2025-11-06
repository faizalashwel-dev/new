

png("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/1.Graphique_Indicateur_de_Bien_Etre.png")
par(mar=c(6,3,3,3),mfrow=c(2,2),mgp=c(3,2,0))

boxplot(list((dat$NrofMonthsFoodInsecure[dat$TVA_quartiless==levels(dat$TVA_quartiles)[1]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$NrofMonthsFoodInsecure[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$NrofMonthsFoodInsecure)), 
        #outline = F,  col = "lightgray", ylab="Nombre de Mois", main="Mois de Faim", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="Nombre de Mois", main="Mois de Faim", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 


boxplot(list((dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$FIES_Score[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$FIES_Score)), 
      #  outline = F,  col = "lightgray", ylab="FIES Score", main="FIES Status", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 
outline = F,  col = "lightgray", ylab="Score FIES", main="Statut FIES", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

boxplot(list((dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_GoodSeason)), 
      #  outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire Bonne Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

boxplot(list((dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$score_HDDS_BadSeason)), 
       # outline = F,  col = "lightgray", ylab="HDDS Score", main="Dietary Diversity Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Up", "All"), las=2) 
outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire Mauvaise Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

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
                                 "Mois de Faim (Médian)", 
                                 "Mois de Faim (Gamme Interquartile)",
                                 "Score FIES (Médian)", 
                                 "Score FIES (Gamme Interquartile)",
                                 "Diversité Alimentaire Bonne Saison (Médian)", 
                                 "Diversité Alimentaire Bonne Saison (Gamme Interquartile)",
                                 "Diversité Alimentaire Mauvaise Saison (Médian)", 
                                 "Diversité Alimentaire Mauvaise Saison (Gamme Interquartile)") 
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
write.csv(Welfare_Inds, "Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/1.Indicateur_de_Bien_Etre_par_Quartile.csv", row.names = F)
#----------------

FIES_ordered<-data.frame(table(dat$FIES_Score))
FIES_ordered<- data.frame("Score_FIES"=row.names(FIES_ordered), "Nombre_de_Ménages"=round(FIES_ordered[,2]/nrow(dat)*100))
write.csv(FIES_ordered,"Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/2.FIES.csv", sep = ",", col.names = F)

png("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/2.FIES.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1)
barplot(FIES_ordered$Nombre_de_Ménages/nrow(dat)*100,names.arg = FIES_ordered$Score_FIES, main="Score FIES", ylab="% de Ménages")
dev.off()



dat$NrofMonthsFoodInsecure<- factor(dat$NrofMonthsFoodInsecure, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), ordered = T)

Number_of_Hungry_Months<-data.frame(round(100*prop.table(table(dat$NrofMonthsFoodInsecure)),2))
colnames(Number_of_Hungry_Months)<- c("Nombre de Mois", "% de Ménages")
png("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/3.Graphique_Nombre_de_Mois_Faim.png", width = 500, height = 500)
par(mar=c(3,3,3,3),mfrow=c(1,1), cex=1.4)
barplot(Number_of_Hungry_Months$`% de Ménages`, names.arg = Number_of_Hungry_Months$`Nombre de Mois`, main="Nombre de Mois de Faim", ylab="% des Ménages")
dev.off()

write.csv(Number_of_Hungry_Months, "Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/3.Nombre_de_Mois_de_Faim.csv", row.names = F)



months<-c("jan","fev","mar","avr","mai","juin","juil","août","sep","oct","nov","dec")


temp<- List_to_True_False(dat_all$foodshortagetime_months_which)
X<-colnames(temp)
X<- gsub("jan", "jan", X)
X<- gsub("feb", "fev", X)
X<- gsub("mar", "mar", X)
X<- gsub("apr", "avr", X)
X<- gsub("may", "mai", X)
X<- gsub("jun", "juin", X)
X<- gsub("jul", "juil", X)
X<- gsub("aug", "août", X)
X<- gsub("sep", "sep", X)
X<- gsub("oct", "oct", X)
X<- gsub("nov", "nov", X)
X<- gsub("dec", "dec", X)
colnames(temp)<-X

temp<-temp[,as.character(months[which(months%in%colnames(temp))])]
lean_months<-colSums(temp,na.rm=T)
lean_months_ordered <- c(rep(0,12))
names(lean_months_ordered) <- c("jan","fev","mar","avr","mai","juin","juil","août","sep","oct","nov","dec")
for (i in 1:length(lean_months)) {
  pos <- which(names(lean_months)[i] == names(lean_months_ordered))
  lean_months_ordered[pos]<-lean_months[i]
}

lean_months_ordered<-data.frame(lean_months_ordered)
lean_months_ordered<- data.frame("Mois_de_Faim"=row.names(lean_months_ordered), "Pourcentage_des_Ménage"=round(lean_months_ordered[,1]/nrow(dat)*100))
colnames(lean_months_ordered)<- c("Mois de Faim", "% des Ménages")
write.csv(lean_months_ordered,"Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/4.Moise_de_Faim.csv", row.names = F)

png("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/4.Graphique_Mois_de_Faim.png", width = 500, height = 500)
par(mfrow=c(1,1), cex=1.4)
barplot(lean_months_ordered$`% des Ménages`, las=2, main="Mois de Faim Selon les Répondent", ylab="% des Ménages", names.arg = lean_months_ordered$`Mois de Faim`)
dev.off()




#--------------------------------------------------------------

png("Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/6.Graphique_HDDS_par_Quartile.png")
par(mar=c(6,3,3,3),mfrow=c(2,3),mgp=c(3,2,0))

boxplot(list((dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_GoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_GoodSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\ Bonne Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

      #  outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\ Bonne Saison", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_BadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
            # (dat$score_HDDS_BadSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\ Mauvaise Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

      #  outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\ Mauvaise Saison", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_farmbasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_farmbasedGoodSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Provenant de la Ferme Bonne Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

#        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Farmbased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_farmbasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_farmbasedBadSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Provenant de la Ferme Mauvaise Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

       # outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Farmbased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_purchasedGoodSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
          #   (dat$score_HDDS_purchasedGoodSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Acheté Bonne Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

#        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Purchased Good Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

boxplot(list((dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[1]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[2]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[3]]), 
             (dat$score_HDDS_purchasedBadSeason[dat$TVA_quartiles==levels(dat$TVA_quartiles)[4]])),
             #(dat$score_HDDS_purchasedBadSeason)), 
        outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Acheté Mauvaise Saison", names=c("Inf","Moy-Inf","Moy-Sup","Haute"), las=2) 

       # outline = F,  col = "lightgray", ylab="Score HDDS", main="Diversité Alimentaire\n Purchased Bad Season", names=c("Low", "Low-Mid", "Up-Mid", "Upper", "All"), las=2) 

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
                            "Diversité Alimentaire Bonne Saison (Médian)", 
                            "Diversité Alimentaire Bonne Saison (Gamme Interquartile)",
                            "Diversité Alimentaire Mauvaise Saison (Médian)", 
                            "Diversité Alimentaire Mauvaise Saison (Gamme Interquartile)",
                            "Diversité Alimentaire Farmbased Bonne Saison (Médian)", 
                            "Diversité Alimentaire Farmbased Bonne Saison (Gamme Interquartile)",
                            "Diversité Alimentaire Farmbased Mauvaise Saison (Médian)", 
                            "Diversité Alimentaire Farmbased Mauvaise Saison (Gamme Interquartile)",
                            "Diversité Alimentaire Purchased Bonne Saison (Médian)", 
                            "Diversité Alimentaire Purchased Bonne Saison (Gamme Interquartile)",
                            "Diversité Alimentaire Purchased Mauvaise Saison (Médian)", 
                            "Diversité Alimentaire Purchased Mauvaise Saison (Gamme Interquartile)")

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
write.csv(HDDS_scores, "Word_Outputs/4.Indicateur_de_Bien_Etre_et_de_Securite_Alimentaire/6.HDDS_par_Quartile.csv", row.names = F)



