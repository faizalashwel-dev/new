dat$score_HDDS_farmbasedGoodSeason<-as.numeric(as.character(dat$score_HDDS_farmbasedGoodSeason))
dat$score_HDDS_farmbasedBadSeason<-as.numeric(as.character(dat$score_HDDS_farmbasedBadSeason))
dat$score_HDDS_purchasedGoodSeason<-as.numeric(as.character(dat$score_HDDS_purchasedGoodSeason))
dat$score_HDDS_purchasedBadSeason<-as.numeric(as.character(dat$score_HDDS_purchasedBadSeason))
dat$score_HDDS_BadSeason<- as.numeric(as.character(dat$score_HDDS_BadSeason))
dat$score_HDDS_GoodSeason<- as.numeric(as.character(dat$score_HDDS_GoodSeason))

dat_all$age_malehead<-as.numeric(dat_all$age_malehead)
dat_all$age_femalehead<-as.numeric(dat_all$age_femalehead)

dat$NrofMonthsFoodInsecure<- as.numeric(as.character(dat$NrofMonthsFoodInsecure))
#dat$FIES_Status<- as.numeric(as.character(dat$FIES_Status))
dat$LandOwned<- as.numeric(as.character(dat$LandOwned))
dat$LandCultivated<- as.numeric(as.character(dat$LandCultivated))
dat$HHsizemembers<- as.numeric(as.character(dat$HHsizemembers))
dat$HHsizeMAE<- as.numeric(as.character(dat$HHsizeMAE))

dat$LivestockHoldings<- as.numeric(as.character(dat$LivestockHoldings))
dat$livestockprodsales<- as.numeric(as.character(dat$livestockprodsales))
dat$cropsales<- as.numeric(as.character(dat$cropsales))
dat$FAEnergyCropSales<- as.numeric(as.character(dat$FAEnergyCropSales))
dat$FoodAvailability<- as.numeric(as.character(dat$FoodAvailability))
dat$FAEnergyLivestockConsumption<- as.numeric(as.character(dat$FAEnergyLivestockConsumption))
dat$FAEnergyLivestockSales<- as.numeric(as.character(dat$FAEnergyLivestockSales))
dat$FAEnergyCropConsumption<- as.numeric(as.character(dat$FAEnergyCropConsumption))
dat$FAEnergyCropSales<- as.numeric(as.character(dat$FAEnergyCropSales))
dat$FAEnergyOffFarm<- as.numeric(as.character(dat$FAEnergyOffFarm))
dat$FAEnergyBought<- as.numeric(as.character(dat$FAEnergyBought))


dat$offfarm_income<- as.numeric(as.character(dat$offfarm_income))
dat$CropDiv<- as.numeric(as.character(dat$CropDiv))
dat$LivestockDiv<- as.numeric(as.character(dat$LivestockDiv))
dat$GHGEmissions<- as.numeric(as.character(dat$GHGEmissions))
dat$TotalEnergyAvailable<- as.numeric(as.character(dat$TotalEnergyAvailable))


dat$total_income<- as.numeric(as.character(dat$total_income))
dat$TVA_pmae_pday<- as.numeric(as.character(dat$TVA_pmae_pday))
dat$FoodSelfSufficiency<- as.numeric(as.character(dat$FoodSelfSufficiency))
dat$FAEnergyFarmBased<- as.numeric(as.character(dat$FAEnergyFarmBased))
dat$FAMarketOrientation<- as.numeric(as.character(dat$FAMarketOrientation))
dat$FALivestockOrientation<- as.numeric(as.character(dat$FALivestockOrientation))
dat$NrofMonthsWildFoodCons<- as.numeric(as.character(dat$NrofMonthsWildFoodCons))
dat$Gender_MaleControl<- as.numeric(as.character(dat$Gender_MaleControl))
dat$Gender_FemaleControl<- as.numeric(as.character(dat$Gender_FemaleControl))
dat$Gender_MaleYouthControl<- as.numeric(as.character(dat$Gender_MaleYouthControl))
dat$Gender_FemaleYouthControl<- as.numeric(as.character(dat$Gender_FemaleYouthControl))
dat$NFertInput<- as.numeric(as.character(dat$NFertInput))
#dat$Local_Currency_to_international_PPP<- as.numeric(as.character(dat$Local_Currency_to_international_PPP))



# dat$value_NTFP_consumed<- as.numeric(as.character(dat$value_NTFP_consumed))
# dat$NTFP_Income<- as.numeric(as.character(dat$NTFP_Income))
# dat$FAEnergy_NTFP_Consumed<- as.numeric(as.character(dat$FAEnergy_NTFP_Consumed))
# dat$FAEnergy_NTFP_Sold<- as.numeric(as.character(dat$FAEnergy_NTFP_Sold))
# 



#dat$TVA_Quartile <- factor(dat$TVA_Quartile, levels=c("lowest","lower middle","upper middle","upper"))












colnames(dat)