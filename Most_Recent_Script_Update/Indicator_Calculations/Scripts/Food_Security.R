# Food Security Script

# A script for food shortage and HFIAS

#### Food Shortage Months and HFIAS ####
#'SEARCH_WorstFoodSecMonth
WorstFoodSecMonth<- dat_all$food_worst_month
#SEARCH_BestFoodSecMonth
BestFoodSecMonth<- dat_all$food_best_month

#Calculating the number of months food insecure

#SEARCH_NrofMonthsFoodInsecure
Months_with_Food_Shortage<- dat_all$foodshortagetime_months_which #A single column containing multiple factors (i.e each month they are food insecure)
Months_with_Food_Shortage_SPLIT<-List_to_True_False(as.character(Months_with_Food_Shortage)) #Splitting the single column into multiple columns (TRUE/FALSE statements for each month)
Months_with_Food_Shortage_SPLIT$`NA`<- NULL #Removing the column "NA"
NrofMonthsFoodInsecure<- rowSums(Months_with_Food_Shortage_SPLIT) #Tallying the number of months of insecurity

# Coverting PPI scores to PPI likelihood based on the PPI conversion tab;e
score_PPI<- rowSums(PPI_score_data)

COUNTRY_CODE<- gsub('_.*','', project_ID)


#SEARCH_PPI_Threshold
#SEARCH_PPI_Likelihood

PPI_Likelihood<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))
PPI_Threshold<- data.frame(matrix(NA, ncol=1, nrow=nrow(dat_all)))

for (i in 1:nrow(dat_all))
{
  x<-as.numeric(score_PPI[i])
  
  y<-paste0("PPI_Likelihood_", COUNTRY_CODE)
  if(!is.na(x)==TRUE && sum(grepl(paste0(y), colnames(PPI_Scorecard))>0))
  {
    PPI_Likelihood[i,1]<- as.numeric(PPI_Scorecard[PPI_Scorecard$Score==x,grepl(paste0(y), colnames(PPI_Scorecard))])
    PPI_Threshold[i,1]<- as.numeric(PPI_Threshold_Sheet[PPI_Threshold_Sheet$Country==COUNTRY_CODE,grepl(paste0('PPI'), colnames(PPI_Threshold_Sheet))])
    
  }
}

#HFIAS CALCULATION, 1 point for weekly or daily consumption
#SEARCH_HFIAS_status

if (length(grep("HFIAS_", colnames(dat_all)))==9)
{
  
  HFIAS_status<-array('0',nrow(dat_all))
  for ( index in 1:nrow(dat_all))
  {
    if  (dat_all$HFIAS_9[index]%in%c('daily','weekly','monthly')|dat_all$HFIAS_8[index]%in%c('daily','weekly','monthly')|dat_all$HFIAS_7[index]%in%c('daily','weekly','monthly')) {
      HFIAS_status[index]<-'SeverelyFI'
    } else { 
      if (dat_all$HFIAS_6[index]=='daily'|dat_all$HFIAS_5[index]=='daily') {
        HFIAS_status[index]<-'SeverelyFI'
      }
      if (dat_all$HFIAS_6[index]%in%c('weekly','monthly')|dat_all$HFIAS_5[index]%in%c('weekly','monthly')) {
        HFIAS_status[index]<-'ModeratelyFI'
      }
    }
    if (HFIAS_status[index]=='0') {
      if (dat_all$HFIAS_4[index]%in%c('daily','weekly')|dat_all$HFIAS_3[index]%in%c('daily','weekly')) {
        HFIAS_status[index]<-'ModeratelyFI'
      }
      if (dat_all$HFIAS_4[index]%in%c('monthly')|dat_all$HFIAS_3[index]%in%c('monthly')) {
        HFIAS_status[index]<-'MildlyFI'
      }
    }
    if (HFIAS_status[index]=='0') {
      if (dat_all$HFIAS_2[index]%in%c('monthly','daily','weekly')) {
        HFIAS_status[index]<-'MildlyFI'
      }
    }
    if (HFIAS_status[index]=='0') {
      if (dat_all$HFIAS_1[index]%in%c('daily','weekly')) {
        HFIAS_status[index]<-'MildlyFI'
      }
    }
    if (HFIAS_status[index]=='0') {
      HFIAS_status[index]<-'FoodSecure'
    }
  }
}

#SEARCH_FIES_Score
if (length(grep("FIES", colnames(dat_all)))>0)
{
  FIES_data<- data.frame(dat_all[,grep("FIES_", colnames(dat_all))])
  FIES_data <- data.frame(lapply(FIES_data, function (x) gsub("Y", 1,x)), stringsAsFactors=FALSE)
  FIES_data <- data.frame(lapply(FIES_data, function (x) gsub("N", 0,x)), stringsAsFactors=FALSE)
  FIES_data <- data.frame(lapply(FIES_data, as.numeric), stringsAsFactors=FALSE)
  
  FIES_Score<- rowSums(FIES_data, na.rm=T)
}

