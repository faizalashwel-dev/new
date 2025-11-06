
#####  this script requires the data to be read in with mutiple choice questions in single column format #####




#### Define the core column names ####

# dat <- dat_all_mother
shortened_column_names <- function(dat){

 core_cols <- colnames(dat)
 
 
 #### Shorten column names from Core 1.3 mother file #####
 
 core_cols_new<-core_cols
 b=array(0,20)
 for (i in 1:length(core_cols)) {
   a<-unlist(strsplit(core_cols[i], "[.]"))
   #test for repeats
   b[1]<-grepl('repeat.1.',core_cols[i], fixed=TRUE)
   b[2]<-grepl('repeat.2.',core_cols[i], fixed=TRUE)
   b[3]<-grepl('repeat.3.',core_cols[i], fixed=TRUE)
   b[4]<-grepl('repeat.4.',core_cols[i], fixed=TRUE)
   b[5]<-grepl('repeat.5.',core_cols[i], fixed=TRUE)
   b[6]<-grepl('repeat.6.',core_cols[i], fixed=TRUE)
   b[7]<-grepl('repeat.7.',core_cols[i], fixed=TRUE)
   b[8]<-grepl('repeat.8.',core_cols[i], fixed=TRUE)
   b[9]<-grepl('repeat.9.',core_cols[i], fixed=TRUE)
   b[10]<-grepl('repeat.10.',core_cols[i], fixed=TRUE)
   
   b[11]<-grepl('repeat.11.',core_cols[i], fixed=TRUE)
   b[12]<-grepl('repeat.12.',core_cols[i], fixed=TRUE)
   b[13]<-grepl('repeat.13.',core_cols[i], fixed=TRUE)
   b[14]<-grepl('repeat.14.',core_cols[i], fixed=TRUE)
   b[15]<-grepl('repeat.15.',core_cols[i], fixed=TRUE)
   b[16]<-grepl('repeat.16.',core_cols[i], fixed=TRUE)
   b[17]<-grepl('repeat.17.',core_cols[i], fixed=TRUE)
   b[18]<-grepl('repeat.18.',core_cols[i], fixed=TRUE)
   b[19]<-grepl('repeat.19.',core_cols[i], fixed=TRUE)
   b[20]<-grepl('repeat.20.',core_cols[i], fixed=TRUE)
   if (length(which((tolower(as.character(b))=='1')))>0) {
     core_cols_new[i]<-paste0(a[length(a)],'_',as.character(which(tolower(as.character(b))=='1')))
   } else {
     core_cols_new[i]<-paste0(a[length(a)])
   }
 }
 
colnames(dat) <- core_cols_new

return(colnames(dat))

}








