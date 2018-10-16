############################################
# First set the directory and import files #
############################################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

department<-read.csv('department.csv',header=T)
diagnosis<-read.csv('diagnosis.csv',header=T)
med_order<-read.csv('medication_order.csv',header=T)
visit_diagnosis<-read.csv('visit_diagnosis.csv',header=T)
visit<-read.csv('visit.csv',header=T)

############################################
# Part 1 #
############################################

#First, remove unneeded variables
visit<-visit[,-c(5,15:18)]
visit_diagnosis<-visit_diagnosis[,-c(10:13)]

#Start by merging the visit and diagnosis tables

visit83<-merge(visit,visit_diagnosis,by=c('VISIT_KEY','PAT_KEY'),all.x=TRUE)




#1.1 Subset to Hospital Encounter
visit83<-visit83[visit83$DICT_ENC_TYPE_KEY==83,]

#1.2 Subset to Hospital Encounters AFTER August 1st, 2014
visit83<-visit83[visit83$CONTACT_DT_KEY>20140801,]

#1.3 Subset patient age to be between 1 and 18

#Note: There are 17 values equal to 1 and 1 value equal to 18, so I interpreted "between" as inclusive
visit83<-visit83[visit83$AGE>=1 & visit83$AGE<=18,]
row.names(visit83) <- 1:nrow(visit83)

#1.4 Patient received diagnosis of anaphylaxis or allergic reaction


#To match ICD9 codes, I will first remove missing ICD9 codes from
#Diagnosis table
diagnosis_NM<-diagnosis
diagnosis_NM$ICD9_CD<-as.character(diagnosis_NM$ICD9_CD)
diagnosis_NM<-diagnosis_NM[is.na(diagnosis_NM$ICD9_CD)==0,]
row.names(diagnosis_NM) <- 1:nrow(diagnosis_NM)

#Because of duplicated ICD9_CD values for the same DX_KEY, I will instead
#extract rows with matching DX_KEY values and subset the visit table using DX_KEY

ana.code<-c(
  995.0,
  995.3,
  995.6,
  995.60,
  995.61,
  995.62,
  995.63,
  995.64,
  995.65,
  995.66,
  995.67,
  995.68,
  995.69,
  995.7,
  999.4,
  999.41,
  999.42,
  999.49)

ana.code<-as.character(ana.code)


diagnosis_ana<-diagnosis_NM[diagnosis_NM$ICD9_CD %in% ana.code,]

#Now subset visit83 table with matching DX_KEY

visit83<-visit83[visit83$DX_KEY %in% diagnosis_ana$DX_KEY,]



#1.5 Subset to ED visits only (313, 314), removing Urgent Care

visit83<-visit83[visit83$DICT_DX_STS_KEY==313 | visit83$DICT_DX_STS_KEY==314,]






############################################
# Part 2 #
############################################

#First merge with medications
med_pat<-med_order[,c(1,7,11)]
med.merge<-merge(visit83,med_pat,by=c('PAT_KEY','VISIT_KEY'),all.x=TRUE)

#2.1 Create ANAPH_DX_IND
#I do this by matching DX_KEY from the diagnosis table
DX_ana<-diagnosis_ana[grep('anaphylaxis',tolower(diagnosis_ana$DX_NM)),3]

med.merge$ANAPH_DX_IND<-rep(0,nrow(med.merge))

med.merge[DX_ana %in% med.merge$DX_KEY,ncol(med.merge)]<-1


#2.2 Create EPI_ORDER_IND

med.merge$EPI_ORDER_IND<-rep(0,nrow(med.merge))

med.merge[grep('epinephrine',tolower(med.merge$MED_ORD_NM)),ncol(med.merge)]<-1

#Quick cleanup

Ep<-aggregate(EPI_ORDER_IND ~ VISIT_KEY, med.merge ,max)
An<-aggregate(ANAPH_DX_IND ~ VISIT_KEY, med.merge ,max)


med.merge<-med.merge[,-c(16:24)]
med.merge<-unique(med.merge[,])

med.merge<-merge(med.merge,Ep,by='VISIT_KEY',all.x=TRUE)
med.merge<-merge(med.merge,An,by='VISIT_KEY',all.x=TRUE)




#2.3 Create FOLLOW_UP_IND

#First, I change dates to proper date format
med.merge$HOSP_DISCHRG_DT<-as.POSIXct(med.merge$HOSP_DISCHRG_DT,format='%Y-%m-%d %H:%M:%S')
med.merge$HOSP_ADMIT_DT<-as.POSIXct(med.merge$HOSP_ADMIT_DT,format='%Y-%m-%d %H:%M:%S')

#From the original visit table, I subset outpatient facilities to merge
visit108<-visit[visit$DICT_ENC_TYPE_KEY==108,]
visit108$OUT_APPT<-as.POSIXct(visit108$APPT_DT,format='%Y-%m-%d %H:%M:%S')
visit108<-visit108[visit108$PAT_KEY %in% med.merge$PAT_KEY,c(2,15)]
rownames(visit108)<-1:nrow(visit108)

final.merge<-merge(med.merge,visit108,by='PAT_KEY',all.x=TRUE)


#Remove outpatient visits BEFORE ED encounter


for(i in 1:nrow(final.merge)){
  if(is.na(final.merge$OUT_APPT[i])){
    final.merge$OUT_APPT[i]<-NA
  }else if(final.merge$OUT_APPT[i]<=final.merge$HOSP_DISCHRG_DT[i]){
    final.merge$OUT_APPT[i]<-NA
  }else{
    final.merge$OUT_APPT[i]<=final.merge$OUT_APPT[i]
  }
}



#Now with so many follow-up appointments, it will be important to only take the first after encounter

follow<-final.merge[,c(2,18)]


follow<-aggregate(OUT_APPT ~VISIT_KEY,follow,min)
colnames(follow)<-c('VISIT_KEY','FOLLOW_UP_DATE')

final.merge<-final.merge[,-ncol(final.merge)]
final.merge<-unique(final.merge[,])
final.merge<-merge(final.merge,follow,by='VISIT_KEY',all.x=TRUE)      


#Now to create FOLLOW_UP_IND

final.merge$FOLLOW_UP_IND<-rep(9,nrow(final.merge))

for(i in 1:nrow(final.merge)){
  if(is.na(as.Date(final.merge$FOLLOW_UP_DATE[i])) | is.na(as.Date(final.merge$HOSP_DISCHRG_DT[i]))){
    final.merge$FOLLOW_UP_IND[i]<-0
  }else if(as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i]) >7){
    final.merge$FOLLOW_UP_IND[i]<-0
  }else if(as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i]) ==7){
    final.merge$FOLLOW_UP_IND[i]<-2
  }else if(as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i]) < 7 & as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i]) >= 0){
    final.merge$FOLLOW_UP_IND[i]<-1
  }else if(as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i]) < 0){
    final.merge$FOLLOW_UP_IND[i]<-NA
  }
}


#The NA is coded for follow-up dates BEFORE ED experience. This is a check; there should be no NA.

#The two is coded for meeting on the 7th day. If greater than 168 hours, will be flagged. All will change to 1.

rownames(final.merge)<-1:nrow(final.merge)

final.merge$flag<-rep(NA,nrow(final.merge))

for(i in 1:nrow(final.merge)){
  if(final.merge$FOLLOW_UP_IND[i]==2 & substr(final.merge$HOSP_DISCHRG_DT[i]+60*60*168,12,19)<substr(final.merge$FOLLOW_UP_DATE[i],12,19)){
    final.merge$flag[i]<-1
  }else{
    final.merge$flag[i]<-NA
  }
}

FLAG.PT<-final.merge[which(final.merge$flag==1),1]

for(i in 1:nrow(final.merge)){
  if(final.merge$FOLLOW_UP_IND[i]==2){
    final.merge$FOLLOW_UP_IND[i]<-1
  }else{
    final.merge$FOLLOW_UP_IND[i]<-final.merge$FOLLOW_UP_IND[i]
  }
}




#2.4 Create FOLLOW_UP_DATE


#This was completed to find FOLLOW_UP_IND and reduce duplicated rows


#2.5 Create DAYS_TO_FOLLOW_UP

final.merge$DAYS_TO_FOLLOW_UP<-rep(NA,nrow(final.merge))

for(i in 1:nrow(final.merge)){
  if(final.merge$FOLLOW_UP_IND[i]==1){
    final.merge$DAYS_TO_FOLLOW_UP[i]<-as.Date(final.merge$FOLLOW_UP_DATE[i]) - as.Date(final.merge$HOSP_DISCHRG_DT[i])
  }else{
    final.merge$FOLLOW_UP_DATE[i]<-NA
  }
}




############################################
# Part 3 #
############################################

#First Only include 9 columns from full merged data set

final<-final.merge[,c(2,1,10,6,17,16,19,18,21)]


#Export CSV

write.csv(final,'visits_cohort.csv')

#NOTE FLAGGED VISIT_KEYS FOR
FLAG.PT


