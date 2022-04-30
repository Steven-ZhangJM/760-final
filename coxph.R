setwd("D:/Mary/22spring/760/project")
library("readxl")
library(survival)
library(survminer)
data=read_excel("OppScrData.xlsx")
data=data[,1:52]
clinical_col=1:14
outcome_col=16:40
CT_col=42:52
CT_data=data[,CT_col]
for (i in 1:ncol(CT_data)){
  CT_data[,i][is.na(CT_data[,i])]<-mean(unlist(CT_data[!is.na(CT_data[,i]),i]))
  CT_data[,i]=as.numeric(unlist(CT_data[,i]))
}
death=as.numeric(unlist(data['DEATH [d from CT]']))
status=rep(0,length(death))
time=rep(0,length(death))
for (i in 1:length(death)){
  if (is.na(death[i])){
    status[i]=1
    time[i]=as.numeric(unlist(data[i,4]))
  }else{
    status[i]=2
    time[i]=death[i]
  }
}
CT_surv=as.data.frame(CT_data)
CT_surv$time=time+0
CT_surv$status=status+0
o=order(CT_surv$time)
CT_surv=CT_surv[o,]
CL_data=data[,clinical_col]
CL_data=CL_data[,-c(1,2,3,6,14)]
CL_data[is.na(CL_data[,5]),5]<-'No'
CL_data[!is.na(CL_data[,6]),6]<-'Yes'
CL_data[is.na(CL_data[,6]),6]<-'No'
CL_data$sex=factor(CL_data$Sex)
CL_data$tobacco=factor(CL_data$Tobacco)
CL_data$alcohol=factor(CL_data$`Alcohol abuse`)
CL_data[CL_data[,7]=='<1%',7]<-'0.01'
CL_data[CL_data[,7]=='>30%',7]<-'0.3'
CL_data[CL_data[,7]=='X',7]<-'0'
CL_data=CL_data[,-c(3,5,6)]
for (i in 1:ncol(CL_data)){
  CL_data[,i]=as.numeric(unlist(CL_data[,i]))
  CL_data[,i][is.na(CL_data[,i])]<-mean(unlist(CL_data[!is.na(CL_data[,i]),i]))
}
CL_CT_surv=cbind(CL_data,CT_surv)
trainind=sample(1:nrow(CL_CT_surv),8000)

colnames(CL_CT_surv)=c('Clinical_day','BMI','Age','FRS_10year_risk','FRAX_10y_fx','FRAX_10y_Hip','Sex','Tobacco','Alchohol','L1_HU_BMD','TAT','Total_body_area','VAT','SAT','VAT_SAT',
                    'Muscle_HU','Muscle_Area','L3_SMI','AoCa','Liver','time','status')

#obj=coxph(Surv(time,status)~L1_HU_BMD+TAT+Total_body_area+VAT+SAT+VAT_SAT+
#            Muscle_HU+Muscle_Area+L3_SMI+AoCa+Liver,data=CT_surv[trainind,])
#obj=coxph(Surv(time,status)~L1_HU_BMDTotal_body_area+VAT+
#            Muscle_HU+Muscle_Area+AoCa+Liver,data=CT_surv[trainind,])
obj=coxph(Surv(time,status)~Clinical_day+BMI+Age+FRS_10year_risk+FRAX_10y_fx+FRAX_10y_Hip+Sex+Tobacco+Alchohol+
            L1_HU_BMD+TAT+Total_body_area+VAT+SAT+VAT_SAT+
            Muscle_HU+Muscle_Area+L3_SMI+AoCa+Liver,data=CL_CT_surv[trainind,])

summary(obj)
#x <- survfit(obj,newdata=CT_surv[-trainind,])
#summary(x)$table
#plot(x)