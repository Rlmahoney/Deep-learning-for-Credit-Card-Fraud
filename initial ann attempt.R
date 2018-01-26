library(tidyverse)
library(data.table)

setwd("~/School Work/Capstone Project")
trans_data <- read.table('training_part_01_of_10.txt',sep="|",nrows=300000)




colnames(trans_data) = c('AUTH_ID', 'ACCT_ID_TOKEN', 'FRD_IND', 'ACCT_ACTVN_DT', 'ACCT_AVL_CASH_BEFORE_AMT',
                     'ACCT_AVL_MONEY_BEFORE_AMT', 'ACCT_CL_AMT', 'ACCT_CURR_BAL', 'ACCT_MULTICARD_IND', 
                     'ACCT_OPEN_DT', 'ACCT_PROD_CD', 'ACCT_TYPE_CD', 'ADR_VFCN_FRMT_CD', 'ADR_VFCN_RESPNS_CD', 
                     'APPRD_AUTHZN_CNT', 'APPRD_CASH_AUTHZN_CNT', 'ARQC_RSLT_CD', 'AUTHZN_ACCT_STAT_CD', 'AUTHZN_AMT', 
                     'AUTHZN_CATG_CD', 'AUTHZN_CHAR_CD', 'AUTHZN_OPSET_ID', 'AUTHZN_ORIG_SRC_ID', 'AUTHZN_OUTSTD_AMT', 
                     'AUTHZN_OUTSTD_CASH_AMT', 'AUTHZN_RQST_PROC_CD', 'AUTHZN_RQST_PROC_DT', 'AUTHZN_RQST_PROC_TM', 
                     'AUTHZN_RQST_TYPE_CD', 'AUTHZN_TRMNL_PIN_CAPBLT_NUM', 'AVG_DLY_AUTHZN_AMT', 'CARD_VFCN_2_RESPNS_CD', 
                     'CARD_VFCN_2_VLDTN_DUR', 'CARD_VFCN_MSMT_REAS_CD', 'CARD_VFCN_PRESNC_CD', 'CARD_VFCN_RESPNS_CD', 
                     'CARD_VFCN2_VLDTN_CD', 'CDHLDR_PRES_CD', 'CRCY_CNVRSN_RT', 'ELCTR_CMRC_IND_CD', 
                     'HOME_PHN_NUM_CHNG_DUR', 'HOTEL_STAY_CAR_RENTL_DUR', 'LAST_ADR_CHNG_DUR', 'LAST_PLSTC_RQST_REAS_CD', 
                     'MRCH_CATG_CD', 'MRCH_CNTRY_CD', 'NEW_USER_ADDED_DUR', 'PHN_CHNG_SNC_APPN_IND', 'PIN_BLK_CD', 
                     'PIN_VLDTN_IND', 'PLSTC_ACTVN_DT', 'PLSTC_ACTVN_REQD_IND', 'PLSTC_FRST_USE_TS', 'PLSTC_ISU_DUR', 
                     'PLSTC_PREV_CURR_CD', 'PLSTC_RQST_TS', 'POS_COND_CD', 'POS_ENTRY_MTHD_CD', 'RCURG_AUTHZN_IND', 
                     'RVRSL_IND', 'SENDR_RSIDNL_CNTRY_CD', 'SRC_CRCY_CD', 'SRC_CRCY_DCML_PSN_NUM', 'TRMNL_ATTNDNC_CD', 
                     'TRMNL_CAPBLT_CD', 'TRMNL_CLASFN_CD', 'TRMNL_ID', 'TRMNL_PIN_CAPBLT_CD', 'DISTANCE_FROM_HOME')



##Paramters #########
#####################

class_ratio=9 #the class ratio will be the number of non fraudulent transactions per fraudulent transaction in the training set 
activation_function="logistic"
hidden_vector=c(32,12,5)

#create training set with the desired ratio of fraud to not fraud 
fraud_samples=trans_data[trans_data$FRD_IND=="Y",]
nonfraud=trans_data[trans_data$FRD_IND=="N",]

num_frauds=nrow(fraud_samples)



sample_nonfrauds=nonfraud[sample(nrow(nonfraud),num_frauds*class_ratio),]


#combine into 1 dataframe 
training_data=rbind.data.frame(fraud_samples,sample_nonfrauds)




training_data$FRD_IND=ifelse(training_data$FRD_IND=="Y",1,0)


##select only numeric columns 
nums=sapply(training_data, is.numeric)

test_df=training_data[ , nums]

#scale the data 

maxs=apply(test_df,2,max)
mins=apply(test_df,2,min)

scaled=as.data.frame(scale(test_df,center=mins,scale=maxs-mins))

scaled=within(scaled, rm('SRC_CRCY_DCML_PSN_NUM'))


# some descriptive statistics
#apply(data,2,function(x) sum(is.na(x)))

library(neuralnet)
n=names(scaled)

f=as.formula(paste("FRD_IND ~", paste(n[!n %in% "FRD_IND"], collapse = " + ")))

nn=neuralnet(f,data=scaled,hidden=hidden_vector,act.fct = activation_function,linear.output=F)

plot(nn)

#make predictions 
pr.nn <- compute(nn,scaled[,-3])

nn.probs=pr.nn$net.result


#compute AUC 

detach(package:neuralnet,unload = T)

library(ROCR)
nn.pred = prediction(nn.probs, scaled$FRD_IND)

#pref <- performance(nn.pred, "tpr", "fpr")

perf=performance(nn.pred,"auc",fpr.stop=0.5)

auc=perf@y.values[[1]]

print(auc)


#apply(scaled,2,function(x) sum(is.na(x)))


#


