library(tidyverse)
library(data.table)
library(ROCR)
library(neuralnet)

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


activation_functions=c('logisitc','tahn')
error_functions=c('sse','ce')
algorithms=c('backprop','rprop+','rprop-','sag','slr')
learning_rates=c(.00001,.0001,.001,.01)
num_trials =10

mean_AUC<-0
for(j in 1:10){
AUC<-0
  for (i in 1:num_trials){
  AUC[i]=fraud_ann(j,hidden_vector=c(32,8,5),"logistic",learning_rate =NULL,algo = 'rprop+',trans_data)
  }
print(j)
mean_AUC[j]=mean(AUC)
  }

# AUC<-0
# for (i in 1:num_trials){
#   AUC[i]=fraud_ann(5,hidden_vector=c(32,8,5),"logistic",learning_rate =NULL,algo = 'rprop+',trans_data)
# }






