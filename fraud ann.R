

fraud_ann=function(class_ratio,hidden_vector,activation_function,learning_rate,algo,trans_data){
  #library(neuralnet)
  
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
  
  library(neuralnet)
  n=names(scaled)
  
  f=as.formula(paste("FRD_IND ~", paste(n[!n %in% "FRD_IND"], collapse = " + ")))
  
  nn=neuralnet(f,data=scaled,hidden=hidden_vector,act.fct = activation_function,
               learningrate=learning_rate,algorithm=algo,linear.output=F)
  
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
  
  return(auc)
  
}