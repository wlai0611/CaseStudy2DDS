 
trainTest=function(data,split)   
{TrainObs = sample(seq(1,dim(data)[1]),round(split*dim(data)[1]),replace = FALSE)
list ( train = data[TrainObs,],
       test = data[-TrainObs,])}
mse=function(yobs,yhat)
{
  MSPE = data.frame(Observed = yobs, Predicted = yhat)
  MSPE$Resisdual = MSPE$Observed - MSPE$Predicted
  MSPE$SquaredResidual = MSPE$Resisdual^2
  mean(MSPE$SquaredResidual,na.rm = TRUE)
}
monteCarloMSE=function(data,response,formula,numMSPEs=20,split=0.75){
  
  MSPEHolderModel1 = numeric(numMSPEs)
  
  
  for(i in 1:numMSPEs)
  {
 #   TrainObs = sample(seq(1,dim(data)[1]),round(split*dim(data)[1]),replace = FALSE)
  #  train = data[TrainObs,]
   #test = data[-TrainObs,]
    
    trainTestOut=trainTest(data,split)
    train=trainTestOut$train
    test=trainTestOut$test
    
    Model1_fit = lm(formula, data = train)
    Model1_Preds = predict(Model1_fit, newdata = test)
    Model1_MSE=mse(as.numeric(test[,response]),Model1_Preds)
    MSPEHolderModel1[i] =  Model1_MSE
    
  }
  mean(MSPEHolderModel1)
}  
#lm(x=data[1:3], y=data[4]....)
#fit = lm(mpg ~ ., data = mtcars)
#summary(fit)
CV=function(data,split){
  TrainObs = sample(seq(1,dim(data)[1]),round(split*dim(data)[1]),replace = FALSE)
  train=data[TrainObs,]
}



cvMse=function(data,response,formula,numMSPEs=20,split=0.75){
  
  #factorNames=data %>% keep(is.factor) %>% discard(function(x) length(unique(x))==1) %>% names
  MSPEHolderModel1 = numeric(numMSPEs)
  
  
  for(i in 1:numMSPEs)
  { #group by all factors
    #onlyFac=subdf[,names(subdf) %in% factorNames]
    
    data = data %>% mutate(indices = seq(1:nrow(data)) )
    train=data %>% group_by_if(is.factor) %>% group_modify( ~ CV(.x,split=0.5))
    test=data[-train$indices,]  
    data=data[,names(data)!="indices"] 
    
    Model1_fit = lm(formula, data = train)
    Model1_Preds = predict(Model1_fit, newdata = test)
    Model1_MSE=mse(as.numeric(test[,response]),Model1_Preds)
    MSPEHolderModel1[i] =  Model1_MSE
    
  }
  mean(MSPEHolderModel1)
}  

