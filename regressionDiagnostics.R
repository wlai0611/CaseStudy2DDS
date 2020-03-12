#takes a dataframe "DF" of the entire dataset
#takes a dataframe "SUBDF" containing only the model's SELECTED explanatory variables and the response variable
#takes a CATEGORICAL explanatory variable against which to regress the response variable and obtains
# the estimated partial Residuals of the response variable against the explanatory variable
#plots a dotplot of the partial residuals against the potential explanatory variable
partialResidualPlot = function(df,subdf,variable){
  #get residuals of current model + candidate variable
  
  subdf = cbind(subdf,df[,variable])
  names(subdf)[ncol(subdf)]=variable
  #employees2 = employees[c("logIncome","TotalWorkingYears","JobLevel","JobRole","Department","DistanceFr#omHome","Attrition",variable)]
  fit=lm(logIncome~.,data=subdf)
  resids=resid(fit)
  #obtain coefficients for attrition
  
  
  
  coeffs = fit$coefficients[ which(grepl(variable,names(fit$coefficients)))]
  #AttritionYes will be added to the residuals of all Employees with attrition
  residDf=subdf %>% mutate(resids=resids,partialRes=resids) 
  #cut off everything after JobRole
  
  unlist(strsplit(names(coeffs[1]),variable))[2]
  #alter the names of the coefficients one by one
  levelNames= vapply(names(coeffs),function(x) unlist(strsplit(x,variable))[2],character(1))
  names(coeffs)=levelNames
  coeffLookup=data.frame(names(coeffs),coeffs)
  residDf=residDf %>% 
    merge(coeffLookup,by.x=variable,by.y="names.coeffs.",all.x = TRUE)
  #the NAs turn to 0s because that is reference level
  residDf[is.na(residDf$coeffs),]$coeffs=0
  residDf$partialRes=residDf$partialRes+residDf$coeffs
  #dotplot(residDf,variable,"partialRes")
  
  ggplot(residDf,aes_string(variable,"partialRes"))+geom_dotplot(binaxis = "y",
                                                                 stackdir="center",method="histodot",binwidth = 0.03)+
    stat_summary(fun.data = function(x) {data.frame(y=mean(x), ymin=mean(x)-2*(sd(x)/sqrt(length(x))), ymax=mean(x)+2*(sd(x)/sqrt(length(x))))},
                 geom="errorbar",color="red")
}
#returns "dummyDf": dataframe of all dummy variables for the categorical variables of input dataframe
#returns colNumLookup: a dataaframe that maps an explanatory variable's name and its corresponding
# column in the dummyDf dataframe
dummyVarLookup =  function(df){
  factorNames=df %>% keep(is.factor) %>% discard(function(x) length(unique(x))==1) %>% names
  factorVars =  df[,factorNames]
  allDummy = dummyVars(~.,data=factorVars)
  dummyDf = predict(allDummy,df)
  #create dataframe of column index lookup for each categorical variable
  #how many levels does attrition have
  numLevels = vapply(factorNames,function(x) length(unique(df[,x])), numeric(1))
  colNumLookup=data.frame(names(numLevels),numLevels)
  colNumLookup$colNum=lag(cumsum(numLevels))
  colNumLookup$colNum[1]=0
  colNumLookup$colNum=colNumLookup$colNum+1
  list(colNumLookup=colNumLookup,dummyDf=dummyDf)
}
#finds the selected dummy variable columns from dummyDf for a potential explanatory variable
dummyVarSelect=function(variable,dummyDf,colNumLookup){
  varIndex = which(colNumLookup$names.numLevels.==variable)
  startCol=colNumLookup$colNum[varIndex]
  #if we are at the last variable
  if(varIndex==nrow(colNumLookup))
  {endCol=ncol(dummyDf)+1
  
  }else{
    endCol=colNumLookup$colNum[which(colNumLookup$names.numLevels.==variable)+1]
    
  }
  levelNames = dimnames(dummyDf[,startCol:(endCol-1)])[[2]]
  dummyMatrix=dummyDf[,startCol:(endCol-1)]
  dummyMatrix = as.matrix(dummyMatrix)
  levelNames=levelNames[-1]
  dummyMatrix=dummyMatrix[,-1]
  dummyMatrix=as.matrix(dummyMatrix)
  dimnames(dummyMatrix)[[2]]=levelNames
  return(dummyMatrix)
}
#factorDum = dummyVarSelect("Attrition",outputList$dummyDf,outputList$colNumLookup)


#takes the full dataframe of data: df, partial dataframe containing only selected variables for predictive model:subdf
#response variable name
# returns a matrix of the numerical variables cbinded to categorical variables' dummy variables
lmfitMatrix=function(df,subdf,response){
  factorNames=df %>% keep(is.factor) %>% discard(function(x) length(unique(x))==1) %>% names
  numberNames=df  %>% keep(is.numeric) %>% names
  
  subdfMtx = as.matrix(subdf[,names(subdf) %in% numberNames])
  
  subdfFacVars = names(subdf)[names(subdf) %in% factorNames]
  out=dummyVarLookup(df)
  dummyDf=out$dummyDf
  colNumLookup=out$colNumLookup
  dummyMatrices = lapply(subdfFacVars,function(x) dummyVarSelect(x,dummyDf,colNumLookup))
  subdfMtx=cbind(subdfMtx,dummyMatrices[[1]])
  for (i in 1:length(dummyMatrices)){
    subdfMtx=cbind(subdfMtx,dummyMatrices[[i]])
    
  }
  
  subdfMtx=subdfMtx[,!(dimnames(subdfMtx)[[2]] %in% c(response))]
  intercept=rep(1,nrow(subdfMtx))
  subdfMtx= cbind(subdfMtx,intercept)
}
#takes a dataframe of data, subdf : partial dataframe of only model's variables, 
# returns a plot of the residuals of response when predicted by model's variables VERSUS
# residuals of selected variable when predicted by model's variables
partialRegression=function(df,subdf,variable,response,confounder="Gender"){
  
  
  subdfMtx=lmfitMatrix(df,subdf,response)
  #remove the explanatory and save it for later use
  #if it is a numerical variable then just find the column with the exact match
  
  subdfMtx=subdfMtx[,!(dimnames(subdfMtx)[[2]] %in% c(variable))]
  
  
  
  
  fitr = lm.fit(y=as.vector(df[,response]),x=subdfMtx)
  responsePredict=fitr$residuals
  #use the current variables to predict age for each employee
  
  
  fit = lm.fit(y=as.vector(df[,variable]),x=subdfMtx)
  covariatePredict=fit$residuals
  
  residDf=data.frame(covarRes=covariatePredict,respRes=responsePredict)
  residDf[,confounder]=df[,confounder]
  #plot(covariatePredict,responsePredict,xlab = variable,type = "p")
  ggplot(residDf,aes_string("covariatePredict","responsePredict",color=confounder))+geom_point()+
    geom_smooth(method="lm")+xlab(variable)
  
}