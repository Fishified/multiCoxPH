#' Run coxPH in series
#'
#' Allows the study of AIC from any number of models. Can be faster than multiCoxParallel for smaller numbers of models.
#' @param rawTerms - pass in output from either reduceTerms or reduceTermsPar
#' @param data - data set in the form of dataframe
#' @param event - event variable indicating censorship
#' @param variable - to perform regression on (e.g. Dmax)
#' @keywords multCoxParallel
#' @export
#' @examples
#' cat_function()

multiCox<-function(rawTerms,data,event,variable)
{
  C=length(rawTerms)
  aicOutput=c()
  termsList=c()
  coxModels=c()
  print(event)
  measurevar=paste("Surv(",variable,",","event=",event,"",")",sep="")
  foreach(i = 1:C) %do%
  {library(survival)
    
    for (b in 1:ncol(rawTerms[[i]])){
      
      tmpList=rawTerms[[i]][,b]
      terms=paste(tmpList, collapse=" + ")
      t=as.formula(paste(measurevar,terms,sep=" ~ "))
      modeloutput=coxph(t, data = data)
      aic=AIC(modeloutput)
      aicOutput=c(aicOutput, aic)
      coxModels=c(coxModels,modeloutput)
      termsList=c(termsList,terms)
    }
  }
  return(data.frame("AIC"=aicOutput, "Terms"=termsList))
}