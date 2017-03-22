#' Run coxPH in series
#'
#' Allows the study of AIC from any number of models. Can be faster than multiCoxParallel for smaller numbers of models.
#' @param rawTerms - pass in output from either reduceTerms or reduceTermsPar
#' @param data - data set in the form of dataframe
#' @param event - event variable indicating censorship
#' @param variable - to perform regression on (e.g. Dmax)
#' @param model - choose between "coxph" or "survreg"
#' @keywords multCoxParallel
#' @export
#' @examples
#' multiCox()

multiCox<-function(rawTerms,data,event,variable,model)
{
  lnt=length(rawTerms)
  aicOutput=c()
  termsList=c()
  coxModels=c()
  measurevar=paste("Surv(",variable,",","event=",event,"",")",sep="")
  
  #start outer loop for each list of sets terms
  foreach(i = 1:lnt) %do%
  {library(survival) #for some reason the survival library has to be loaded each time ...
    
    #inner loop for each set of terms
    for (b in 1:ncol(rawTerms[[i]])){
      
      #construct object to model
      tmpList=rawTerms[[i]][,b]
      terms=paste(tmpList, collapse=" + ")
      t=as.formula(paste(measurevar,terms,sep=" ~ "))
      
      #model choice
      if (model=="coxph"){
        modeloutput=coxph(t, data = data)
        }
      else if (model=="survreg"){
        modeloutput=survreg(t,data=data)
        }
      
      #construct model terms for output
      a=modeloutput$terms
      b=attr(a,"term.labels")
      terms=paste(b, collapse=" + ")
      termsList=c(termsList,terms)
      
      #determine AIC for output
      aic=AIC(modeloutput)
      aic=round(aic, digits = 4)
      aicOutput=c(aicOutput, aic)
      
    }
  }
  
  df=data.frame("AIC"=aicOutput, "Terms"=termsList)
  return(subset(df, !duplicated(AIC))) #removes redundant models from df
}
