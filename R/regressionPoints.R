#' Outputs text file with regression points 
#'
#' Allows one to produced multiple modified survival curves based on input provided in an excel spreadsheet
#' @param coxObject - pass in the coxPH (e.g. A <- coxph(), pass in "A")
#' @param excelFile - excelFile following format in example
#' @param printName - name of output text file
#' @keywords regressionPoints
#' @export
#' @examples
#' regressionPoints()

regressionPoints <-function(coxObject,excelFile,printName)
{
  #import excel input file
  covariate_data<-read.xlsx(excelFile, 1)
  
  #isolate base-line survival curve
  survObject<-survfit(coxObject)
  km1<-as.data.frame(cbind(time=survObject$time,n.risk=survObject$n.risk,n.event=survObject$n.event,surv=survObject$surv,cumhaz=survObject$cumhaz))
  constant<-sum(coxObject$means*coxObject$coefficients) #this is the constant (sum of Bi*Xi) in exp() which raises the power of the baseline survival curve
  km1$bslnsurv<-(km1$surv)^(1/exp(constant))            #isolates the baseline survival curve
  newrow=c(0,0,0,1,0,1)                                 # some necessary values to fill in on the top row
  km1 = rbind(newrow,km1)                               #bind the necessary values to the top row
  
  #extract model coefficients and means
  coefMatrix<-do.call(rbind, Map(data.frame, means=coxObject$means, coefs=coxObject$coefficients))#construct dataframe holding means and coefficients
  coefMatrix$covariate<-rownames(coefMatrix)
  covariateInfo=merge(x = covariate_data, y = coefMatrix, by = "covariate", all = FALSE)
  names=names(covariateInfo)
  rmnames=c("covariate","datatype","covariateValues","means","coefs")
  sets=setdiff(names, rmnames)
  
  #determine coefficients for each set of covariates
  newconstants=c()
  i=1
  for (n in sets) {
    covariateInfo[[n]]=ifelse(covariateInfo$datatype==1,covariateInfo[[n]]*covariateInfo$coefs*covariateInfo$covariateValues,
                              ifelse(covariateInfo$datatype==0,covariateInfo[[n]]*covariateInfo$coefs,covariateInfo[[n]]))
    newconstants[i]<-sum(covariateInfo[[n]])
    i=i+1
  }
  
  #adjust and create points for regression
  p=1
  for (i in newconstants){
    header=sets[p]
    km1$adjKM<-(km1$bslnsurv)^(exp(i))
    km1[header]<-1-((Lag(km1$adjKM,shift=1)-km1$adjKM)/2+km1$adjKM)       #find the average between a step in KM plot
    km1[header]<-ifelse(is.na(km1[[header]]),0,km1[[header]])
    p=p+1
  }
  
  #export data as .txt
  write.table(km1, paste(getwd(),"/",printName, ".txt",sep=''), sep=",", row.names=FALSE)
  
}
