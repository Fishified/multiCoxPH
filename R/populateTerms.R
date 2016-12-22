#' Derive all possible combinations of base and interaction terms
#'
#' Allows you to study AIC output from any number of models.
#' @param termsList - list of terms, include interactions with "*" or ":" as you see fit
#' @param frailty - "True" includes frailty()
#' @param frailtyTerm - indicates frailty term (e.g. "individual")
#' @keywords populateTerms()
#' @export
#' @examples
#' populateTerms()


populateTerms<-function(termsList,frailty,frailtyTerm)
{ 
  terms=c(termList)
  listLength=length(terms)
  a=c()
  foreach(i = 1:listLength) %do%
    if (frailty=="True"){
      a=rbind(combn(terms,i, simplify = TRUE),paste("frailty(",frailtyTerm,")",sep=""))
      return(a)}
  else if (frailty=="False"){
    a=combn(terms,i, simplify = TRUE)
    return(a)}
}