% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/populateTerms.R
\name{populateTerms}
\alias{populateTerms}
\title{Derive all possible combinations of base and interaction terms}
\usage{
populateTerms(termsList, frailty, frailtyTerm)
}
\arguments{
\item{termsList}{- list of terms, include interactions with "*" or ":" as you see fit}

\item{frailty}{- "True" includes frailty()}

\item{frailtyTerm}{- indicates frailty term (e.g. "individual")}
}
\description{
Allows you produce a list of all possible combinations of terms provided
}
\examples{
populateTerms()
}
\keyword{populateTerms()}
