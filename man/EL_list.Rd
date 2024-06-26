% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EL_list.R
\name{EL_list}
\alias{EL_list}
\title{Make a list containing the results of \code{eval_logLik()}}
\usage{
EL_list(fits, ull, se, cll, cll_se, np_pf, nreps)
}
\arguments{
\item{fits}{Data frame of fit results. Columns should be named \code{logLik},
\code{se}, followed by the parameter names.}

\item{ull}{Data frame of unit log likelihoods. Column names are unit names.}

\item{se}{Data frame of unit standard errors. Column names are unit names.}

\item{cll}{List of matrices containing estimated conditional log likelihoods
for each unit.}

\item{cll_se}{List of matrices containing standard errors for estimated
conditional log likelihoods for each unit.}

\item{np_pf}{Number of particles used by \code{eval_logLik()}.}

\item{nreps}{Number of replications used by \code{eval_logLik()}.}
}
\value{
The arguments in list form with class \code{EL_list}.
}
\description{
Make a list containing the results of \code{eval_logLik()}
}
