% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_tmerge.R
\name{surv_tmerge}
\alias{surv_tmerge}
\title{Tmerge Long Data}
\usage{
surv_tmerge(data, id, age, age_death, dead_censor, outcomes)
}
\arguments{
\item{data}{dataframe in long form for outcome measurements}

\item{id}{character string specifying subject in data}

\item{age}{character string specifying age in data}

\item{age_death}{character string specifiying age of death in data}

\item{dead_censor}{character string specifying censor in data.
A death is represented by 1 and censorship is represented by 0}

\item{outcomes}{vector or list of strings specifying the outcome variables
at each timepoint}
}
\value{
outputs dataframe with tstart, tstop, death that can be used for
time dependent survival analysis
}
\description{
This will take data in long form and create a tmerge dataframe
that can be used in time dependent survival analysis
}
\seealso{
\link[survival]{tmerge}
}
\author{
William Mueller
}
