% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_cox.R
\name{surv_cox}
\alias{surv_cox}
\title{Cox Function that can either be time dependent or time independent}
\usage{
surv_cox(
  data,
  covariates,
  time,
  time2 = NULL,
  death,
  id = NULL,
  tt = NULL,
  type = c("right", "left", "interval", "counting", "interval2", "mstate")
)
}
\arguments{
\item{data}{dataframe that includes the covariates we would like to model.
If your dataset is longitudinal, the recommendation is to use
SLAM::surv_tmerge to create an interval dataframe.}

\item{covariates}{on sided formula starting with "~" that provides the
formula for the cox model. If the coefficients for your covariates are time
dependent, that covariate should be wrapped with the tt() function, for
example tt(age_wk). Then in tt argument of this function define the function
that will time transform your variable.}

\item{time}{A character string. In cross sectional data, it should be the
name of the variable for age of observation in data. Inlongitudinal datasets
created by SLAM::surv_tmerge, this will be "tstart"}

\item{time2}{A optional character string only used when working with
longitudinal datasets. This string provides the variable that defines theend
point of interval in longitudinal datasets. If the dataset is created by
SLAM::surv_tmerge, it would be "tstop".}

\item{death}{A character string. in cross-sectional datasets, this is the
variable that determines death censorship, where a natural death is 1 and a
censored subject is 0. In longitudinal datasets, this variable specifies
whether or not a subject died at the end of the interval. If your
longitudinal dataset was created by SLAM::surv_tmerge, this would be "tstop".}

\item{id}{optional variable that identifies subjects. Only necessary
when a subject can have multiple rows in the data, and there is
more than one event type.  This variable will normally be found
in ‘data’. However, it appears that the survival packge has not yet
gotten this subject based cox model to work}

\item{tt}{a function that defines the time transformation that will be
applied when a covariate is wrapped with the tt(). By default tt = NULL.}

\item{type}{A character string that specifies the type of censoring.}
}
\value{
returns coxph object
}
\description{
Function that calls Survival Object and Coxph in one function call.
This function can be used if your dataset is cross sectional (one
observation per individual) and longitudianl (multiple observations per
individual). It can also be used if your hazard ratios are time
independent (hazard is constant with time) or if you hazard ratios are time
depedent (hazard changes with time).
}
\seealso{
\link[survival]{Surv} \link[survival]{coxph}
}
\author{
William Mueller
}
