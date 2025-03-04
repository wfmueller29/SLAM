% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_km.R
\name{surv_plot_km}
\alias{surv_plot_km}
\title{This function plots KM curves for a categorical variable of interest}
\usage{
surv_plot_km(data, var, age_death, event, ...)
}
\arguments{
\item{data}{the dataframe that will be plotted}

\item{var}{a string specifying the variable name to stratify the plots}

\item{age_death}{a string specifying the age of death variable}

\item{event}{a string specifying the status of the event 0=alive, 1=dead}

\item{...}{other named arguments to pass to ggsurvplot function}
}
\value{
a km ggplot
}
\description{
Calls survminer::ggsurvplot to create KM curves
}
\author{
William Mueller, Eric Shiroma
}
