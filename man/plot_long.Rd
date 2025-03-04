% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_long.R
\name{plot_long}
\alias{plot_long}
\title{Plot to show longitudinal data}
\usage{
plot_long(
  data,
  x,
  y,
  by,
  title,
  xlab,
  ylab,
  xlow,
  xhigh,
  ylow,
  yhigh,
  color1,
  color2 = color1,
  percentile = 0,
  show_max = FALSE,
  span = 0.75
)
}
\arguments{
\item{data}{- data set from which we plot}

\item{x}{- what is on the x-axis}

\item{y}{- what is on the y-axis}

\item{by}{- data groupings we choose (different coloured, different
calculations) - example - plotting bodyweight over mouse age, grouping by
strain to see how different strains tend to gain or lose weight over time}

\item{title}{- this is the title of your plot. This parameter is optional}

\item{xlab}{- this is the x-axis label. This parameter is optional}

\item{ylab}{- this is the y-axis label. This parameter is optional}

\item{xlow}{- lower bound on the x-axis. This parameter is optional, and if
you skip it, it will recalculate based on data}

\item{xhigh}{- upper bound on the x-axis. This parameter is optional, and if
you skip it, it will recalculate based on data}

\item{ylow}{- lower bound on the y-axis. This parameter is optional, and if
you skip it, it will recalculate based on data}

\item{yhigh}{- upper bound on the y-axis. This parameter is optional, and if
you skip it, it will recalculate based on data}

\item{color1}{- palette you want to use. This is optional - if you don't set
it, r will use default pallette}

\item{color2}{- if you want a different color for the lines than the points,
indicate it here, otherwise it is the same as color1 by default}

\item{percentile}{- this refers to which points you don't want to plot. For
example, if it is set to 0.05, meaning that 5% of  highest and 5% of lowest
points don't get plotted. Default is 0, so no points get removed}

\item{show_max}{- this asks if you want to put a line through local maximum
of y. Default is FALSE}

\item{span}{- this refers to span for LOESS lines. Default is set to r
defalut, which is 0.75}
}
\value{
Plot
}
\description{
Function takes in data, and plots points and lines longitudinally, using
loess (you can change span, but it is set to default) You can also choose
to show the local maximum for analytical purposes
}
\author{
....
}
