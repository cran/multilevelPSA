% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.mlpsa.distribution.R
\name{mlpsa.distribution.plot}
\alias{mlpsa.distribution.plot}
\title{Plots distribution for either the treatment or comparison group.}
\usage{
mlpsa.distribution.plot(
  x,
  treat,
  fill.colours = NULL,
  flip = TRUE,
  label = treat,
  level2.label = NULL,
  legendlab = NULL,
  axis.text.size = 8,
  fill.colors = NULL,
  ...
)
}
\arguments{
\item{x}{the results of [mlpsa()].}

\item{treat}{the group to plot. This must be one of the two levels of the treatment variable.}

\item{fill.colours}{if specified, the colors to use for level 2 points.}

\item{flip}{if TRUE, the level 2 clusters will be on the y-axis and the outcome
variable on the x-axis. Otherwise reversed.}

\item{label}{the label to use for the axis.}

\item{level2.label}{the axis label for the level 2 indicators.}

\item{legendlab}{the label for the legend, or NULL to exclude a legend.}

\item{axis.text.size}{the size of the axis text}

\item{fill.colors}{if specified, the colors to use for level 2 points.}

\item{...}{currently unused.}
}
\description{
Plots distribution for either the treatment or comparison group.
}
\seealso{
plot.mlpsa
}
