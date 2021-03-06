% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interference.R
\name{interference}
\alias{interference}
\title{If the two crossover events are too close to each other,
only keep one of the crossover locations}
\usage{
interference(xo_pos, dist = 10000000)
}
\arguments{
\item{xo_pos}{crossover position (int)}

\item{dist}{physical distance below which interference occurs (default is 10 Mb)}
}
\description{
If the two crossover events are too close to each other,
only keep one of the crossover locations
}
