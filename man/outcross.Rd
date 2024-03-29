% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outcross.R
\name{outcross}
\alias{outcross}
\title{Function to outcross population for ngen generations}
\usage{
outcross(pop, ngen, chroms, g_map, endsize = NULL)
}
\arguments{
\item{pop}{Pop object representing starting population}

\item{ngen}{The number of generations of outcrossing}

\item{chroms}{The number of chromosomes}

\item{g_map}{The genetic map}

\item{endsize}{The size of the resulting population, if NULL, pop will be same size as starting pop}
}
\value{
A synthetic population that
has been randomly outcrossed for ngen generations (list of lists)
}
\description{
Function to outcross population for ngen generations
}
