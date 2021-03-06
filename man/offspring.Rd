% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/offspring.R
\name{offspring}
\alias{offspring}
\title{Generate offspring from a cross between two parents}
\usage{
offspring(p1, p2, chroms, g_map)
}
\arguments{
\item{p1}{The first parents to cross. An Indv object created by indv_init()}

\item{p2}{The second parent to cross. An Indv object created by indv_init()}

\item{chroms}{The number of chromosomes}

\item{g_map}{The genetic map}
}
\value{
A chromosome object that is the combination of two the two parents with potential crossing over
}
\description{
Generate offspring from a cross between two parents
}
