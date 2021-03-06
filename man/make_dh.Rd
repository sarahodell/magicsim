% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_dh.R
\name{make_dh}
\alias{make_dh}
\title{make_dh ------------------------------------------------------------------------
Creates an Indv object of doubled haploid (DH) lines randomly selected from input Pop. For subset of individuals,
randomly selects one of the two chromosome pairs to double}
\usage{
make_dh(indv, c = 10, g_map)
}
\arguments{
\item{indv}{An Indv object}

\item{c}{Chromosome number (default is 10)}

\item{g_map}{The genetic map}
}
\value{
An Indv object that is completely homozygous
}
\description{
make_dh ------------------------------------------------------------------------
Creates an Indv object of doubled haploid (DH) lines randomly selected from input Pop. For subset of individuals,
randomly selects one of the two chromosome pairs to double
}
