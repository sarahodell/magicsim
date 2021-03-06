% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gametes.R
\name{get_gametes}
\alias{get_gametes}
\title{Generates a gamete randomly from one of the two chromosomes, with 50 percent chance of crossing over}
\usage{
get_gametes(parent, recomb, c)
}
\arguments{
\item{parent}{ChromPair object in format created from chrom_init}

\item{recomb}{dataframe of genetic and physical map for chrom c}

\item{c}{Chromosome number}
}
\value{
One chromosome object
}
\description{
Generates a gamete randomly from one of the two chromosomes, with 50 percent chance of crossing over
}
