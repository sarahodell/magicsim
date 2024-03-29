% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chrom_init.R
\name{chrom_init}
\alias{chrom_init}
\title{Intialize a chromosome object (diploid)}
\usage{
chrom_init(
  c = 10,
  map,
  h1_breakpoints = NULL,
  h1_donors = c("A"),
  h2_breakpoints = NULL,
  h2_donors = c("A")
)
}
\arguments{
\item{c}{Chromosome to initialize (int); default is 10}

\item{map}{Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position}

\item{h1_breakpoints}{physical breakpoint locations where donor identity changes on copy one of chromosome; default is NULL (list)}

\item{h1_donors}{donor identities for copy one of chromosome; default is A (list)}

\item{h2_breakpoints}{physical breakpoint locations where donor identity changes on copy two of chromosome; default is NULL (list)}

\item{h2_donors}{donor identities for copy two of chromosome; default is A (list)}
}
\value{
A list of lists with attributes chr, h1, and h2, with h1 and h2
each containing vectors of breakpoint locations and donor names
}
\description{
Intialize a chromosome object (diploid)
}
