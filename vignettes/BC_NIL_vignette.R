## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='asis', echo=FALSE, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
### Script to simulate 400 Double Haploid MAGIC lines

devtools::install_github('sarahodell/magicsim',force = T)

library('magicsim')
library("data.table")
library("tidyverse")
library("ggplot2")

options(scipen=999)

## ------------------------------------------------------------------------

gmap=fread('ogutmap_v4_ordered.csv',data.table=F)

# The 2 initial founder maize lines. This could be anything
founders=c("PT","B73")

### Simulate chromosome 1
c=1
### Initialize the founders
B73=indv_init(chr=1,gmap,h1_donors=c("B73"),h2_donors=c("B73"))
PT=indv_init(chr=1,gmap,h1_donors=c("PT"),h2_donors=c("PT"))


## ------------------------------------------------------------------------
#Make F1s for each of the first crosses
f1=make_f1(B73,PT,gmap,chroms=1)
n=100

f2s=new("Pop",nIndv=n,indvlist=vector("list",length=n))
for(i in seq(1,n)){
  f2s@indvlist[[i]]=offspring(f1,f1,gmap,chroms=1)
}


