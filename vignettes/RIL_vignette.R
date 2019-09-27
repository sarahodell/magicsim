## ----results='asis', echo=FALSE, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
### Script to simulate 400 Double Haploid MAGIC lines

#devtools::install_github('sarahodell/magicsim',force = T)

library('magicsim')
library("data.table")
library("tidyverse")
library("ggplot2")

options(scipen=999)

## ------------------------------------------------------------------------
set.seed(11)
gmap=fread('ogutmap_v4_ordered.csv',data.table=F)

# The 2 initial founder maize lines. This could be anything
founders=c("Mo17","B73")

### Simulate chromosomes 1 throught 10
c=10
### Initialize the founders
B73=indv_init(chr=c,gmap,h1_donors=c("B73"),h2_donors=c("B73"))
Mo17=indv_init(chr=c,gmap,h1_donors=c("Mo17"),h2_donors=c("Mo17"))


## ------------------------------------------------------------------------
#Make F1s for each of the first crosses
#f1=make_f1(B73,Mo17,gmap,chroms=c)

n=100

### Create F1s from the first round of crossing
f1s=new("Pop",nIndv=n,indvlist=vector("list",length=n))

#Make F1s for each of the first crosses and add to the Pop object
for(i in seq(1,n)){
  f1s@indvlist[[i]]=make_f1(B73,Mo17,gmap,chroms=c)
}

