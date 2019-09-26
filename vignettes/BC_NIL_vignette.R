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


## ------------------------------------------------------------------------
#Fix this so f2 is a population, not individuals
bc_pop=backcross_pop(B73,f2s,c=1,n=100,ngen=3,g_map=gmap)

## ------------------------------------------------------------------------
bc_nils=self_pop(bc_pop,c=1,ngen=6,g_map=gmap)

## ------------------------------------------------------------------------
breaktable=make_pop_breaktable(bc_nils,n=10,c=1,het=F)
plot_Pop(breaktable,n=10,c=1,het=F)

## ------------------------------------------------------------------------
B73=indv_init(chr=10,gmap,h1_donors=c("B73"),h2_donors=c("B73"))
PT=indv_init(chr=10,gmap,h1_donors=c("PT"),h2_donors=c("PT"))
f1=make_f1(B73,PT,gmap,chroms=10)
f2=offspring(f1,f1,g_map=gmap,chroms=10)

indv_breaktable=make_indv_breaktable(f2,het=F)
plot_Indv(indv_breaktable,het=F)

