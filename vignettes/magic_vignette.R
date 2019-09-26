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

# The 16 founder maize inbred lines. This could be anything
founders=c("A632","B73","CO255_inra","FV252",
           "OH43","A654","FV2","C103",
           "EP1","D105","W117","B96","DK63",
           "F492","ND245","VA85")

cross_order=c(2,4,14,15,3,8,10,16,1,9,5,6,12,7,11,13)
cross_info=founders[cross_order]


### Simulate chromosome 1
c=1
### Initialize the 16 founders
founderpop=pop_init(n=16,chroms=c,donors=cross_info,g_map=gmap,poptype="magic")

## ------------------------------------------------------------------------
### Create F1s from the first round of crossing
#Create an empty Pop object
f1s=new("Pop",nIndv=8,indvlist=vector("list",length=8))

#Make F1s for each of the first crosses and add to the Pop object
count=1
for(i in seq(1,16,2)){
  f1s@indvlist[[count]]=make_f1(founderpop[i],founderpop[i+1],gmap,chroms=c)
  count=count+1
}


