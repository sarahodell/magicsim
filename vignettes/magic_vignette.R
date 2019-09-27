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
set.seed(11)

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


## ---- echo=F-------------------------------------------------------------
magic_pop=make_magic_pop(start_pop=f1s,c=1,g_map=gmap,popsize=1000)

## ------------------------------------------------------------------------
#what is the distribution of xo_no in the population?
total_xo=c()
for(i in seq(1,1000)){total_xo=c(total_xo,magic_pop[i][1]@h1@xo_no)}

crossovers=data.frame(ind=seq(1,1000),xo_no=total_xo)
ggplot(crossovers,aes(x=xo_no)) + geom_histogram(bins=9,fill="darkgreen",color="black") + theme_classic() +
  ggtitle("Distribution of Crossovers in Simulation MAGIC Population") + xlab("Crossover Number") + 
  ylab("Frequency") + geom_vline(xintercept=mean(crossovers$xo_no))


## ------------------------------------------------------------------------
breaktable=make_pop_breaktable(magic_pop,n=10,c=1,het=T)
head(breaktable)

## ------------------------------------------------------------------------
plot_Pop(breaktable,n=10,c=1,het=T)
# Plotting can be done as heterozygous or homozygous

## ---- echo=F-------------------------------------------------------------
synth_pop=outcross(magic_pop,3,1,gmap)

## ------------------------------------------------------------------------
#what is the distribution of xo_no in the population?
total_xo=c()
for(i in seq(1,1000)){total_xo=c(total_xo,synth_pop[i][1]@h1@xo_no)}

crossovers=data.frame(ind=seq(1,1000),xo_no=total_xo)
ggplot(crossovers,aes(x=xo_no)) + geom_histogram(bins=9,fill="darkgreen",color="black") + theme_classic() +
  ggtitle("Distribution of Crossovers in Simulation Synth3 Population") + xlab("Crossover Number") + 
  ylab("Frequency") + geom_vline(xintercept=mean(crossovers$xo_no))


## ------------------------------------------------------------------------
#randomly choose 400 out of the 1000
dh_pop=make_dh_pop(synth_pop,n=400,c=1,gmap)

## ------------------------------------------------------------------------
no=sapply(seq(1,400),function(x) length(dh_pop[x][1]@h1@donors))
#what is the distribution of xo_no in the population?
total_xo=c()
for(i in seq(1,400)){total_xo=c(total_xo,dh_pop[i][1]@h1@xo_no)}

crossovers=data.frame(ind=seq(1,400),xo_no=total_xo)
ggplot(crossovers,aes(x=xo_no)) + geom_histogram(bins=9,fill="darkgreen",color="black") + theme_classic() +
  ggtitle("Distribution of Crossovers in Simulation DH Population") + xlab("Crossover Number") + 
  ylab("Frequency") + geom_vline(xintercept=mean(crossovers$xo_no))


## ------------------------------------------------------------------------
#This should be its own function
breaktable=make_pop_breaktable(dh_pop,n=10,c=1,het=F)
head(breaktable)

## ------------------------------------------------------------------------
plot_Pop(breaktable = breaktable,n=10,c=1,het=F)

