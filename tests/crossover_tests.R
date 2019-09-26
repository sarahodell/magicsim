### Testing of crossover function\
devtools::install_github('sarahodell/magicsim',force = T)
library('data.table')
library('magicsim')
library("tidyverse")
library("ggplot2")


gmap=fread('vignettes/ogutmap_v4_ordered.csv',data.table=F)

A=indv_init(chr=10,gmap,h1_donors=c("A"),h2_donors=c("A"))
B=indv_init(chr=10,gmap,h1_donors=c("B"),h2_donors=c("B"))

f1=make_f1(A,B,gmap,chroms=1)

chrom_B=f1@chromlist[[1]]@h1
chrom_A=f1@chromlist[[1]]@h2


recomb=gmap[gmap$chr==1,]
for(i in seq(1,1000)){
  f2_1=magicsim::crossover(2,chrom_B,chrom_A,recomb,1)
}

t1=magicsim::indv_init(chr=1,gmap,h1_donors=c("A","B"),h1_breakpoints=c(121525174,306879533),h2_donors=c("C","D"),h2_breakpoints = c(291774183,306879533))
h1=t1@chromlist[[1]]@h1
h2=t1@chromlist[[1]]@h2

for(i in seq(1,1000)){
  f2_1=magicsim::crossover(2,h1,h2,recomb,1)
}

