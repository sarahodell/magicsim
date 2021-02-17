#' Simulate a crossover between homologous chromosomes of a parent
#' @param xo the number of crossovers
#' @param donor1 Chrom object in format created from chrom_init, crossover starts with this one
#' @param donor2 Chrom object in format created from chrom_init
#' @param recomb dataframe of genetic and physical map for chrom c
#' @param c The chromosome being simulated
#'
#' @return a list object of one recombinant chromosome
#' @export

crossover<-function(xo,donor1,donor2,recomb,c){
  interp=approxfun(recomb$scaled_cM,recomb$pos,yleft=min(recomb$pos),yright=max(recomb$pos),ties="ordered")
  maxp=max(recomb$pos)
  print(maxp)
  draw=runif(xo)
  xo_pos=round(interp(draw*max(recomb$scaled_cM)),0)
  xo_pos=sort(xo_pos)
  #if(xo>=2){
    #dist=max(recomb$scaled_cM)/100
  #  xo_pos=interference(xo_pos)
  #}
  for(p in sort(xo_pos)){
    print(c)
    print(p)
    # If p is equal to a pre-existing breakpoint, shift p up 100 base pairs
    while((0 %in% c(p-donor1@breakpoints)) | (0 %in% c(p-donor2@breakpoints))){
      if(p+1000 < maxp){
        p = p+1000
      }
      else{
        p = p-1000
      }
      print("Overlap")
      print(sprintf("New p %.0f",p))
    }
    # If this is an F1 cross between inbreds (breakpoints and donors are of length 1)
    # If the new break is less than any previous breakpoint in either of the two donors
    if((p < donor1@breakpoints[1]) & (p < donor2@breakpoints[1])){
      print("1")
      recomb_pos=c(p,donor2@breakpoints)
      recomb_donor=c(donor1@donors[1],donor2@donors)
    }
    else if((p < donor1@breakpoints[1]) & (p > donor2@breakpoints[1])){
      print("2")
      #If the new break is less than previous breakpoints in donor1
      #but not in donor2
      d2_upper=donor2@breakpoints[donor2@breakpoints-p>0]
      d2_index=which(min(d2_upper)==donor2@breakpoints)
      # New breakpoints are p and all breakpoints from donor2 greater than p
      recomb_pos=c(p,donor2@breakpoints[d2_index:length(donor2@breakpoints)])
      recomb_donor=c(donor1@donors[1],donor2@donors[d2_index:length(donor2@donors)])
    }
    else if((p > donor1@breakpoints[1]) & (p < donor2@breakpoints[1])){
      print("3")
      #If the new break is less than previous breakpoints in donor2
      #but not in donor1
      d1_lower=donor1@breakpoints[p-donor1@breakpoints>0]
      d2_upper=donor2@breakpoints[donor2@breakpoints-p>0]
      d2_index=which(min(d2_upper)==donor2@breakpoints)
      d1_index=which(max(d1_lower)==donor1@breakpoints)
      recomb_pos=c(donor1@breakpoints[1:d1_index],p,donor2@breakpoints[d2_index:length(donor2@breakpoints)])
      d1_index=d1_index+1
      recomb_donor=c(donor1@donors[1:d1_index],donor2@donors[d2_index:length(donor2@donors)])
    }
    else{
      print("4")
      # Or both have breakpoints before p
      d1_lower=donor1@breakpoints[p-donor1@breakpoints>0]
      d2_upper=donor2@breakpoints[donor2@breakpoints-p>0]

      if(length(d2_upper)==0){
        print("No breakpoints in donor2 greater than p")
        print(p)
        print("donor1")
        print(donor1)
        print("donor2")
        print(donor2)
        browser()
      }
      else if(length(d1_lower)==0){
        print("No breakpoints in donor1 less than p")
        print(p)
        print("donor1")
        print(donor1)
        print("donor2")
        print(donor2)
        browser()
      }
      else{
        d2_index=which(min(d2_upper)==donor2@breakpoints)
        d1_index=which(max(d1_lower)==donor1@breakpoints)
        recomb_pos=c(donor1@breakpoints[1:d1_index],p,donor2@breakpoints[d2_index:length(donor2@breakpoints)])
        d1_index=d1_index+1
        recomb_donor=c(donor1@donors[1:d1_index],donor2@donors[d2_index:length(donor2@donors)])
      }
    }
    donor2=donor1
    donor1=Chrom(chr=c,breakpoints=recomb_pos,donors=recomb_donor,xo_no=length(recomb_pos)-1)

  }
  return(Chrom(chr=c,breakpoints=recomb_pos,donors=recomb_donor,xo_no=length(recomb_pos)-1))
}
