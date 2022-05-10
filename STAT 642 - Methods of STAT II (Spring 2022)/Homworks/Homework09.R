#### STAT 642 Asignment 09

############# Problem 2 (just using this to check if what I did for 5 was correct/generalizable)

design = FrF2(nruns=16,nfactors=7,generators=c("ABC","BCD","ABD"))
design$y = c(1:nrow(design))
alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets

treatments = NULL
for(i in 1:length(alias_sets[["aliases"]])){
  treatments[i] = unlist(alias_sets[["aliases"]][i])[1]
  treatments = gsub(":","",treatments)
}

##### so b/c the way you need to specify the generators in FrF2 (the letters can only
# go up to D in this case (they can go up to E in problem 5 for some reason)) I get a slightly
# different set of treatmennts and a very different set of implict generators when I try and make
# it work with R... Im just gonna do the defining of the treatments manually, and manually specify
# the design generators below and see if what I get matches what I did by hand.



DesignGenerators = c("ABCF","CDEF","ADFG")
ImplicitGenerators = NULL
iter = 1
for(i in 1:(length(DesignGenerators)-1)){
  for(j in (i+1):(length(DesignGenerators))){
    ImplicitGenerators[iter] = alias_construction(DesignGenerators[i],DesignGenerators[j])
    iter = iter+1
    
  }
}
# NOTE: this only takes care of 2 way interactions... for this problem, since p=3 and n = 7
# I need a 3 way interaction (b/c I should have 4 implicit generators and this only gives 3)

Generators = c(DesignGenerators,ImplicitGenerators,"BEFG")
treatments = c(treatments[-c(14,15)],"BG","ABG")

# Now I'll apply the rest of the stuff to check if it works

alias.sets = matrix("NA", nrow = length(treatments)+1,ncol = length(Generators)+1)

for(i in 1:(length(treatments))){
  if(i == 1){
    alias.sets[,i] = c("I", treatments)
    alias.sets[i,-1] = Generators
  }
  for(j in 1:length(Generators)){
    alias.sets[i+1,j+1] = alias_construction(treatments[i],Generators[j]) 
  }
}
# works like a fucking charm

######################################### Problem 5 ##########################################

#install.packages("FrF2")
library(FrF2)
design = FrF2(nruns=32,nfactors=8,generators=c("ABC","ABD","BCDE"))
design$y = c(1:nrow(design))
alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets


# making my own loop to make alias sets, st. the aliases are the ones with our defining contrasts
# including the implicit ones 
treatments = NULL
for(i in 1:length(alias_sets[["aliases"]])){
  treatments[i] = unlist(alias_sets[["aliases"]][i])[1]
  treatments = gsub(":","",treatments)
}


alias_construction = function(alias1,alias2){
  aliasC = paste0(alias1,alias2)
  aliasC = paste(sort(unlist(strsplit(aliasC, ""))), collapse = "")
  aliasC = gsub("([[:alpha:]])\\1{1,}", "", aliasC)
  return(aliasC)
  
}

DesignGenerators = c("ABCF", "ABDG", "BCDEH")
ImplicitGenerators = NULL
iter = 1
for(i in 1:(length(DesignGenerators)-1)){
  for(j in (i+1):(length(DesignGenerators))){
    ImplicitGenerators[iter] = alias_construction(DesignGenerators[i],DesignGenerators[j])
    iter = iter+1
    
  }
}
Generators = c(DesignGenerators,ImplicitGenerators,"BEFGH")


alias.sets = matrix("NA", nrow = length(treatments)+1,ncol = length(Generators)+1)

for(i in 1:(length(treatments))){
  if(i == 1){
    alias.sets[,i] = c("I", treatments)
    alias.sets[i,-1] = Generators
  }
  for(j in 1:length(Generators)){
      alias.sets[i+1,j+1] = alias_construction(treatments[i],Generators[j]) 
  }
}

nrow(alias.sets)*ncol(alias.sets)
length(unique(c(alias.sets)))
# note: since these are the same, there is no unintended confounding in our design


#install.packages("gridExtra")
library(gridExtra)
grid.table(alias.sets, theme = ttheme_default(base_size = 8))

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = .5)))
  #colhead = list(fg_params=list(cex = 1.0)),
  #rowhead = list(fg_params=list(cex = 1.0)))

a = tableGrob(alias.sets,theme = mytheme)

grid.draw(a)

################################### Problem 6 ###########################################

library(FrF2)
design = FrF2(nruns=16,nfactors=5,generators=c("ABCD"))
design$y = c(1:nrow(design))
alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets

View(alias_sets)


design


# Problem 6 (6)

library(FrF2)
design = FrF2(nruns=8,nfactors=4,generators=c("AB","AC"))
design$y = c(1:nrow(design))
alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets

class


library(FrF2)
design = FrF2(nruns=8,nfactors=4,generators=c("ABC"))
design$y = c(1:nrow(design))
alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets

class




