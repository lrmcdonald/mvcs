# Load Packages ----
libs = c("tidyverse")
lapply(libs, require, character.only = TRUE)
rm(libs)

# Simulation Functions
writer = function(string){
  # Write with the possibility of additive mutation or no change (equal probability)
  mutations = c("none", "a", "g", "c", "t")
  add = sample(mutations, size = 1)
  where = sample(0:str_length(string), size = 1)
  if(add == "none"){
    output = string
  } else {
    output = str_c(str_sub(string, start = 1, end = where), add, str_sub(string, start = where, end = str_length(string)))
  }
  return(output)
}

reader = function(codon){
  first = str_sub(codon, start = 1, end = 1)
  second = str_sub(codon, start = 2, end = 2)
  third = str_sub(codon, start = 3, end = 3)
  
  
}

x = "augttacggcuga"

