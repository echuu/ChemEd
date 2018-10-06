
# preprocess.R

# preprocess the data from part 1, with a few changes: (1) some rows filtered out, 
# (2) results from common questions included 

## clean up the way the data is presented, add some variables of interest

setwd('/home/eric/Dropbox/chem_ed/data')

library(dplyr)

d = read.csv("p2_data.csv") # 1020 x 56

## GLOBAL VARS
n = nrow(d) # number of observations
school_names = c("ACUP", "TAMSA", "TAMU", "TSU", "UNTB", "UNTW", "UTXW")
MIN_PASS = 70 # lowest passing course score


#################### ---- reformat school variable ---- ########################


## remove the suffixes of each of the school so to extract 4-5 letter abbrev
id      = as.character(d[,1])
id_inst = character(n)
for(i in 1:n) {
  n_char <- nchar(id[i])
  id_inst[i] <- substr(id[i], 1, n_char - 3)
}

## Store just the 7 institution identifiers.
for (s in school_names) {
  id_inst[grep(s, id_inst)] = s
}

## note that the number of observations from each school is different
## when we split into train/test sets, take equal proportions from each school
## so that we do not completely leave out a school
table(id_inst) # 7 schools identified by 4-5 letter codes -- 1020 total


##################### ---- add pass/fail variable ---- #########################

d = d %>% mutate(pass = (course > MIN_PASS) + 0) # indicator for pass/fail



## end of proceprocess.R
