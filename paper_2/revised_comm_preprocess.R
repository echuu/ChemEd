

# revised_comm_preprocess.R
library(dplyr)
# this file is used to find incorrectly encoded variables so that we can go 
# through by hand to edit them - it will be easier to do this by hand since
# we're just doing this once

d = read.csv("must_og.csv") # 1020 x 48

## GLOBAL VARS
n = nrow(d) # number of observations
school_names = c("ACUP", "TAMSA", "TAMU", "TSU", "UNTB", "UNTW", "UTXW")
MIN_PASS = 70 # lowest passing course score


#################### ---- reformat school variable ---- ########################

## remove the suffixes of each of the school so to extract 4-5 letter abbrev
id      = as.character(d[,1])  # school name is in the first column
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

# replace the existing school column with the newly encoded school IDs
d = d %>% mutate(school = id_inst)

# merge UNT schools
d[d$school == "UNTB" | d$school == "UNTW",]$school = "UNT"

table(d$school) # 5 possible schools: ACUP, TAMSA, TAMU, TSU, UNT, UTXW 

##################### ---- add pass/fail variable ---- #########################

d = d %>% mutate(pass = (course > MIN_PASS) + 0) # indicator for pass/fail

d = d %>% mutate(alg = Q1A + Q2A + Q3A + Q4A + Q5A + Q6A)
d = d %>% mutate(conc = Q1C + Q2C + Q3C + Q4C + Q5C + Q6C)

# at this point, d is (1020 x 48), we plan on omitting 'zip' variable when 
# modeling, but there are some tables that look at scores over different
# zip codes, we keep the variable in the data set for now

# next: look in comm_preprocess.R file for the step by step changes we need
#       to make to obtain the final (clean) dataset


d %>% select(class) %>% table
d$class[d$class == "SO " ] = "SO" ### fixed



