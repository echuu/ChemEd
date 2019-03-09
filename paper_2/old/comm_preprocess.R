
# preprocess.R

# preprocess the data from part 1, with a few changes: (1) some rows filtered out, 
# (2) results from common questions included 

## clean up the way the data is presented, add some variables of interest

# 12/13 -- moved p2_data to working directory
# setwd('/home/eric/Dropbox/chem_ed')

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

# replace the existing school column with the newly encoded school IDs
d = d %>% mutate(school = id_inst)


##################### ---- add pass/fail variable ---- #########################

d = d %>% mutate(pass = (course > MIN_PASS) + 0) # indicator for pass/fail


######## ---- add separate score for alg and conceptual  questions ----#########

d = d %>% mutate(alg = Q1A + Q2A + Q3A + Q4A + Q5A + Q6A)
d = d %>% mutate(conc = Q1C + Q2C + Q3C + Q4C + Q5C + Q6C)

#####################    process variable values    ############################

## Many of the variables contain values that are incorrectly inputted, creating
## extra factor levels. Here we manually go through and fix the inputs

# class
d %>% select(class) %>% table
d$class[d$class == "SO " ] = "SO"
d$class = droplevels(d$class)     # drop levels with no observations

# ethnicity (Paper's Count // My Count), * indicates count discrepancy
# Asian:     78  //  78
# Black:     60  //  60
# Hispanic: 267  //  267
# Mixed:     85  //  86  *
# White:    493  //  492 * 
# Other:     36  //  37  *

# Other (Arabic, Asian Indian, Native American, Pacific Islander, Pakistani)
# note: this new def of the group 'Other' differs slightly from part 1 analysis

table(d$ethnic)
ethn_names = names(table(d$ethnic))
mixed = c("White,Nat Am", "White,PI", "White, Asian", "White,AsianInd",
          "White,Black", "White,Hisp", " White,NA",
          "White, NA", "White,Asian", "White,Arab", "Mixed", "Hisp,White",
          "Hisp,Arab", "Hisp,Asian", "Hisp,AsianInd", "Hisp,Black", "Hisp, NA",
          "Hisp &White", "Black,White", "Black,Hisp", "Asian,White", 
          "Asian, PI")
sum(!(mixed %in% ethn_names)) # check for mis-spelled ethnicities

d$ethnic = as.character(d$ethnic)
d$ethnic[(d$ethnic %in% mixed)] = "Mixed"  # group Mixed

d$ethnic[d$ethnic == "Whte"] = "White"     # White mispelled
ethn_groups = c("Other", "Asian", "White", "Mixed", "Hisp", "Black")
d$ethnic[!(d$ethnic %in% ethn_groups)] = "Other"

table(d$ethnic)


# gender: 0 (female), 1 (male), -1 (missing)
table(d$gender)
sum(is.na(d$gender)) # 10 missing values
d$gender[is.na(d$gender)] = -1
d$gender = as.factor(d$gender)


# parents
table(d$parent)
d$parent = as.character(d$parent)
d$parent[d$parent == "n"] = "N"
d$parent[!(d$parent %in% c("N", "Y"))] = "Other"
d$parent = as.factor(d$parent)

# grands
table(d$grand)
d$grand = as.character(d$grand)
d$grand[d$grand == "n"] = "N"
d$grand[d$grand == "N "] = "N"
d$grand[!(d$grand %in% c("N", "Y"))] = "Other"
d$grand = as.factor(d$grand)


### done


# version
str(d$version) 
d$version = as.factor(d$version) # factor: levels 78, 87

# tx_hs
d$tx_hs[d$tx_hs != "Y"] = "N"
d$tx_hs = droplevels(d$tx_hs) # too few N's

# tx_birth
d$tx_birth[d$tx_birth != "Y"] = "N"
d$tx_birth = droplevels(d$tx_birth) # too few N's

# hrs
d$hrs = as.character(d$hrs)
d$hrs[is.na(d$hrs)] = 0
# d$hrs[d$hrs == "11-19?"] = "11-19"
# d$hrs[d$hrs == "11-19?"] = "11-19"
d$hrs[d$hrs == "11-10"] = "1-10"
# d$hrs[d$hrs == "0?"] = "0"
# d$hrs[d$hrs == "-"] = "0"
d$hrs[d$hrs == ""] = "0"
d$hrs[d$hrs == "20-39"] = "20-29"
table(d$hrs) # 6 groups depending on hours working
d$hrs = as.factor(d$hrs)

# emp_on:
table(d$emp_on)
d$emp_on = as.character(d$emp_on)
d$emp_on[d$emp_on == ""] = "N"
d$emp_on[d$emp_on == "n"] = "N"
d$emp_on = as.factor(d$emp_on)

# emp_off:
table(d$emp_off)
d$emp_off = as.character(d$emp_off)
d$emp_off[d$emp_off == ""] = "N"
d$emp_off[d$emp_off == "B"] = "N"
d$emp_off[d$emp_off == "T"] = "N"
d$emp_off = as.factor(d$emp_off)




# one of the MQ7 entries has a typo, making the entire column a factor
table(d$MQ7)
i = which(d$MQ7 == "`0")
d$MQ7[i] = 0
d$MQ7 = droplevels(d$MQ7)
d$MQ7 = as.numeric(as.character(d$MQ7))


# re-encode the major column: 
# STEM
# medical
# dual (stem + non-stem)
# other

# original responses (what a mess)
#  [1] ""                 " "                "a"               
#  [4] "a. Eng"           "a. Math"          "a.Sci"           
#  [7] "a. Sci"           "a. Sci & b"       "a. Sci,Math"     
#  [10] "a. Sci, Tech"     "a. Sc/Math"       "a. Tech"         
#  [13] "a. Tech,Eng"      "a. Tech,Math"     "b"               
#  [16] "b+a. Eng"         "c"                "c + b"           
#  [19] "d"                "SCI: c, b & b"    "SCI: c-b; b: ALL"
#  [22] "STEM"             "STEM & b"  

stem  = c("a", "a. Eng", "a. Math", "a.Sci", "a. Sci", "a. Sci,Math",
          "a. Sci, Tech", "a. Sc/Math", "a. Tech", "a. Tech,Eng", 
          "a. Tech,Math", "STEM") 
med   = c("b", "c + b", "SCI: c, b & b", "SCI: c-b; b: ALL") # medical students
dual  = c("a. Sci & b", "b+a. Eng", "d", "STEM & b") # stem + non-stem 
other = c("", " ", "c") # other students, includes blank responses

# length(stem) + length(med) + length(dual) + length(other)

d$major = as.character(d$major)
d$major[d$major %in% stem]  = "stem"
d$major[d$major %in% med]   = "med"
d$major[d$major %in% dual]  = "dual"
d$major[d$major %in% other] = "other"

d$major = as.factor(d$major)


write.csv(d, "part2/data/full_data.csv", row.names = FALSE)

# test_read = read.csv("part2/data/full_data.csv")


## end of proceprocess.R
