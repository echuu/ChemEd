

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
# d$class[d$class == "SO " ] = "SO" ### fixed

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
table(d$gender)

# parents
table(d$parent)
d$parent = as.character(d$parent)
d$parent[d$parent == "n"] = "N"
d$parent[!(d$parent %in% c("N", "Y"))] = "DK"
d$parent = as.factor(d$parent)


# grands
table(d$grand)
d$grand = as.character(d$grand)
d$grand[d$grand == "n"] = "N"
d$grand[d$grand == "N "] = "N"
d$grand[!(d$grand %in% c("N", "Y"))] = "DK"
d$grand = as.factor(d$grand)
table(d$grand)

# version
table(d$ver)
str(d$ver) 
d$ver = as.factor(d$ver) # factor: levels 78, 87
table(d$ver)

# hrs
table(d$hrs)
d$hrs = as.character(d$hrs)
sum(is.na(d$hrs)) # no missing values, only empty strings?
d$hrs[d$hrs == ""] = "0"
d$hrs = as.factor(d$hrs)
d$hrs = droplevels(d$hrs)
table(d$hrs) # 6 groups depending on hours working


# emp_on:
table(d$emp_on)
d$emp_on = as.character(d$emp_on)
d$emp_on[d$emp_on == ""] = "N"
d$emp_on[d$emp_on == "n"] = "N"
table(d$emp_on)
d$emp_on = as.factor(d$emp_on)
table(d$emp_on)


# emp_off:
table(d$emp_off)
d$emp_off = as.character(d$emp_off)
d$emp_off[d$emp_off == ""] = "N"
d$emp_off[d$emp_off == "B"] = "N"
table(d$emp_off)
d$emp_off = as.factor(d$emp_off)
table(d$emp_off)


# majors
stem  = c("a", "a. Eng", "a. Math", "a.Sci", "a. Sci", "a. Sci,Math",
          "a. Sci, Tech", "a. Sc/Math", "a. Tech", "a. Tech,Eng", 
          "a. Tech,Math", "STEM") 
med   = c("b", "c + b", "SCI: c, b & b", "SCI: c-b; b: ALL") # medical students
dual  = c("a. Sci & b", "b+a. Eng", "d", "STEM & b") # stem + non-stem 
other = c("", " ", "c") # other students, includes blank responses

all = c(stem, med, dual, other)

sum(!(d$major %in% all))

# length(stem) + length(med) + length(dual) + length(other)

d$major = as.character(d$major)
d$major[d$major %in% stem]  = "stem"
d$major[d$major %in% med]   = "med"
d$major[d$major %in% dual]  = "dual"
d$major[d$major %in% other] = "other"

table(d$major)

d$major = as.factor(d$major)

write.csv(d, "must.csv", row.names = FALSE)



