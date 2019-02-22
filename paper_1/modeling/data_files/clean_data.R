
# update_clean.R


# updated data cleaning script
# the updated data has fewer inconsistencies than did the previous files, 
# so we can omit some of the previous code used for cleaning up the data

library(dplyr)

setwd("C:/Users/chuu/ChemEd/revisions/modeling/data_files")

d = read.csv("must_final.csv")

## GLOBAL VARS
n = nrow(d) # number of observations
school_names = c("ACUP", "TAMSA", "TAMU", "TSU", "UNTB", "UNTW", "UTXW")
MIN_PASS = 70 # lowest passing course score


################# ---- reformat school variable ---- ########################


# names(d)[1] = "school"

## remove the suffixes of each of the school so to extract 4-5 letter abbrev
id      = as.character(d[,1])
id_inst = character(n)
for(i in 1:n) {
    n_char <- nchar(id[i])
    id_inst[i] <- substr(id[i], 1, n_char - 3) # abbreviate schools
}

## Store just the 7 institution identifiers.
for (s in school_names) {
    id_inst[grep(s, id_inst)] = s
}

## note that the number of observations from each school is different
## when we split into train/test sets, take equal proportions from each school
## so that we do not completely leave out a school
table(id_inst) # 7 schools identified by 4-5 letter codes -- 1073 total

# replace the existing school column with the newly encoded school IDs
d = d %>% mutate(school = id_inst)

# omit entries with missing school; at this point, we should have 1170 x 50
d = d %>% filter(school %in% school_names)

# merge UNTB and UNTW schools into UNT
d[d$school == "UNTB" | d$school == "UNTW",]$school = "UNT"

hrs = d$hrs

d = d[,1:33] # omit columns with common questions for this analysis

# write to file to do intermediate (manual) cleaning
write.csv(d, "must_partial_clean.csv", row.names = FALSE)

### -------------------------------------------------------------------------

# read in edited must_partial_clean.csv
d = read.csv("must_partial_clean.csv") # 1073 x 32

# create pass indicator variable
d = d %>% mutate(pass = (course > MIN_PASS) + 0) # indicator for pass/fail

# check the class variable (4 outcomes)
d %>% select(class) %>% table

# check ethnicity variable (Asian, Black, Hispanic, Mixed, Other, White)
d %>% select(ethnic) %>% table
d$ethnic = as.factor(d$ethnic)

# gender: 0/female 642, 1/male 454, -1/missing 9 
table(d$gender)
d$gender[is.na(d$gender)] = -1
d$gender = as.character(d$gender)
d$gender = as.factor(d$gender)

# major: 
    # STEM                    : 494
    # medical                 : 469
    # dual (stem + non-stem)  :  28
    # other                   :  82

# stem students
stem  = c("a", "a. Eng", "a. Math", "a.Sci", "a. Sci", "a. Sci,Math",
          "a. Sci, Tech", "a. Sc/Math", "a. Tech", "a. Tech,Eng", 
          "a. Tech,Math", "STEM", "a. Sci,Tech", "MATH")
# medical students
med   = c("b", "c + b", "SCI: c, b & b", 
          "SCI: c-b; b: ALL", "a. Sci+c")            
# stem + non-stem 
dual  = c("a. Sci & b", "b+a. Eng", "d", "STEM & b", "SCI: T&E; b") 
# other students, blanks
other = c("", " ", "c", "Other", "C")                

# length(stem) + length(med) + length(dual) + length(other)

d$major = as.character(d$major)
d$major[d$major %in% stem]  = "stem"
d$major[d$major %in% med]   = "med"
d$major[d$major %in% dual]  = "dual"
d$major[d$major %in% other] = "other"

table(d$major)
d$major = as.factor(d$major)

# parents: N (305), Y (751), DK (17)
table(d$parent)
sum(is.na(d$parent))
str(d$parent)  # should be encoded as factor

# grands: N (491), Y (437), Other (145)
table(d$grand)
sum(is.na(d$grand))
str(d$grand)  # should be encoded as factor

# employment status (on/off campus)
table(d$emp_off)
str(d$emp_off)  # should be encoded as factor
table(d$emp_on)
str(d$emp_on)   # should be encoded as factor

# properly encode d$hrs variable
table(d$hrs)

## hrs  :    0 | 1-10 |  11-19 |  20-29 |  30-39  |   40+ 
## total:  813 |   63 |     97 |     66 |     25  |     9 

str(d$hrs)


# version
str(d$ver) 
d$ver = as.factor(d$ver) # factor: levels 78, 87

# check that all must questions encoded all values should be 1073
colSums(apply(d[,12:31], 2, table)) 


#### to do: below --------------------------------------------------------------

# create question categories
d = d %>% mutate(old_must = must - MQ6 - MQ16 - MQ17 - MQ18)

# create 5 category scores (see revision_notes.txt for details)
d = d %>% mutate(mult    = MQ1  +  MQ2  + MQ3,
                 div     = MQ4  +  MQ6  + MQ7  + MQ8  + MQ16,
                 frac    = MQ9  +  MQ10 + MQ17 + MQ18,
                 log_exp = MQ5  +  MQ12 + MQ13 + MQ14 + MQ15,
                 symb    = MQ11 +  MQ19 + MQ20)


# final dimensions to write to file: 1073 x 40
write.csv(d, "must.csv", row.names = FALSE)

# check to make sure gender, MQ's are factors before begin modeling
test_read = read.csv("must.csv") 


## -----------------------------------------------------------------------------
str(d) # make sure all strings are saved as factors, keep must q's as numeric
       # for now to form category sums, later will be turned into factors

# end of update_clean.R
