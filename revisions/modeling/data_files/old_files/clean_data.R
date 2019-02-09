

# cleaning the data for the millionth time

library(dplyr)

setwd("C:/Users/chuu/ChemEd/revisions/data_cleaning")

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

d = d[,1:32] # omit columns with common questions for this analysis

# write to file to do intermediate (manual) cleaning
write.csv(d, "must_partial_clean.csv", row.names = FALSE)

### -------------------------------------------------------------------------

# read in edited must_partial_clean.csv
d = read.csv("must_partial_clean.csv") # 1073 x 32


# remove rows without course average reported
d = d[!is.na(d$course),]   # 1107 x 34


d_miss = d[rowSums(is.na(d)) > 0,] # a 

##################### ---- add pass/fail variable ---- #########################

d = d %>% mutate(pass = (course > MIN_PASS) + 0) # indicator for pass/fail


#####################    process variable values    ############################

# remove letter variable
# d$letter = NULL


## Many of the variables contain values that are incorrectly inputted, creating
## extra factor levels. Here we manually go through and fix the inputs

# class (FR/)
d %>% select(class) %>% table
# d$class[d$class == "SO " ] = "SO"
# d$class[d$class == "SP" ]  = "SO"
# d$class = droplevels(d$class)      # drop levels with no observations

## PB class variable ???

# ethnicity (Paper's Count // My Count), * indicates count discrepancy
# Asian      :  80     
# Black      :  70    
# Hispanic   : 299
# Mixed      :  98    
# White      :  41  
# Other      : 519  

# Other (Arabic, Asian Indian, Native American, Pacific Islander, Pakistani)
# note: this new def of the group 'Other' differs slightly from part 1 analysis
table(d$ethnic)
# length(table(d$ethnic))
# table(d$ethnic)
# ethn_names = names(table(d$ethnic))
# mixed = c("White,Nat Am", "White,PI", "White, Asian", "White,AsianInd",
#          "White,Black", "White,Hisp", " White,NA",
#          "White, NA", "White,Asian", "White,Arab", "Mixed", "Hisp,White",
#          "Hisp,Arab", "Hisp,Asian", "Hisp,AsianInd", "Hisp,Black", "Hisp, NA",
#          "Hisp &White", "Black,White", "Black,Hisp", "Asian,White", 
#          "Asian, PI")
# sum(!(mixed %in% ethn_names)) # check for mis-spelled ethnicities

d$ethnic = as.character(d$ethnic)
# d$ethnic[(d$ethnic %in% mixed)] = "Mixed"  # group Mixed

# d$ethnic[d$ethnic == "Whte"] = "White"     # White mispelled

# ethn_groups = c("Other", "Asian", "White", "Mixed", "Hisp", "Black")
# d$ethnic[!(d$ethnic %in% ethn_groups)] = "Other"
d$ethnic = as.factor(d$ethnic)


# gender: 0/female 642, 1/male 454, -1/missing 11 
table(d$gender)
d$gender[is.na(d$gender)] = -1
d$gender = as.character(d$gender)
# d$gender[d$gender == ""] = -1
# d$gender[d$gender == "O"] = 0
d$gender = as.factor(d$gender)


## DONE UP TIL HERE --------------------------------------------------

table(d$gp_edu)
d$gp_edu = as.character(d$gp_edu)
edu = c("NN", "YY", "NY", "YN", "Other")
d$gp_edu[!(d$gp_edu %in% edu)] = "Other"
d$gp_edu = as.factor(d$gp_edu)


# parents: N (321), Y (766), Other (20)
table(d$parents)
d$parents = as.character(d$parents)
d$parents[d$parents == "n"] = "N"
d$parents[!(d$parents %in% c("N", "Y"))] = "Other"
d$parents = as.factor(d$parents)

# grands: N (512), Y (446), Other (149)
table(d$grands)
d$grands = as.character(d$grands)
d$grands[d$grands == "n"] = "N"
d$grands[d$grands == "N "] = "N"
d$grands[!(d$grands %in% c("N", "Y"))] = "Other"
d$grands = as.factor(d$grands)

# version
str(d$version) 
d$version = as.factor(d$version) # factor: levels 78, 87


# no rows with missing values
rows_NA = d[rowSums(is.na(d)) > 0,] 

# incorrectly encoded answer for MQ7
table(d$MQ7)
i = which(d$MQ7 == "`0")
d$MQ7[i] = 0
d$MQ7 = droplevels(d$MQ7)
d$MQ7 = as.numeric(as.character(d$MQ7))


colSums(apply(d[,12:31], 2, table)) # check that all must questions encoded


# remove emp_on, emp_off variables
d$emp_off = NULL
d$emp_on  = NULL

# dsave = d

# check point ------------------------------------------------------------------

# re-encode the major column: 
# STEM                    : 512
# medical                 : 479
# dual (stem + non-stem)  :  29 
# other                   :  87

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
          "a. Tech,Math", "STEM", "a. Sci,Tech", "MATH") 
med   = c("b", "c + b", "SCI: c, b & b", 
          "SCI: c-b; b: ALL", "a. Sci+c")            # medical students
dual  = c("a. Sci & b", "b+a. Eng", "d", "STEM & b") # stem + non-stem 
other = c("", " ", "c", "Other", "C")                # other students, blanks

# length(stem) + length(med) + length(dual) + length(other)

d$major = as.character(d$major)
d$major[d$major %in% stem]  = "stem"
d$major[d$major %in% med]   = "med"
d$major[d$major %in% dual]  = "dual"
d$major[d$major %in% other] = "other"

table(d$major)
d$major = as.factor(d$major)

str(d) # make sure all strings are saved as factors, keep must q's as numeric
       # for now to form category sums, later will be turned into factors


# check point ------------------------------------------------------------------


# form old must score (subtract MQ6, MQ16, MQ17, MQ18 from must)

d = d %>% mutate(old_must = must - MQ6 - MQ16 - MQ17 - MQ18)

# create 5 category scores (see revision_notes.txt for details)
d = d %>% mutate(mult    = MQ1  +  MQ2  + MQ3,
                 div     = MQ4  +  MQ6  + MQ7  + MQ8  + MQ16,
                 frac    = MQ9  +  MQ10 + MQ17 + MQ18,
                 log_exp = MQ5  +  MQ12 + MQ13 + MQ14 + MQ15,
                 chem    = MQ11 +  MQ19 + MQ20)


# final dimensions to write to file: 1107 x 37

write.csv(d, "must.csv", row.names = FALSE)

test_read = read.csv("must.csv") # check to make sure gender, MQ's are factors
                                 # before begin modeling




