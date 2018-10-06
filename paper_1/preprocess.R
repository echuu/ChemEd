
## preprocess.R

library(dplyr)

## Note: run MUST_script.R first to load necessary variables to run this script
## pre-process data

# process X_df so that all variables have non-ambiguous values
# add school as a column of predictors, add the response variable
X_df = dta[, -c(1:3)]
X_df = X_df %>% mutate(score = y, school_id = id_inst)

# subset out Q1 - Q20
X_df = X_df[,-c(20:39)]

# 15 variable names relevant to the model
model_varnames = c("course_avg", "school", "class", "gender", 
                   "ethnicity", "parent", "grand", "hrs_work",
                   "emp_on", "emp_off", "tx_birth", "tx_hs", 
                   "version", "must")  

# X_sub: 1073 x 14
X_sub = X_df %>% select("score", "school_id", "Class", "Gender_Male",
                       "Ethnicity", "Parent", "Grands", "Hrs",
                       "Emp.On", "Emp.Off", "Birth", "Texas.HS", 
                       "MUST.ver", "MUST.Sum")
names(X_sub) = model_varnames

#####################    process variable values    ############################

# class
table(X_sub$class) # has blank level, typo in "SO" creating extra level
X_sub$class[X_sub$class == "SO "] = "SO"
X_sub$class = droplevels(X_sub$class) # drop levels with no observations
levels(X_sub$class) # only 4 levels now

# ethnicity (should be 6 groups)
# Other:     39
# Asian:     79  
# White:    505  
# Mixed:     93  
# Hispanic: 292 
# Black:     65 
length(table(X_sub$ethnicity)) # 38 table entries --> reduce to the 6 groups

# Mixed:
ethn_names = names(table(X_sub$ethnicity))
mixed = c("White,Nat Am", "White,PI", "White, Asian", "White,AsianInd",
          "White,AsIndian", "White,Black", "White,Hisp", " White,NA",
          "White, NA", "White,Asian", "White,Arab", "Mixed", "Hisp,White",
          "Hisp,Arab", "Hisp,Asian", "Hisp,AsianInd", "Hisp,Black", "Hisp, NA",
          "Hisp &White", "Black,White", "Black,Hisp", "Asian,White", 
          "Asian, PI", "AI-PAKASTANI")
sum(!(mixed %in% ethn_names)) # check for mis-spelled ethnicities

X_sub$ethnicity = as.character(X_sub$ethnicity)
X_sub$ethnicity[(X_sub$ethnicity %in% mixed)] = "Mixed"  # group Mixed
X_sub$ethnicity[X_sub$ethnicity == "Whte"] = "White"     # White mispelled

ethn_groups = c("Other", "Asian", "White", "Mixed", "Hisp", "Black")
X_sub$ethnicity[!(X_sub$ethnicity %in% ethn_groups)] = "Other"
X_sub$ethnicity = as.factor(X_sub$ethnicity)
table(X_sub$ethnicity) # 6 ethnic groups


# gender
table(X_sub$gender)
sum(is.na(X_sub$gender))
X_sub$gender = as.character(X_sub$gender)
X_sub$gender[is.na(X_sub$gender)] = "missing"
X_sub$gender = as.factor(X_sub$gender)

# parents
table(X_sub$parent)
X_sub$parent = as.character(X_sub$parent)
X_sub$parent[X_sub$parent == "n"] = "N"
X_sub$parent[!(X_sub$parent %in% c("N", "Y"))] = "Other"
X_sub$parent = as.factor(X_sub$parent)

# grands
table(X_sub$grand)
X_sub$grand = as.character(X_sub$grand)
X_sub$grand[X_sub$grand == "n"] = "N"
X_sub$grand[X_sub$grand == "N "] = "N"
X_sub$grand[!(X_sub$grand %in% c("N", "Y"))] = "Other"
X_sub$grand = as.factor(X_sub$grand)

# version
str(X_sub$version) # should be factor
X_sub$version[X_sub$version == 78] = "v78"
X_sub$version[X_sub$version == "87"] = "v87"
X_sub$version = as.factor(X_sub$version)

# tx_hs
X_sub$tx_hs[X_sub$tx_hs != "Y"] = "N"
X_sub$tx_hs = droplevels(X_sub$tx_hs) # too few N's

# tx_birth
X_sub$tx_birth[X_sub$tx_birth != "Y"] = "N"
X_sub$tx_birth = droplevels(X_sub$tx_birth) # too few N's

# hrs_work
X_sub$hrs_work = as.character(X_sub$hrs_work)
X_sub$hrs_work[is.na(X_sub$hrs_work)] = 0
X_sub$hrs_work[X_sub$hrs_work == "11-19?"] = "11-19"
X_sub$hrs_work[X_sub$hrs_work == "11-19?"] = "11-19"
X_sub$hrs_work[X_sub$hrs_work == "11-10"] = "1-10"
X_sub$hrs_work[X_sub$hrs_work == "0?"] = "0"
X_sub$hrs_work[X_sub$hrs_work == "-"] = "0"
X_sub$hrs_work[X_sub$hrs_work == ""] = "0"
X_sub$hrs_work[X_sub$hrs_work == "20-39"] = "20-29"
table(X_sub$hrs_work) # 6 groups depending on hours working
X_sub$hrs_work = as.factor(X_sub$hrs_work)

# emp_on:
table(X_sub$emp_on)
X_sub$emp_on = as.character(X_sub$emp_on)
X_sub$emp_on[X_sub$emp_on == ""] = "N"
X_sub$emp_on[X_sub$emp_on == "n"] = "N"
X_sub$emp_on = as.factor(X_sub$emp_on)

# emp_off:
table(X_sub$emp_off)
X_sub$emp_off = as.character(X_sub$emp_off)
X_sub$emp_off[X_sub$emp_off == ""] = "N"
X_sub$emp_off[X_sub$emp_off == "B"] = "N"
X_sub$emp_off[X_sub$emp_off == "T"] = "N"
X_sub$emp_off = as.factor(X_sub$emp_off)

# check all variable values are expected
apply(X_sub, 2, table)

# write data out to csv file
write.csv(X_sub, "data/model_x.csv", row.names = FALSE)
# x_read = read.csv("data/model_x.csv")

###################    finish process variable values    #######################









