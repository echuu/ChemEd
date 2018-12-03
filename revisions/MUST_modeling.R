
# MUST_modeling.R

library(MASS)
library(dplyr)
library(glmnet)

# if running this file locally, set the working directory to the folder that 
# contains the pertinent dataset, 'x_model.csv'
setwd('/home/eric/Dropbox/chem_ed/part2/data')

# misc.R contains some useful modeling functions that are specific to 
# this problem
source("/home/eric/ChemEd/paper_2/misc.R")

# Use the dataset that was curated for second paper to perform MUST modeling so
# that future modeling that involves both MUST and Common Questions (CQs)
# use the same dataset, allowing some fluidity between the analyses

## load in dataset
x = read.csv("x_model.csv") # 1020 x 60

## omit the variables irrelevant to the model:
# 10 variables omitted in the following list
omit_vars = c("grand", "parent", "hs_chem", "math_comp", "curr_math", 
              "chem1", "chem2", "letter", "final_exam", "zip")

# since we're considering MUST performance for this paper, we add 
# common question related columns to the omit list
    # transform indicator variables to factors:
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")          # 20 MUST questions
COMM_q = paste(rep(paste("Q", 1:6, sep = ""), 2),      # 12 Common questions
               c(rep("A", 6), rep("C", 6)), sep = "")
# comm = CQ score, alg = algorithmic score, conc = conceptual score
cq_split = c("comm", "alg", "conc") 

# 10 original vars + 12 CQ + 3 CQ-related = 25 columns to omit
omit_vars = c(omit_vars, COMM_q, cq_split) # 25 x 1


# create the dataframe used for modeling; note that depending on the task,
# either course or pass will need to be omitted from the feature space
# since course determines values of pass
x0   = x[,-which(names(x) %in% omit_vars)]   # 1020 x 35


# transform indicator variables to factors:
# gender, version
indicator_vars = c("gender", "version")
x0[,indicator_vars] = lapply(x0[,indicator_vars], factor)

#### ----------------    end initial cleaning    -------------------------- ####


# variable transformations: 

# sum of old must (old must did not have questions 6, 16, 17, 18)
x0 = x0 %>% mutate(old_must = must - MQ6 - MQ16 - MQ17 - MQ18)

# create 5 category scores (see revision_notes.txt for details)
x0 = x0 %>% mutate(mult    = MQ1 + MQ2 + MQ3,
                   div     = MQ4 + MQ6 + MQ7 + MQ8 + MQ16,
                   frac    = MQ9 + MQ10 + MQ17 + MQ18,
                   log_exp = MQ5 + MQ12 + MQ13 + MQ14 + MQ15,
                   chem    = MQ11 + MQ19 + MQ20)

# final dimensions of x0 : 1020 x 41 

# save file
write.csv(x0, "revised_x.csv", row.names = FALSE)

#### ---------------    end variable transformation    -------------------- ####

## generate train/test set
train_test = generateTrainTest(x0, seed = 0)
x_train = train_test[[1]]   # 682 x 41
x_test  = train_test[[2]]   # 338 x 41 


#### ------    regression for modeling course average old vs. new   ------- ####

# questions not included in the old MUST exam
new_qs = c("MQ6", "MQ16", "MQ17", "MQ18", "must") 
cats   = c("mult", "div", "frac", "log_exp", "chem") # category names

# (1a) model using the total must score of old vs. new

# course_new: course average ~ new must + . (682 x 14 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course) # 96.38957

# course_old: course average ~ old must + . (682 x 14 training matrix)
vars_omit = c(MUST_q, cats, "pass", "must")
course_must16 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must16)
must16_coeffs = summary(course_must16)$coefficients
getMSE(course_must16, x_test, x_test$course) # 99.18616


# (1b) model using indicator for each question old vs. new (nested model)



#### ------    regression for modeling course average old vs. new   ------- ####

# (2a) model using category sums



#### -------    lasso reg for modeling course average old vs. new   ------- ####

# (3a) lasso: course average ~ individual must questions (new)

# (3b) lasso: course average ~ category sums 
    # see which categories are indicative of performance



#### ----    classification for modeling course average old vs. new   ----- ####

# (4a) model using the total must score of old vs. new

# (4b) model using indicator for each question old vs. new (nested model)

# (4c) model using category sums








