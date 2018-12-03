
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

# x0 = read.csv("revised_x.csv")

## generate train/test set
train_test = generateTrainTest(x0, seed = 0)
x_train = train_test[[1]]   # 682 x 41
x_test  = train_test[[2]]   # 338 x 41 


#### ------    regression for modeling course average old vs. new   ------- ####

# questions not included in the old MUST exam
new_qs = c("MQ6", "MQ16", "MQ17", "MQ18") 
cats   = c("mult", "div", "frac", "log_exp", "chem") # category names

# (1) model using the total must score of old vs. new

# (1a) course_old: course average ~ old must + . (682 x 14 training matrix)
vars_omit = c(MUST_q, cats, "pass", "must")
course_must16 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must16)
must16_coeffs = summary(course_must16)$coefficients
getMSE(course_must16, x_test, x_test$course) # 99.18616


# (1b) course_new: course average ~ new must + . (682 x 14 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course) # 96.38957


# (2) model using indicator for each question old vs. new (nested model)
# ensure that each question is encoded as a factor
indicator_vars = c("gender", "version", MUST_q)
x0[,indicator_vars] = lapply(x0[,indicator_vars], as.factor)

## (2a) course_ind16: course average ~ 16 must questions (682 x 29 training)
# omit the 4 questions included in the new MUST
vars_omit = c(cats, new_qs, "pass", "old_must", "must")
course_ind16 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_ind16)
course_ind16_coeffs = summary(course_ind16)$coefficients
getMSE(course_ind16, x_test, x_test$course) # 95.98762

# (2a) course_ind20: course average ~ all 20 must questions (682 x 33 training)
vars_omit = c(cats, "pass", "old_must", "must")
course_ind20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_ind20)
course_ind20_coeffs = summary(course_ind20)$coefficients
getMSE(course_ind20, x_test, x_test$course) # 92.78069

# test for significance between the nested models
anova(course_ind16, course_ind20) # p < 0.05 --> the extra q's are significant


#### ---   regression for modeling course average using question cats   --- ####

# (3) model using category sums

## course_cat: course average ~ must question categories (682 x 18)
vars_omit = c(MUST_q, "pass", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 18

course_cat = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_cat) # all cats but multiplication significant
course_cat_coeffs = summary(course_cat)$coefficients
getMSE(course_cat, x_test, x_test$course) # 93.46515
 

#### -------    lasso reg for modeling course average old vs. new   ------- ####

# (4) lasso: course average ~ individual must questions (682 x 33 training)
     # see if the new questions are not zero'd out by lasso
vars_omit = c(cats, "pass", "must", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 18


# (5) lasso: course average ~ category sums 
    # see which categories are indicative of performance
vars_omit = c(MUST_q, "pass", "must", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 18




#### ----    classification for modeling course average old vs. new   ----- ####

# (6) model using the total must score of old vs. new --------------------------

# (6a) logistic: pass ~ old must + ...
vars_omit = c(MUST_q, cats, "course", "must")     # old
dim(x_train[,!(names(x0) %in% vars_omit)])        # 682 x 14 -- match w/ (1a)
names(x_train[,!(names(x0) %in% vars_omit)])      # 682 x 14

# (6b) logistic: pass ~ new must + ...
vars_omit = c(MUST_q, cats, "course", "old_must") # new
dim(x_train[,!(names(x0) %in% vars_omit)])        # 682 x 14 -- match w/ (1b)
names(x_train[,!(names(x0) %in% vars_omit)])      # 682 x 14

# (7) pass ~ new/old must individual questions (omit category sums) ------------

# (7a) class_indic16: pass ~ 16 old MUST questions + ...
vars_omit = c(cats, new_qs, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 29 -- match with (2a)

# (7b) class_indic20: pass ~ 20 old MUST questions + ...
vars_omit = c(cats, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 33 -- match with (2b)

# (8) class_cat: pass ~ category sums
# class_cat
vars_omit = c(MUST_q, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 682 x 18 -- match with (3)






