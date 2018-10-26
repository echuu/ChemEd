

# originalModels.R

## build the basic models: 
    # course ~ must
    # course ~ comm
    # course ~ must + comm

## compare this with the full model as created in model.R


setwd('/home/eric/Dropbox/chem_ed')

source("/home/eric/ChemEd/paper_2/misc.R") # load in the misc.R functions

library(MASS)
library(dplyr)
library(glmnet)

x = read.csv("part2/data/x_model.csv") # 1020 x 60

## train/test split --- training set will have 2/3 of the total data

# variables to omit from model features (10 features to be omitted)
#    grand      :  value accounted for in gp_edu
#    parent     :  value accounted for in gp_edu
#    hs_chem    :  poor encoding
#    math_comp  :  poor encoding
#    curr_math  :  poor encoding
#    chem1      :  
#    chem2      : 
#    letter     :  calculated using the course average
#    final_exam :  included in the calculation of the course average
#    zip        :  poorly encoded

# one of the two is omitted depending on the task (regression/class.)
#    pass       :  calculated using the course average
#    course     :  used to calculate the 'pass' variable

# depending on modeling choice, we can omit 
#    (1) alg and conc since they are sum of Q1A to Q6A and Q1C to Q6C
#    (2) must and comm since they are sum of MQ1 to MQ20, Q1A/C to Q6A/C

# varibales to omit
omit_vars   = c("grand", "parent", "hs_chem", "math_comp", "curr_math", 
                "chem1", "chem2", "letter", "final_exam", "zip")


# create the dataframe used for modeling; note that depending on the task,
# either course or pass will need to be omitted from the feature space
# since course determines values of pass
x0   = x[,-which(names(x) %in% omit_vars)]   # 1020 x 50


# transform indicator variables to factors:
# gender, version, MQ1-MQ20, Q1A-Q6C
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")
COMM_q = paste(rep(paste("Q", 1:6, sep = ""), 2), 
               c(rep("A", 6), rep("C", 6)), sep = "")
all_questions = c(MUST_q, COMM_q)
numer_cols = c("gender", "version", all_questions)

x0[,numer_cols] = lapply(x0[,numer_cols], factor) # numeric indicators -> factor

train_test = generateTrainTest(x0, seed = 0)

x_train = train_test[[1]]   # 682 x 51
x_test  = train_test[[2]]   # 338 x 51 


#### linear regression for modeling course average -----------------------------

## linear regression (full); course ~ all features (omit created features in
#                                                   figures.R, correlated)

# m0: course_avg ~ . (omit individual must Q's, omit CQ's)
    # fit the full model, exclude indicators for individual questions
    # exlucde comm, use alg + conc breakdown since we want to see the role of 
    # algorithmic vs. conceptual in predicting course average
full = lm(course ~ . - pass - comm, x_train[,!(names(x0) %in% all_questions)])
summary(full)
full_coeffs = summary(full)$coefficients
getMSE(full, x_test, x_test$course) # 74.7165



#### basic models --------------------------------------------------------------

## build the basic models: 
# course ~ must
basic_must = lm(course ~ must, x_train)
summary(basic_must) # must is significant
must_coeffs = summary(basic_must)$coefficients
getMSE(basic_must, x_test, x_test$course) # 110.9036


# course ~ comm
basic_comm = lm(course ~ comm, x_train)
summary(basic_comm) # comm is significant
comm_coeffs = summary(basic_comm)$coefficients
getMSE(basic_comm, x_test, x_test$course) # 100.0315


# course ~ must + comm
basic_must_comm = lm(course ~ must + comm, x_train)
summary(basic_must_comm) # both must and comm significant
basic_must_comm_coeffs = summary(basic_must_comm)$coefficients
getMSE(basic_must_comm, x_test, x_test$course) # 89.75877



# end of originalModels.R
