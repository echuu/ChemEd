
# model.R

setwd('/home/eric/Dropbox/chem_ed')

library(MASS)
library(dplyr)
library(glmnet)

############################     variables    ##################################
#-------------------------------------------------------------------------------

# school          :  TX universities (ACUP, TAMSA, TAMU, TSU, UNTB, UNTW, UTXW)
# letter          :  letter grade in chem1 (A, B, C, D, F)
# final_exam      :  percentage grade on final exam
# course          :  course average percentage; our primary regression response
# class           :  class standing (FR, SO, JR, SR)
# gender          :  indicator for male, if not reported, then -1
# ethnic          :  ethnicity (Asian, Black, Hisp, Mixed, White, Other)
# tx_birth        :  indicator for born in Texas
# tx_hs           :  indiator for attended high school in Texas
# grand           :  indicator for grandparents college education (N, Y, Other)
# parent          :  indicator for parents college education (N, Y, Other)
# emp_on          :  indicator for employment on campus (0, 1)
# emp_off         :  indicator for employment off campus (0, 1)
# hrs             :  hours worked a week (0, 1-10, 11-19, 20-29, 30-39, 40+)
# version         :  MUST version (78, 87)
# MQ1-MQ20        :  MUST questions
# must            :  MUST score: sum of MQ1-MQ20 (0, 20)
# Q1A-Q6A         :  algorithmic common questions (0, 1)
# Q1C-Q6C         :  conceptual common questions (0, 1)
# comm            :  sum of all 12 common questions (0, 12)
# pass            :  indicator variable for course_avg >= 70
# alg             :  sum of 6 algorithmic questions, Q1A-Q6A (0, 6)
# conc            :  sum of 6 conceptual questions, Q1C-Q6C (0, 6)
# gp_edu          :  grandparents/parents education code (N/A, NN, NY, YN, YY)


## variables (some temporarily) excluded due to ambiguous/poor encoding

# curr_math       :  description of current math level, poorly encoded; excluded
# chem1           :  ambiguous encoding; exlcuded for now
# chem2           :  indicator for chem 2, poorly encoded; excluded for now
# major           :  major; needs to be re-encoded; excluded for now
# zip             :  first two numbers of zip code; poorly encoded; excluded
# hs_chem         :  description for high school chem taken; poorly encoded
# math_comp       :  description for math level; poorly encoded, excluded


#### ---------------------------------------------------------------------------


x = read.csv("part2/data/x_model.csv") # 1020 x 60

## train/test split --- training set will have 2/3 of the total data

# variables to omit from model features (9 features to be omitted)
#    grand      :  value accounted for in gp_edu
#    parent     :  value accounted for in gp_edu
#    hs_chem    :  poor encoding
#    math_comp  :  poor encoding
#    curr_math  :  poor encoding
#    chem1      :  
#    chem2      : 
#    letter     :  calculated using the course average
#    final_exam :  included in the calculation of the course average

# one of the two is omitted depending on the task (regression/class.)
#    pass       :  calculated using the course average
#    course     :  used to calculate the 'pass' variable


# varibales to omit
omit_vars   = c("grand", "parent", "hs_chem", "math_comp", "curr_math", 
                "chem1", "chem2", "letter", "final_exam")

# create the dataframe used for modeling; note that depending on the task,
# either course or pass will need to be omitted from the feature space
# since course determines values of pass
x0   = x[,-which(names(x) %in% omit_vars)]   # 1020 x 51

train_test = generateTrainTest(x0, seed = 0)

x_train = train_test[[1]] # 682 x 51
x_test  = train_test[[2]] # 338 x 51 


## modeling tasks:

#### linear regression for modeling course average -----------------------------

## linear regression (full); course ~ all features (omit created features from
#                                                   figures, correlated)

# variables to omit: 
#    


# m0: course_avg ~ must + rest (omit individual must Q's, omit CQ's)
# m1: course_avg ~ Q1 + ... + Q20 + rest
# m2: course_avg ~ cq


## lasso regression for feature selection




## linear regression using (LASSO) selected variables



# ------------------------------------------------------------------------------


#### logistic regression for modeling pass/fail --------------------------------







# ------------------------------------------------------------------------------


## repeat the models done for paper 1



## using common questions as a feature to predict course average ---------------




# ------------------------------------------------------------------------------



## predictive model using just common questions (compare vs. MUST)





## common questions as a function of MUST and the other explanatory variables

    ## nonlinear model (linear regression with polynomial term for MUST)
    ## some of the figures indicated a nonlienar relationship




## relationship between common questions and MUST



## difference (if any) between conceptual vs algorithmic
    ## algorithmic as a function of MUST + other
    ## conceptual as a function of MUST + other
    ## compare these models
    ## think of a way to do hypothesis test between using alg and conc




## include the new data from the 'common' questions




# end of figures.R
