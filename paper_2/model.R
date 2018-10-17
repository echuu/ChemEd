
# model.R


setwd('/home/eric/Dropbox/chem_ed')

source("/home/eric/ChemEd/paper_2/misc.R") # load in the misc.R functions

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


## modeling tasks:
 
#################### ----------- REGRESSION --------------- ####################


#### linear regression for modeling course average -----------------------------

## linear regression (full); course ~ all features (omit created features in
#                                                   figures.R, correlated)

# m0: course_avg ~ . (omit individual must Q's, omit CQ's)
    # fit the full model, exclude indicators for individual questions
    # exlucde comm, use alg + conc breakdown since we want to see the role of 
    # algorithmic vs. conceptual in predicting course average
m0 = lm(course ~ . - pass - comm, x_train[,!(names(x0) %in% all_questions)])
summary(m0)
m0_coeffs = summary(m0)$coefficients
getMSE(m0, x_test, x_test$course) # 74.7165

# m1: course ~ must + ... (omit comm, conc, alg, individual q's)
m1 = lm(course ~ . - pass - comm - conc - alg, 
        x_train[,!(names(x0) %in% all_questions)])
summary(m1)
m1_coeffs = summary(m1)$coefficients
getMSE(m1, x_test, x_test$course) # 96.38957

# m2: course ~ conc + alg + ... (omit comm, must, inidividual q's)
m2 = lm(course ~ . - pass - comm - must, 
        x_train[,!(names(x0) %in% all_questions)])
summary(m2)
m2_coeffs = summary(m2)$coefficients
getMSE(m2, x_test, x_test$course) # 86.69607

# ------------------------------------------------------------------------------

## lasso regression for feature selection --------------------------------------

# build model matrix, must exclude: all response columns, all_questions

x_train_0 = x_train[, -c(2,47)]  # omit response variables
x_train_0 = x_train_0[,!(names(x_train_0) %in% all_questions)] # omit questions
x_test_0 = x_test[,-c(2,47)]
x_test_0 = x_test_0[,!(names(x_test_0) %in% all_questions)] # omit questions

y_train = x_train$course
y_test  = x_test$course

xtrain_mat = model.matrix( ~ . - 1, x_train_0)  # one-hot-encoding for factors
xtest_mat = model.matrix( ~ . - 1, x_test_0)

## m_lasso0: course ~ .
set.seed(1)
m_lasso0  = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_lasso0 = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
lambda_star0 = cv_lasso0$lambda.min # optimal lambda: 0.1872694
lasso_pred0  = predict(m_lasso0, s = lambda_star0, newx = xtest_mat)
mean((lasso_pred0 - y_test)^2) # MSE: 76.25308

# examine coefficients:
predict(m_lasso0, type = 'coefficients', s = lambda_star0)

# ------------------------------------------------------------------------------


## linear regression using (LASSO) selected variables

# only variable that would be excluded is 'conc' 
# may not be worth refitting entire model just to exclude one variable
# even if we refit, the coefficients estimates would be different, and the 
# 'gained' advantage of having uncertainty estimates of each coefficient would 
# be lost since they would not correspond to the LASSO model
# would not be unreasonable to keep one or the other, both agree on the
# 'important' features, and the predictive abilities (mse) are similar

# ------------------------------------------------------------------------------


################## ----------- CLASSIFICATION --------------- ##################


#### logistic regression for modeling pass/fail --------------------------------

# pass/fail model 0: pass ~ . (omit individual must Q's, omit CQ's)
    # use both must + common qs' to predict pass/fail
pf_m0 = glm(as.factor(pass) ~ . - course - comm, family = 'binomial',
            x_train[,!(names(x0) %in% all_questions)])
summary(pf_m0)

# evaluate classification here:


# pass/fail model 1: pass ~ must + ... (omit comm, conc, alg, individual q's)
    # use only must to predict pass/fail
pf_m1 = glm(as.factor(pass) ~ . - course - comm - conc - alg, 
            family = 'binomial', x_train[,!(names(x0) %in% all_questions)])
summary(pf_m1)

# evaluate classification here:


# pass/fail model 2: pass ~ conc + alg + ... (omit comm, must, inidividual q's)
    # use only alg + conc to predict pass/fail (common questions)
pf_m2 = glm(as.factor(pass) ~ . - course - comm - must, family = 'binomial',
            x_train[,!(names(x0) %in% all_questions)])
summary(pf_m2)


# evaluate classification here:



# ------------------------------------------------------------------------------




# ------------------------------   TO DO    ------------------------------------


## relationship between common questions and MUST

  ## common questions as a function of MUST and the other explanatory variables
  
      ## nonlinear model (linear regression with polynomial term for MUST)
      ## some of the figures indicated a nonlienar relationship


## difference (if any) between conceptual vs algorithmic (this is somewhat 
##  addressed when examining the coefficients for alg and conc in previous
##  modeling steps)
    ## algorithmic as a function of MUST + other
    ## conceptual as a function of MUST + other
    ## compare these models
    ## think of a way to do hypothesis test between using alg and conc




# end of model.R
