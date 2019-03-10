
# model.R


setwd("C:/Users/chuu/ChemEd/paper_2")

source("C:/Users/chuu/ChemEd/paper_2/misc.R") # load in the misc.R functions

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
# tx_hs           :  indicator for attended high school in Texas
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




#### ---------------------------------------------------------------------------


x = read.csv("must.csv") # 1020 x 50

# omit 'zip' variable from the grand list of variables because of poor encoding
var_names = c("school",  "course" ,  "class"  , "gender"  , "ethnic" , "major",  
              "grand" ,  "parent" ,  "emp_on" , "emp_off" , "hrs"    , "ver"  ,    
              "MQ1"   ,  "MQ2"    ,  "MQ3"    , "MQ4"     , "MQ5"    , "MQ6"  ,  
              "MQ7"   ,  "MQ8"    ,  "MQ9"    , "MQ10"    , "MQ11"   , "MQ12" ,  
              "MQ13"  ,  "MQ14"   ,  "MQ15"   , "MQ16"    , "MQ17"   , "MQ18" ,  
              "MQ19"  ,  "MQ20"   ,  "must"   , "Q1A"     , "Q1C"    , "Q2A"  ,  
              "Q2C"   ,  "Q3A"    ,  "Q3C"    , "Q4A"     , "Q4C"    , "Q5A"  ,  
              "Q5C"   ,  "Q6A"    ,  "Q6C"    , "comm"    , "pass"   , "alg"  ,  
              "conc")

response_vars = c("course", "pass")

x = x[,which(names(x) %in% var_names)] # 1020 x 49


## train/test split --- training set will have 2/3 of the total data

# one of the two is omitted depending on the task (regression/class.)
#    pass       :  calculated using the course average
#    course     :  used to calculate the 'pass' variable

# depending on modeling choice, we can omit 
#    (1) alg and conc since they are sum of Q1A to Q6A and Q1C to Q6C
#    (2) must and comm since they are sum of MQ1 to MQ20, Q1A/C to Q6A/C


# transform indicator variables to factors:
    # gender, version, MQ1-MQ20, Q1A-Q6C
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")
COMM_q = paste(rep(paste("Q", 1:6, sep = ""), 2), 
               c(rep("A", 6), rep("C", 6)), sep = "")
all_questions = c(MUST_q, COMM_q)
numer_cols = c("gender", "ver", "pass", all_questions)

x[,numer_cols] = lapply(x[,numer_cols], factor) # numeric indicators -> factor
str(x)

train_test = generateTrainTest(x, seed = 0)

x_train = train_test[[1]]   # 682 x 49
x_test  = train_test[[2]]   # 338 x 49 


## modeling tasks:
 
#################### ----------- REGRESSION --------------- ####################

#### linear regression for modeling course average -----------------------------

# (0) --------------------------------------------------------------------------
# m0: course_avg ~ . (omit individual must Q's, omit CQ's)
    # model description: model the course average using all features, using
    #                    must, cq (conc + alg) totals rather than individual q's
    # omit: individual questions, comm (use alg + conc), pass

# omit 'pass' variable since it is a function of the course average
vars_omit = c(all_questions, "pass", "comm")
var_subset = !(var_names %in% vars_omit)
var_names[!(var_names %in% vars_omit)]
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 15
m0 = lm(course ~ . , x_train[,var_subset])
summary(m0)
m0_coeffs = summary(m0)$coefficients
getMSE(m0, x_test, x_test$course)                # TEST MSE:    75.78647

# save m0 coefficients to csv file
coeffs_out = round(m0_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/reg/course_full_coef.csv")


# (1) --------------------------------------------------------------------------
# m1: course ~ must + ...
    # model description: model the course average using must total and omitting
    #                    any CQ-related variables in order to gauge the 
    #                    predictive ability of the MUST
    # omit: individual q's, comm, conc, alg, pass
vars_omit = c(all_questions, "pass", "comm", "alg", "conc")
var_names[!(var_names %in% vars_omit)]
var_subset = !(var_names %in% vars_omit)
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 13
m1 = lm(course ~ . , x_train[,var_subset])
summary(m1)
m1_coeffs = summary(m1)$coefficients
getMSE(m1, x_test, x_test$course)                # TEST MSE:    105.3188

# save m1 coefficients to csv file
coeffs_out = round(m1_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/reg/course_must_coef.csv")


# (2) --------------------------------------------------------------------------
# m2: course ~ conc + alg + ...
    # model description: model the course average using conc, alg totals, and
    #                    omitting any CQ-related variables in order to gauge the 
    #                    predictive ability of the COMMON QUESTIONS
    # omit: individual q's, comm, must, pass
vars_omit = c(all_questions, "must", "pass", "comm")
var_names[!(var_names %in% vars_omit)]
var_subset = !(var_names %in% vars_omit)
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 14
m2 = lm(course ~ . , x_train[,var_subset])     
summary(m2)
m2_coeffs = summary(m2)$coefficients
getMSE(m2, x_test, x_test$course)                # TEST MSE:    86.62713

# save m2 coefficients to csv file
coeffs_out = round(m2_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/reg/course_comm_coef.csv")

# ------------------------------------------------------------------------------

## (3) lasso regression for feature selection ----------------------------------

# m3: course ~ ...
    # model description: model course average using all features using LASSO
    #                    for feature selection (use conc + alg instead of comm)
    # omit: individual q's, comm

# in the model_matrix, omit response variables (course, pass), individual q's,
# and 'comm' since we will be using conc + alg instead
vars_omit = c(response_vars, all_questions, "comm")
var_names[!(var_names %in% vars_omit)]
var_subset = !(var_names %in% vars_omit)
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 14

# set up xtrain, xtest, ytrain, ytest
xtrain_lasso = x_train[,var_subset]
xtest_lasso  = x_test[,var_subset]
y_train      = x_train$course
y_test       = x_test$course

ncol(xtrain_lasso) == ncol(xtest_lasso) # should print TRUE

xtrain_mat = model.matrix( ~ . , xtrain_lasso)[,-1] # 682 x 33
xtest_mat  = model.matrix( ~ . , xtest_lasso)[,-1]  # 338 x 33

set.seed(1)
course_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_course_lasso = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
(lambda_star0 = cv_course_lasso$lambda.min)      # optimal lambda: 0.1902754

# lasso model coefficients
coeffs_l0 = predict(course_lasso, type = 'coefficients', s = lambda_star0) 
lasso_pred0  = predict(course_lasso, s = lambda_star0, newx = xtest_mat)
mean((lasso_pred0 - y_test)^2)                   # TEST MSE: 76.5686

# save lasso coefficients to csv
coeffs_out = as.data.frame(as.matrix(round(coeffs_l0, 4)))
write.csv(coeffs_out, "model_coeffs/reg/course_lasso.csv")

# ------------------------------------------------------------------------------



# end of comm_model_reg.R
