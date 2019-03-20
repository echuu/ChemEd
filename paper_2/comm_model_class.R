
# comm_model_class.R

setwd("C:/Users/chuu/ChemEd/paper_2")

source("C:/Users/chuu/ChemEd/paper_2/misc.R") # load in the misc.R functions

library(MASS)
library(dplyr)
library(glmnet)

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


# start classification modeling ------------------------------------------------

#### logistic regression for modeling pass/fail --------------------------------


# (0) --------------------------------------------------------------------------
# pf_m0: pass ~ . (full model, with both must and conc + alg)
    # model description: model pass/fail using all features, using must, 
    #                    cq (conc + alg) totals rather than individual q's
    # omit: individual questions, comm (use alg + conc), course

# omit 'course' variable since it is a function of the 'pass'
vars_omit = c(all_questions, "course", "comm")
var_subset = !(var_names %in% vars_omit)
var_names[var_subset]
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 15

pf_m0 = glm(as.factor(pass) ~ ., family = 'binomial', x_train[,var_subset])
summary(pf_m0)

# obtain train/test balanced/overall accuracy, confusion matrix
m0_results = getClassResults(m = pf_m0, x_train = x_train, x_test = x_test)
m0_results$test_balance   # test balanced accuracy :  0.6656854
m0_results$test_overall   # test overall accuracy  :  0.8343195
m0_results$conf_mat       # true values of pass/fail are given by column sums

# write out coefficients
pf_m0_coeffs = summary(pf_m0)$coefficients
coeffs_out = round(pf_m0_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/class/pf_full_coef.csv")


# (1) --------------------------------------------------------------------------
# m1: pass ~ must + ...
    # model description: model pass/fail using must total and omitting
    #                    any CQ-related variables in order to gauge the 
    #                    predictive ability of the MUST
    # omit: individual q's, comm, conc, alg, course

# omit 'course' variable since it is a function of the 'pass'
vars_omit = c(all_questions, "course", "comm", "alg", "conc")
var_subset = !(var_names %in% vars_omit)
var_names[var_subset]
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 13

pf_m1 = glm(as.factor(pass) ~ ., family = 'binomial', x_train[,var_subset])
summary(pf_m1)

m1_results = getClassResults(m = pf_m1, x_train = x_train, x_test = x_test)
m1_results$test_balance   # test balanced accuracy  :  0.6075036
m1_results$test_overall   # test overall accuracy   :  0.739645
m1_results$conf_mat       # true values of pass/fail are given by column sums

# write out coefficients
pf_m1_coeffs = summary(pf_m1)$coefficients
coeffs_out = round(pf_m1_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/class/pf_must_coef.csv")


# (2) --------------------------------------------------------------------------
# m2: pass ~ conc + alg + ...
    # model description: model pass/fail using conc + algo and omitting
    #                    any MUST-related variables in order to gauge the 
    #                    predictive ability of the common questions
    # omit: individual q's, comm, must, course

# omit 'course' variable since it is a function of the 'pass'
vars_omit = c(all_questions, "must", "course", "comm")
var_subset = !(var_names %in% vars_omit)
var_names[var_subset]
dim(x_train[,var_subset])                        # X_TRAIN DIM: 682 x 14

pf_m2 = glm(as.factor(pass) ~ ., family = 'binomial', x_train[,var_subset])
summary(pf_m2)

m2_results = getClassResults(m = pf_m2, x_train = x_train, x_test = x_test)
m2_results$test_balance   # test balanced accuracy  :  0.6729582
m2_results$test_overall   # test overall accuracy   :  0.8461538
m2_results$conf_mat       # true values of pass/fail are given by column sums

# write out coefficients
pf_m2_coeffs = summary(pf_m2)$coefficients
coeffs_out = round(pf_m2_coeffs, 4)
write.csv(coeffs_out, "model_coeffs/class/pf_comm_coef.csv")


# (3) --------------------------------------------------------------------------
#### logistic regression with LASSO for modeling pass/fail ---------------------

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
pf_lasso = glmnet(x = xtrain_mat, y = as.factor(x_train$pass), 
                  alpha = 1, family = 'binomial')         # fit lasso model
cv_pf_lasso = cv.glmnet(x = xtrain_mat, y = as.factor(x_train$pass), 
                        family = 'binomial', alpha = 1) 
lambda_star = cv_pf_lasso$lambda.min               # optimal lambda: 0.01035964

# predict(pf_lasso, type = 'coefficients', s = lambda_star) # coefficients

lasso_results = getClassResults(pf_lasso, x_train, x_test, lambda = lambda_star, 
                                xtrain_mat = xtrain_mat, xtest_mat = xtest_mat)
lasso_results$test_balance   # test balanced accuracy  :  0.6962771
lasso_results$test_overall   # test overall accuracy   :  0.8343195
lasso_results$conf_mat       # true values of pass/fail are given by column sums

# lasso model coefficients
coeffs_lasso = predict(pf_lasso, type = 'coefficients', s = lambda_star) 

# save lasso coefficients to csv
coeffs_out = as.data.frame(as.matrix(round(coeffs_lasso, 4)))
write.csv(coeffs_out, "model_coeffs/class/pf_lasso.csv")

# ------------------------------------------------------------------------------

ex_stud = xtrain_mat[1,]         # extract an example of a student
ex_stud[1:length(ex_stud)] = 0   # reset the entries
ex_stud[c(2, 10, 12, 19, 22, 23, 25, 18)] = 1
ex_stud['must'] = 13
ex_stud['alg']  = 5
ex_stud['conc'] = 3

data.frame(ex_stud, coeffs_lasso[2:34])

x_copy = xtest_mat
x_copy[1,] = ex_stud

lasso_pred = predict(pf_lasso, s = lambda_star, newx = x_copy,
                     type = 'response')  ## 0.8706258 probability of success
logodds = predict(pf_lasso, s = lambda_star, newx = x_copy,
        type = 'link')[1]

exp(logodds) / (1 + exp(logodds))

lasso_pred[1] # 82.13804





# end of comm_model_class.R
