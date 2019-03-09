

# ------------------------------------------------------------------------------
################## ----------- CLASSIFICATION --------------- ##################


#### logistic regression for modeling pass/fail --------------------------------

# pass/fail model 0: pass ~ . (omit individual must Q's, omit CQ's)
# use both must + common qs' to predict pass/fail
pf_m0 = glm(as.factor(pass) ~ . - course - comm, family = 'binomial',
            x_train[,!(names(x0) %in% all_questions)])
summary(pf_m0)

# obtain train/test balanced/overall accuracy, confusion matrix
m0_results = getClassResults(m = pf_m0, x_train = x_train, x_test = x_test)
m0_results$test_balance   # test balanced accuracy :  0.6864469
m0_results$test_overall   # test overall accuracy  :  0.8343195
m0_results$conf_mat       # true values of pass/fail are given by column sums


# pass/fail model 1: pass ~ must + ... (omit comm, conc, alg, individual q's)
# use only must to predict pass/fail
pf_m1 = glm(as.factor(pass) ~ . - course - comm - conc - alg, 
            family = 'binomial', x_train[,!(names(x0) %in% all_questions)])
summary(pf_m1)

m1_results = getClassResults(m = pf_m1, x_train = x_train, x_test = x_test)
m1_results$test_balance   # test balanced accuracy  :  0.6622711
m1_results$test_overall   # test overall accuracy   :  0.7573964
m1_results$conf_mat       # true values of pass/fail are given by column sums


# pass/fail model 2: pass ~ conc + alg + ... (omit comm, must, inidividual q's)
# use only alg + conc to predict pass/fail (common questions)
pf_m2 = glm(as.factor(pass) ~ . - course - comm - must, family = 'binomial',
            x_train[,!(names(x0) %in% all_questions)])
summary(pf_m2)

m2_results = getClassResults(m = pf_m2, x_train = x_train, x_test = x_test)
m2_results$test_balance   # test balanced accuracy  :  0.6959707
m2_results$test_overall   # test overall accuracy   :  0.8402367
m2_results$conf_mat       # true values of pass/fail are given by column sums


#### logistic regression with LASSO for modeling pass/fail ---------------------

set.seed(1)
# transform factor variables into dummies
pf_lasso = glmnet(x = xtrain_mat, y = as.factor(x_train$pass), 
                  alpha = 1, family = 'binomial')     # fit lasso model
cv_pf_lasso = cv.glmnet(x = xtrain_mat, y = as.factor(x_train$pass), 
                        family = 'binomial', alpha = 1) 
lambda_star = cv_pf_lasso$lambda.min                  # optimal lambda: 0.349

predict(pf_lasso, type = 'coefficients', s = lambda_star) # coefficients

lasso_results = getClassResults(pf_lasso, x_train, x_test, lambda = lambda_star, 
                                xtrain_mat = xtrain_mat, xtest_mat = xtest_mat)
lasso_results$test_balance   # test balanced accuracy  :  0.7208791
lasso_results$test_overall   # test overall accuracy   :  0.852071
lasso_results$conf_mat       # true values of pass/fail are given by column sums


# ------------------------------------------------------------------------------



# ------------------------------   TO DO    ------------------------------------


##### ------- (1) relationship between common questions and MUST ------- #######


## common questions as a function of MUST and the other explanatory variables

# omit individual must, common questions (previously this was done in-line)
# when creating the model
x_train_sub = x_train[,!(names(x0) %in% all_questions)]

# omit algorithmic, conceptual questions since they sum to give comm
# omit pass, since course average used to determine pass (0/1)
cq_m1 = lm(comm ~ . - alg - conc - pass, x_train_sub)
summary(cq_m1)
cq_m1_coeffs = summary(cq_m0)$coefficients
getMSE(cq_m1, x_test, x_test$comm) # MSE: 3.264922


################################ TO - DO #######################################

## nonlinear model (linear regression with polynomial term for MUST)
## quadratic must term
cq_m2 = lm(comm ~ . - alg - conc - pass - must + poly(must, 2), x_train_sub)
summary(cq_m2)
cq_m2_coeffs = summary(cq_m2)$coefficients
getMSE(cq_m2, x_test, x_test$comm) # MSE: 3.270627

## cubic must term
cq_m3 = lm(comm ~ . - alg - conc - pass - must + poly(must, 3), x_train_sub)
summary(cq_m3) # only linear must term
cq_m3_coeffs = summary(cq_m3)$coefficients
getMSE(cq_m3, x_test, x_test$comm) # MSE: 3.273161

# ------------------------------------------------------------------------------


################ ----------- MODEING ALGORITHMIC ----------- ###################

## algorithmic as a function of MUST + other (linear must term)
# omit comm since alg contributes to comm
# omit pass, since course average used to determine pass (0/1)
alg_m1 = lm(alg ~ . - comm - conc - pass, x_train_sub)
summary(alg_m1)
alg_m1_coeffs = summary(alg_m1)$coefficients
getMSE(alg_m1, x_test, x_test$alg) # MSE: 1.18494

## algorithmic as a function of MUST + other (quadratic must term)
# omit comm since alg contributes to comm
# omit pass, since course average used to determine pass (0/1)
alg_m2 = lm(alg ~ . - comm - conc - pass - must + poly(must, 2), x_train_sub)
summary(alg_m2) ## both linear and quadratic fit significant 
alg_m2_coeffs = summary(alg_m2)$coefficients
getMSE(alg_m2, x_test, x_test$alg) # MSE: 1.190465


################ ----------- MODEING CONCEPTUAL ----------- ####################

## conceptual as a function of MUST + other (linear must term)
# omit comm since conc contributes to comm 
# omit pass, since course average used to determine pass (0/1)
conc_m1 = lm(conc ~ . - comm - alg - pass, x_train_sub)
summary(conc_m1)
conc_m1_coeffs = summary(conc_m1)$coefficients
getMSE(conc_m1, x_test, x_test$conc) # MSE: 1.325434


## conceptual as a function of MUST + other (quadratic must term)
# omit comm since conc contributes to comm
# omit pass, since course average used to determine pass (0/1)
conc_m2 = lm(conc ~ . - comm - alg - pass - must + poly(must, 2), x_train_sub)
summary(conc_m2)
conc_m2_coeffs = summary(conc_m2)$coefficients
getMSE(conc_m2, x_test, x_test$conc) # MSE: 1.324543


#### ----- (2) difference (if any) between conceptual vs algorithmic ----- #####


## think of a way to do hypothesis test between using alg and conc



# ------------------------------------------------------------------------------


# end of comm_model_class.R


