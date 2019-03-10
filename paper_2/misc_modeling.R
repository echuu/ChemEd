
# misc_modeling.R


# incorporate later if still needed --------------------------------------------

# misc. modeling    ------------------------------------------------------------


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


############### ----------- MODELING ALGORITHMIC ----------- ###################

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


################ ----------- MODELING CONCEPTUAL ----------- ####################

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



# ------------------------------------------------------------------------------




# end misc_modeling.R
