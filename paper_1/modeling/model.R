
# modeling_updated.R

# modeling using new, cleaned data ---- 12/12/18

library(glmnet)

# setwd("~/ChemEd/paper_1/data_cleaning")            # linux machine path
setwd("C:/Users/chuu/ChemEd/paper_1/modeling")  # windows machine path

# source("/home/eric/ChemEd/paper_2/misc.R")   # linux machine path to misc.R
source("C:/Users/chuu/ChemEd/paper_2/misc.R")  # windows machine path to misc.R

x0 = read.csv("must.csv")  # must.csv is the final cleaned data (1073 x 40)

# global vars
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")  # 20 MUST questions


# ensure that each question is encoded as a factor
indicator_vars = c("gender", "ver", "pass", MUST_q)
x0[,indicator_vars] = lapply(x0[,indicator_vars], as.factor)

str(x0) # only numeric variables: must, old_must, 5 categories, course average
        # ALL ELSE SHOULD BE FACTORS

## generate train/test set: seed = 30
train_test = generateTrainTest(x0, seed = 30)
x_train = train_test[[1]]   # 718 x 40
x_test  = train_test[[2]]   # 355 x 40


#### ------    regression for modeling course average old vs. new   ------- ####

# questions not included in the old MUST exam
new_qs = c("MQ6", "MQ16", "MQ17", "MQ18") 
cats   = c("mult", "div", "frac", "log_exp", "symb") # category names


# (1) model using the total must score of old vs. new

# (1a) course_old: course average ~ old must + . (718 x 13 training matrix)
vars_omit = c(MUST_q, cats, "pass", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 13
course_must16 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must16)
must16_coeffs = summary(course_must16)$coefficients
getMSE(course_must16, x_test, x_test$course) # 112.4562
# write.csv(must16_coeffs, "model_coeffs/1a_coeffs.csv")

# (1b) course_new: course average ~ new must + . (718 x 13 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 13
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course) # 110.3085
# write.csv(must20_coeffs, "model_coeffs/1b_coeffs.csv")


# (2) model using indicator for each question old vs. new (nested model)

## (2a) course_ind16: course average ~ 16 must questions (718 x 28 training)
# omit the 4 questions included in the new MUST
vars_omit = c(cats, new_qs, "pass", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 28
course_ind16 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_ind16)
course_ind16_coeffs = summary(course_ind16)$coefficients
getMSE(course_ind16, x_test, x_test$course) # 108.3177
# write.csv(course_ind16_coeffs, "model_coeffs/2a_coeffs.csv")


# (2b) course_ind20: course average ~ all 20 must questions (718 x 29 training)
vars_omit = c(cats, "pass", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 32
course_ind20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_ind20)
course_ind20_coeffs = summary(course_ind20)$coefficients
getMSE(course_ind20, x_test, x_test$course) # 106.9001
# write.csv(course_ind20_coeffs, "model_coeffs/2b_coeffs.csv")


# test for significance between the nested models
anova(course_ind16, course_ind20) # p = 0.0226 < 0.05 --> extra q's significant


#### ---   regression for modeling course average using question cats   --- ####

# (3) model using category sums

## course_cat: course average ~ must question categories (718 x 17)
vars_omit = c(MUST_q, "pass", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 17

course_cat = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_cat) # all cats but division significant
course_cat_coeffs = summary(course_cat)$coefficients
getMSE(course_cat, x_test, x_test$course) # 108.7652
# write.csv(course_cat_coeffs, "model_coeffs/3_coeffs.csv")

#### -------    lasso reg for modeling course average vs must       ------- ####

# (4) lasso: course average ~ individual must questions (682 x 33 training)
# see if the new questions are not zero'd out by lasso
vars_omit = c(cats, "pass", "must", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 32

# lasso requires model matrix set up in particular way:
xtrain_lasso = x_train[,!(names(x0) %in% vars_omit)]     # omit irrelevant vars
xtrain_lasso$course = NULL                               # omit response

xtest_lasso = x_test[,!(names(x0) %in% vars_omit)]       # omit irrelevant vars
xtest_lasso$course = NULL                                # omit response

## check: num of columns for xtrain_lasso, xtest_lasso should be same
ncol(xtrain_lasso) == ncol(xtest_lasso) # should print TRUE

y_train = x_train$course   # training response (718 x 1)
y_test  = x_test$course    # test response     (355 x 1)

# create model matrix compatible with glmnet(), one hot encoding for factors
xtrain_mat = model.matrix( ~ . - 1, xtrain_lasso) # 682 x 51
xtest_mat  = model.matrix( ~ . - 1, xtest_lasso)  # 338 x 51

set.seed(1)
course_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_course_lasso = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
(lambda_star0 = cv_course_lasso$lambda.min)    # optimal lambda: 0.2778991

# lasso model coefficients
coeffs_l0 = predict(course_lasso, type = 'coefficients', s = lambda_star0) 
lasso_pred0  = predict(course_lasso, s = lambda_star0, newx = xtest_mat)
mean((lasso_pred0 - y_test)^2) # MSE: 107.9899

write.csv(as.data.frame(as.matrix(coeffs_l0)), "model_coeffs/4_coeffs.csv")

# (5) lasso: course average ~ category sums 
# see which categories are indicative of performance
vars_omit = c(MUST_q, "pass", "must", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 17

# lasso requires model matrix set up in particular way:
xtrain_lasso = x_train[,!(names(x0) %in% vars_omit)]   # omit irrelevant vars
xtrain_lasso$course = NULL                             # omit response

xtest_lasso = x_test[,!(names(x0) %in% vars_omit)]      # omit irrelevant vars
xtest_lasso$course = NULL                               # omit response

## check: num of columns for xtrain_lasso, xtest_lasso should be same
ncol(xtrain_lasso) == ncol(xtest_lasso) # should print TRUE

y_train = x_train$course   # training response (718 x 1)
y_test  = x_test$course    # test response     (355 x 1)

# create model matrix compatible with glmnet(), one hot encoding for factors
xtrain_mat = model.matrix( ~ . - 1, xtrain_lasso) # 718 x 36
xtest_mat  = model.matrix( ~ . - 1, xtest_lasso)  # 355 x 36

set.seed(1)
course_lasso_cats = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_course_lasso_cats = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
(lambda_star1 = cv_course_lasso_cats$lambda.min)    # optimal lambda: 0.2955357

# lasso model coefficients
coeffs_l1 = predict(course_lasso_cats, type = 'coefficients', s = lambda_star1) 
lasso_pred1  = predict(course_lasso_cats, s = lambda_star1, newx = xtest_mat)
mean((lasso_pred1 - y_test)^2) # MSE: 109.4341

# write.csv(as.data.frame(as.matrix(coeffs_l1)), "model_coeffs/5_coeffs.csv")


#### ----    classification for modeling course average old vs. new   ----- ####

# (6) model using the total must score of old vs. new --------------------------

# (6a) logistic: pass ~ old must + ...
vars_omit = c(MUST_q, cats, "course", "must")     # old
dim(x_train[,!(names(x0) %in% vars_omit)])        # 718 x 13 -- match w/ (1a)
names(x_train[,!(names(x0) %in% vars_omit)])      # 718 x 13

pf0 = glm(as.factor(pass) ~ ., family = 'binomial', 
          x_train[,!(names(x0) %in% vars_omit)])
summary(pf0)
# obtain train/test balanced/overall accuracy, confusion matrix
pf0_results = getClassResults(m = pf0, x_train = x_train, x_test = x_test)
pf0_results$test_balance   # test balanced accuracy :  0.6675231
pf0_results$test_overall   # test overall accuracy  :  0.8028169
pf0_results$conf_mat       # true values of pass/fail are given by column sums

coeffs_6a = summary(pf0)$coefficients
# write.csv(as.data.frame(as.matrix(coeffs_6a)), "model_coeffs/6a_coeffs.csv")


# (6b) logistic: pass ~ new must + ...
vars_omit = c(MUST_q, cats, "course", "old_must") # new
dim(x_train[,!(names(x0) %in% vars_omit)])        # 718 x 13 -- match w/ (1b)
names(x_train[,!(names(x0) %in% vars_omit)])      # 718 x 13

pf1 = glm(as.factor(pass) ~ ., family = 'binomial', 
          x_train[,!(names(x0) %in% vars_omit)])
summary(pf1)
# obtain train/test balanced/overall accuracy, confusion matrix
pf1_results = getClassResults(m = pf1, x_train = x_train, x_test = x_test)
pf1_results$test_balance   # test balanced accuracy :  0.6733159
pf1_results$test_overall   # test overall accuracy  :  0.7971831
pf1_results$conf_mat       # true values of pass/fail are given by column sums

coeffs_6b = summary(pf1)$coefficients
# write.csv(as.data.frame(as.matrix(coeffs_6b)), "model_coeffs/6b_coeffs.csv")

# (7) pass ~ new/old must individual questions (omit category sums) ------------

# (7a) class_indic16: pass ~ 16 old MUST questions + ...
vars_omit = c(cats, new_qs, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 28 -- match with (2a)
names(x_train[,!(names(x0) %in% vars_omit)])

pf2 = glm(as.factor(pass) ~ ., family = 'binomial', 
          x_train[,!(names(x0) %in% vars_omit)])
summary(pf2)
# obtain train/test balanced/overall accuracy, confusion matrix
pf2_results = getClassResults(m = pf2, x_train = x_train, x_test = x_test)
pf2_results$test_balance   # test balanced accuracy :  0.6941979
pf2_results$test_overall   # test overall accuracy  :  0.8225352
pf2_results$conf_mat       # true values of pass/fail are given by column sums

coeffs_7a = summary(pf2)$coefficients
# write.csv(coeffs_7a, "model_coeffs/7a_coeffs.csv")


# (7b) class_indic20: pass ~ 20 old MUST questions + ...
vars_omit = c(cats, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 32 -- match with (2b)
names(x_train[,!(names(x0) %in% vars_omit)])

pf3 = glm(as.factor(pass) ~ ., family = 'binomial', 
          x_train[,!(names(x0) %in% vars_omit)])
summary(pf3)
# obtain train/test balanced/overall accuracy, confusion matrix
pf3_results = getClassResults(m = pf3, x_train = x_train, x_test = x_test)
pf3_results$test_balance   # test balanced accuracy :  0.6711202
pf3_results$test_overall   # test overall accuracy  :  0.8084507
pf3_results$conf_mat       # true values of pass/fail are given by column sums

coeffs_7b = summary(pf3)$coefficients
# write.csv(coeffs_7b, "model_coeffs/7b_coeffs.csv")

# (8) class_cat: pass ~ category sums
# class_cat
vars_omit = c(MUST_q, "course", "old_must", "must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 17 -- match with (3)

pf4 = glm(as.factor(pass) ~ ., family = 'binomial', 
          x_train[,!(names(x0) %in% vars_omit)])
summary(pf4)
# obtain train/test balanced/overall accuracy, confusion matrix
pf4_results = getClassResults(m = pf4, x_train = x_train, x_test = x_test)
pf4_results$test_balance   # test balanced accuracy :  0.676913
pf4_results$test_overall   # test overall accuracy  :  0.8028169
pf4_results$conf_mat       # true values of pass/fail are given by column sums

coeffs_8 = summary(pf4)$coefficients
# write.csv(coeffs_8, "model_coeffs/8_coeffs.csv")








