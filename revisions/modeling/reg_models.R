

# modeling_updated.R

# modeling using new, cleaned data ---- 12/12/18

library(glmnet)

# setwd("~/ChemEd/revisions/data_cleaning")            # linux machine path
setwd("C:/Users/chuu/ChemEd/revisions/modeling")  # windows machine path

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


# (1) course_new: course average ~ new must + . (718 x 13 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 13
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course) # 110.3085
write.csv(must20_coeffs, "reg_coef.csv")


# null model
null_model = lm(course ~ 1,  x_train[,!(names(x0) %in% vars_omit)])
summary(null_model)
getMSE(null_model, x_test, x_test$course) # 182.7921

anova(null_model, course_must20)

#### -------    lasso reg for modeling course average vs must       ------- ####


# (4) lasso: course average ~ individual must questions (682 x 33 training)
# see if the new questions are not zero'd out by lasso
# vars_omit = c(cats, MUST_q, "pass", "old_must")
# dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 32

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
xtrain_mat = model.matrix( ~ . - 1, xtrain_lasso) # 682 x 32
xtest_mat  = model.matrix( ~ . - 1, xtest_lasso)  # 338 x 32

set.seed(1)
course_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_course_lasso = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
(lambda_star0 = cv_course_lasso$lambda.min)    # optimal lambda: 0.296566

# lasso model coefficients
coeffs_l0 = predict(course_lasso, type = 'coefficients', s = lambda_star0) 
lasso_pred0  = predict(course_lasso, s = lambda_star0, newx = xtest_mat)
mean((lasso_pred0 - y_test)^2) # MSE: 110.7456

write.csv(as.data.frame(as.matrix(coeffs_l0)), "reg_coef_lasso.csv")




