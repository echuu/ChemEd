
# regression models

# Modeling Included:
#    (1a) : (full model) course average ~ new must + .
#    (1b) : (null model) course average ~  1
#    (2)  : (LASSO)      course average ~ new must + . (+ var selection) 


library(glmnet)

source("misc.R")

x0 = read.csv("must_data.csv") # read in data file

MUST_q = paste(rep("MQ", 20), 1:20, sep = "")  # 20 MUST questions


# ensure that each question is encoded as a factor
indicator_vars = c("school", "ethnic",
                   "gender", "ver", "pass", MUST_q)
x0[,indicator_vars] = lapply(x0[,indicator_vars], as.factor)


# generate training/testing set using 2/3, 1/3 split
train_test = generateTrainTest(x0, seed = 30)
x_train = train_test[[1]]  
x_test  = train_test[[2]]

# questions not included in the old MUST exam
new_qs = c("MQ6", "MQ16", "MQ17", "MQ18") 
cats   = c("mult", "div", "frac", "log_exp", "symb") # category names


# (1a) course average ~ new must + . (718 x 13 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 13
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])
summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course)

# (1b) null model
null_model = lm(course ~ 1,  x_train[,!(names(x0) %in% vars_omit)])
summary(null_model)
getMSE(null_model, x_test, x_test$course) # 182.7921

anova(null_model, course_must20)


## (2) LASSO Regression Model

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
xtrain_mat = model.matrix( ~ . , xtrain_lasso)[,-1] # 682 x 32
xtest_mat  = model.matrix( ~ . , xtest_lasso)[,-1]  # 338 x 32


set.seed(1)
course_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1)
cv_course_lasso = cv.glmnet(x = xtrain_mat, y = y_train, alpha = 1) 
(lambda_star0 = cv_course_lasso$lambda.min)    # optimal lambda: 0.296566

# lasso model coefficients
coeffs_l0 = predict(course_lasso, type = 'coefficients', s = lambda_star0) 
lasso_pred0  = predict(course_lasso, s = lambda_star0, newx = xtest_mat)
mean((lasso_pred0 - y_test)^2) # MSE: 111.4254









