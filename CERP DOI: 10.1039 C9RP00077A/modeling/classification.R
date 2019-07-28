

# classifcation.R -- classification models used in Paper 1

# Modeling Included:
#    (1)  : (full model) pass/fail ~ new must + .
#    (2)  : (LASSO)      pass/fail ~ new must + . (+ var selection) 

library(glmnet)

source("misc.R")

x0 = read.csv("must_data.csv") 

# global vars
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")  # 20 MUST questions

# ensure that each question is encoded as a factor
indicator_vars = c("school", "ethnic",
                   "gender", "ver", "pass", MUST_q)
x0[,indicator_vars] = lapply(x0[,indicator_vars], as.factor)

str(x0) # only numeric variables: must, old_must, 5 categories, course average
# ALL ELSE SHOULD BE FACTORS

## generate train/test set: seed = 30
train_test = generateTrainTest(x0, seed = 30)
x_train = train_test[[1]]   
x_test  = train_test[[2]]  


## ----    (1) classification for modeling course average old vs. new   ----- ##

# (1) logistic: pass ~ new must + ...
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
write.csv(as.data.frame(as.matrix(round(coeffs_6b, 4))), "class_coef.csv")


#### ------    (2) LASSO reg for modeling pass/fail vs must       --------- ####

# lasso requires model matrix set up in particular way:
xtrain_lasso = x_train[,!(names(x0) %in% vars_omit)]     # omit irrelevant vars
xtrain_lasso$pass = NULL                                 # omit response

xtest_lasso = x_test[,!(names(x0) %in% vars_omit)]       # omit irrelevant vars
xtest_lasso$pass = NULL                                  # omit response

## check: num of columns for xtrain_lasso, xtest_lasso should be same
ncol(xtrain_lasso) == ncol(xtest_lasso) # should print TRUE

y_train = x_train$pass      # training response (718 x 1)
y_test  = x_test$pass       # test response     (355 x 1)

# create model matrix compatible with glmnet(), one hot encoding for factors
xtrain_mat = model.matrix( ~ . , xtrain_lasso)[,-1] # 682 x 32
xtest_mat  = model.matrix( ~ . , xtest_lasso)[,-1]  # 338 x 32

set.seed(1)
pf_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1, family = 'binomial')
cv_pf_lasso = cv.glmnet(x = xtrain_mat, y = y_train, 
                        alpha = 1, family = 'binomial') 
(lambda_star0 = cv_pf_lasso$lambda.min)    # optimal lambda: 0.004799824

coeffs_lasso = predict(pf_lasso, type = 'coefficients', s = lambda_star0)
write.csv(as.data.frame(as.matrix(round(coeffs_lasso, 4))),
          "class_coef_lasso.csv")

# tune threshold
train_probs = predict(pf_lasso, s = lambda_star0, newx = xtrain_mat, 
                      type = 'response')
lasso_threshold = tuneThreshold(train_probs, x_train$pass, 0.7, DEBUG = FALSE)
t_star_lasso = lasso_threshold$t_star
train_preds = (train_probs > t_star_lasso) + 0
classAccuracy(train_preds, x_train$pass) 


# use model on test data
lasso_pred = predict(pf_lasso, s = lambda_star0, newx = xtest_mat,
                     type = 'response')
lasso_response = (lasso_pred > t_star_lasso) + 0
classAccuracy(lasso_response, x_test$pass)









