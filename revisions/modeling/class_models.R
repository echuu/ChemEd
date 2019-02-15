

#### ----    classification for modeling course average old vs. new   ----- ####


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
write.csv(as.data.frame(as.matrix(coeffs_6b)), "class_coef.csv")


#### --------    lasso reg for modeling pass/fail vs must       ----------- ####

# lasso requires model matrix set up in particular way:
xtrain_lasso = x_train[,!(names(x0) %in% vars_omit)]     # omit irrelevant vars
xtrain_lasso$pass = NULL                               # omit response

xtest_lasso = x_test[,!(names(x0) %in% vars_omit)]       # omit irrelevant vars
xtest_lasso$pass = NULL                                  # omit response

## check: num of columns for xtrain_lasso, xtest_lasso should be same
ncol(xtrain_lasso) == ncol(xtest_lasso) # should print TRUE

y_train = x_train$pass      # training response (718 x 1)
y_test  = x_test$pass       # test response     (355 x 1)

# create model matrix compatible with glmnet(), one hot encoding for factors
xtrain_mat = model.matrix( ~ . - 1, xtrain_lasso) # 682 x 32
xtest_mat  = model.matrix( ~ . - 1, xtest_lasso)  # 338 x 32

set.seed(1)
pf_lasso = glmnet(x = xtrain_mat, y = y_train, alpha = 1, family = 'binomial')
cv_pf_lasso = cv.glmnet(x = xtrain_mat, y = y_train, 
                        alpha = 1, family = 'binomial') 
(lambda_star0 = cv_pf_lasso$lambda.min)    # optimal lambda: 0.004799824

coeffs_lasso = predict(pf_lasso, type = 'coefficients', s = lambda_star0)
write.csv(as.data.frame(as.matrix(coeffs_lasso)), "class_coef_lasso.csv")

# tune threshold
train_probs = predict(pf_lasso, s = lambda_star0, newx = xtrain_mat, 
                      type = 'response')
lasso_threshold = tuneThreshold(train_probs, x_train$pass, 0.7, DEBUG = FALSE)
t_star_lasso = lasso_threshold$t_star
train_preds = (train_probs > t_star_lasso) + 0
# TRAINING results:
# balanced: 0.7041144
# overall : 0.8008357
classAccuracy(train_preds, x_train$pass) 

# use model on test data
lasso_pred = predict(pf_lasso, s = lambda_star0, newx = xtest_mat,
                     type = 'response')
lasso_response = (lasso_pred > t_star_lasso) + 0
# TEST results:
# balanced: 0.6960899
# overall : 0.7887324
classAccuracy(lasso_response, x_test$pass)

# test on example student ------------------------------------------------------
ex_stud = xtrain_mat[1,]
ex_stud[c(4, 9, 10, 16, 17, 21, 23, 24, 30, 31)] = 1
ex_stud[32] = 10


x_copy = xtest_mat
x_copy[1,] = ex_stud
lasso_pred = predict(pf_lasso, s = lambda_star0, newx = x_copy,
                     type = 'response')
lasso_pred[1] # 0.2417805

