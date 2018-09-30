
# model.R


# in order for this script to be run, must:
#    1. first MUST_script.R for some variables and data structures created there
#    2. run train.R, which contains the generateTrainTest() function
#    3. run evaluateClassification.R for classAccuracy() and tuneThreshold()

library(MASS)
library(dplyr)
library(glmnet)

# response: 
# grade    : course average (0 - 100)
# pass     : pass (1), fail (0) - depending on if course average >= 69.5

x = read.csv("data/model_x.csv") # 1073 x 14 (1 response, 13 predictors)

pass = as.numeric(x$course_avg >= 69.5)
x = cbind(pass, x)


# create train, test sets
train_test = generateTrainTest(x, seed = 0) # this function is in train.R
x_train = train_test[[1]]  # 718 x 15
x_test = train_test[[2]]   # 355 x 15

########################    linear regression full    ##########################

# x_train %>% group_by(school) %>% summarise(mean(course_avg))

## basic regression model with all predictors included using TRAINING data
m0 = lm(course_avg ~ . - pass, data = x_train)
summary(m0)

preds = predict(m0, newdata = x_test) # use model on TEST data
mean((x_test$course_avg - preds)^2)   # TEST MSE: 110.7344

#########################    LASSO regression full    ##########################

set.seed(1) # used for cross validation when finding optimal lambda
# transform factor variables into dummies
xtrain_mat = model.matrix( ~ . - 1, x_train[ , -c(1:2)])
xtest_mat = model.matrix( ~ . - 1, x_test[ , -c(1:2)])
m_lasso = glmnet(x = xtrain_mat, y = x_train[,2], alpha = 1)   # fit lasso model
cv_lasso = cv.glmnet(x = xtrain_mat, y = x_train[,2], alpha = 1) 
lambda_star = cv_lasso$lambda.min # optimal lambda: 0.349
lasso_pred = predict(m_lasso, s = lambda_star, newx = xtest_mat)
mean((lasso_pred - x_test[,2])^2) # MSE: 108.956


# examine coefficients:
predict(m_lasso, type = 'coefficients', s = lambda_star)

######################    linear regression subset    ##########################
# exclude: emp_on, emp_off, tx_hs, version

m2 = lm(course_avg ~ . - pass - emp_on - emp_off - tx_hs - version, x_train)
summary(m2)
preds = predict(m2, newdata = x_test) # use model on TEST data
mean((x_test$course_avg - preds)^2)   # TEST MSE: 110.2807

# including the features as done in m0 does not result in significant difference
# note: m2 (features chosen via lasso) is nested in m0 (full model)
anova(m2, m0)  # p-value = 0.8577 -> not significant 


########################    logistic regression full    ########################

m3 = glm(as.factor(pass) ~ . - course_avg, data = x_train, family = 'binomial')
summary(m3)

 
# tune threshold
train_probs = predict(m3, type = 'response')

## we tune the threshold for 'balanced accuracy'
threshold = tuneThreshold(train_probs, x_train$pass, 0.7, DEBUG = FALSE)
t_star = threshold$t_star

train_preds = (train_probs > t_star) + 0
# TRAINING results:
    # balanced: 0.7385831
    # overall : 0.816156
classAccuracy(train_preds, x_train$pass)


# test accuracy
test_preds = (predict(m3, newdata = x_test, type = 'response') > t_star) + 0
# TEST results:
    # balanced: 0.6819048
    # overall : 0.7830986
classAccuracy(test_preds, x_test$pass)

# table(test_preds, x_test$pass)

#####################    logistic regression w/ lasso     ######################

set.seed(1)
# transform factor variables into dummies
log_lasso = glmnet(x = xtrain_mat, y = as.factor(x_train[,1]), 
                   alpha = 1, family = 'binomial')     # fit lasso model
cv_log_lasso = cv.glmnet(x = xtrain_mat, y = as.factor(x_train[,1]), 
                         family = 'binomial', alpha = 1) 
lambda_star = cv_log_lasso$lambda.min                  # optimal lambda: 0.349

predict(log_lasso, type = 'coefficients', s = lambda_star)


# tune threshold
train_probs = predict(log_lasso, s = lambda_star, newx = xtrain_mat, 
                      type = 'response')
## we tune the threshold for 'balanced accuracy'
lasso_threshold = tuneThreshold(train_probs, x_train$pass, 0.7, DEBUG = FALSE)
t_star_lasso = lasso_threshold$t_star
train_preds = (train_probs > t_star_lasso) + 0
# TRAINING results:
    # balanced: 0.7276342
    # overall : 0.8119777
classAccuracy(train_preds, x_train$pass) 

# use model on test data
lasso_pred = predict(log_lasso, s = lambda_star, newx = xtest_mat,
                     type = 'response')
lasso_response = (lasso_pred > t_star_lasso) + 0
# TEST results:
    # balanced: 0.6752381
    # overall : 0.7802817
classAccuracy(lasso_response, x_test$pass)


# logistic model after using lasso for variable selection
m_final = glm(as.factor(pass) ~ . - course_avg - emp_on - emp_off - tx_birth - 
           tx_hs - version, data = x_train, family = 'binomial')
summary(m_final)


# tune threshold
train_probs = predict(m_final, type = 'response')

## we tune the threshold for 'balanced accuracy'
threshold = tuneThreshold(train_probs, x_train$pass, 0.7, DEBUG = FALSE)
t_star = threshold$t_star

train_preds = (train_probs > t_star) + 0
# TRAINING results:
# balanced: 0.7385831
# overall : 0.816156
classAccuracy(train_preds, x_train$pass)


# test accuracy
test_preds = (predict(m3, newdata = x_test, type = 'response') > t_star) + 0
# TEST results:
# balanced: 0.6819048
# overall : 0.7830986
classAccuracy(test_preds, x_test$pass)







