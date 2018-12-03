
# misc.R
# contains useful functions that are commonly used when doing modeling
# some functions are specific to the Chemistry Education project

# list of functions:
#
#    generateTrainTest()    :  returns train/test sets as a list
#    getMSE()               :  mse of predictions using input model
#    classAccuracy()        :  sensitivity, specificity, accuracy of input model
#    tuneThreshold()        :  optimal threshold, balanced accuracy, # iters
#    getClassResults()      :  list of classification accuracy results
#
#-------------------------------------------------------------------------------

library(dplyr)

##### --------------- Chemistry Education Project Specific ---------------- ####

# generateTrainTest() ----------------------------------------------------------
# input:    
#           x           : original dataset
#           train_split : the proportion of the original dataset to put in train
# output:   train-test  : list containing train, test sets
generateTrainTest = function(x, train_split = 2 / 3, seed = 1) {
  ## train_split is the proportion of data to allocate to training set
  set.seed(seed)
  ## id_inst_nms: school id's (7 x 1)
  ## id_inst    : all schools (1073 x 1)
  print(dim(x))
  schools = x$school
  id_inst_nms = as.character(unique(schools))
  N_INST      = length(id_inst_nms)         # number of unique schools
  inst_freq   = data.frame(table(schools))  # freq of occurence of each school
  
  # sample train_split * freq of j-th school
  # since each school is stored together in the same chunk, sample 2/3 from 
  # each of the school's chunks and append the training indices to a running
  # total of indices that we eventually use to subset out the training set
  train = c()
  for (j in 1:N_INST) {
    n_j = inst_freq[j, 2]                   # num. of occurences of j-th school
    train_ind = rep(FALSE, n_j)                
    j_train = ceiling(n_j * train_split)   
    train_j = sample(1:n_j, j_train)        # all schools are stored together
    train_ind[train_j] = TRUE               # mark training set with TRUE
    train = c(train, train_ind)             # append to set of train indices
  }
  
  x_train = x[train,]
  x_test = x[!train,]
  
  
  print(dim(x_train))
  print(dim(x_test))
  train_test = list(x_train, x_test)
  
  return(train_test)
} # end of generateTrainTest() function


# getMSE() --------------------------------------------------------------
# input:    
#           m           : fitted model
#           x_test      : features for the test set
#           y_test      : true response values
# output:   mse         : mean squared error of the predictions using m
getMSE = function(m, x_test, y_test) {
  
  preds = predict(m, newdata = x_test)
  mse = mean((y_test - preds)^2)
    
  return(mse)
} # end of getMSE() function


# classAccuracy() --------------------------------------------------------------
# input:    
#           preds       : model predictions (binary values, not probabilities)
#           true_class  : true classes (binary values)
# output:   out         : dataframe reporting sensitivity, specificity, accuracy
classAccuracy = function(preds, true_class) {
  # false positive: predict fail, no fail
  # false negative: predict pass, no pass
  # sensitivity: rate that we correctly detect failure
  # specificity: rate that we correctly detect pass
  
  conf_mat = table(true_class, preds)
  sens = conf_mat[1,1] / sum(conf_mat[1,])       # sensitivity
  spec = conf_mat[2,2] / sum(conf_mat[2,])       # specificity
  bal_acc = (sens + spec) / 2
  acc = sum(diag(conf_mat)) / sum(conf_mat)      # overall class accuracy
  
  out = data.frame(sens, spec, bal_acc, acc)
  
  return(out)
} # end of classAccuracy() function


# tuneThreshold() --------------------------------------------------------------
# input:    
#           pred_prob   : output of predict(), vector of probabilities
#                         note these are not binary value, not yet thresholded
#           true_class  : true classes (binary values)
#           targetAcc   : target BALANCED accuracy
#           minSpec     : min. specificity (rate that we correctly detect pass)
# output:   out         : dataframe containing convergence results: 
#                             (1) optimal threshold 
#                             (2) balanced accuracy
#                             (3) number of iterations to converence
tuneThreshold = function(pred_prob, true_class, targetAcc, maxIter = 50, 
                         minSpec = 0.6, DEBUG = TRUE) {
  
  # lower threshold  -> higher spec, lower sens
  # higher threshold -> lower spec, higher sens
  # define balAcc = average of sensitivity and specificity
  
  t    = 0.5      # initial threshold
  acc  = 0.0      # guarantee 1 iteration
  spec = 0.9      # initial specificity -- guarantee 1 iteration
  
  iter = 0   
  preds_thresh = (pred_prob > t) + 0                  # obtain predictions
  balAcc = classAccuracy(preds_thresh, true_class)$bal_acc
  
  # iterate while (1), (2), (3) are satisfied
  # (1): balanced acc is increasing OR balanced acc < target acc
  # (2): specificity > minimum specificity
  # (3): current iteration < maximum iterations
  while (((balAcc < targetAcc) || (balAcc > acc)) && 
         (spec > minSpec) && (iter < maxIter)) {
    iter = iter + 1
    acc  = balAcc       # update accuracy
    t    = t + 0.01     # increment the threshold
    
    preds_thresh = (pred_prob > t) + 0                  # obtain predictions
    accuracy = classAccuracy(preds_thresh, true_class)  # eval general accuracy 
    balAcc = accuracy$bal_acc                           # update balAcc
    spec = accuracy$spec                                # update spec
    
    if (DEBUG) {
      print(paste("iteration: ", iter))
      print(paste("specificity: ", spec))
      print(paste("balanced acc: ", balAcc))
      print("")
    }
  } # end while()
  
  # print("inside tune threshold")
  # print(length(t))
  out = data.frame(t_star = t, balAcc, iters = iter)
  
  return(out)
  
} # end of tuneThreshold() function


# getClassResults() --------------------------------------------------------------
# input:    
#           m           : classification model
#           x_train     : training set (contains response as a column)
#           x_test      : test set (contains response as a column)
#           minBalAcc   : min. BALANCED accuracy
# output:   out         : list containing classification results: 
#                             (1) balanced train accuracy 
#                             (2) overall train accuracy
#                             (3) balanced test accuracy 
#                             (4) overall test accuracy
#                             (5) confusion matrix for predictions vs. true val
getClassResults = function(m, x_train, x_test, minBalAcc = 0.7, lambda = 0,
                           xtrain_mat = NULL, xtest_mat = NULL) {
  
  ## generate training predictions: vector of probabilities
  if (lambda == 0) {
    train_probs = predict(m, type = 'response')
  } else { # generate predictions for LASSO model
    train_probs = predict(m, type = 'response', newx = xtrain_mat, s = lambda)
  }
  
  
  ## we tune the threshold for 'balanced accuracy'
  threshold = tuneThreshold(pred_prob = train_probs, true_class = x_train$pass, 
                            targetAcc = minBalAcc, DEBUG = FALSE)
  t_star = threshold$t_star   # optimal threshold to make decisions
  
  # print(length(threshold$t_star))
  
  ## obtain pass/fail predictions by thresholding the probabilities w/ t_star
  train_preds = (train_probs > t_star) + 0
  
  ## training classificaiton accuracy
  train_accuracy = classAccuracy(train_preds, x_train$pass)
  train_balance = train_accuracy$bal_acc  # training balanced accuracy
  train_overall = train_accuracy$acc      # training overall accuracy
  
  ## obtain test accuracy using 'm' and thresholding with t_star
  if (lambda == 0) {
    test_preds = (predict(m, newdata = x_test, type = 'response') > t_star) + 0
  } else {
    test_probs = predict(m, type = 'response', newx = xtest_mat, s = lambda)
    test_preds = (test_probs > t_star) + 0
  }
  
  
  test_accuracy = classAccuracy(test_preds, x_test$pass)
  test_balance = test_accuracy$bal_acc
  test_overall = test_accuracy$acc
  
  # conf_mat = test_accuracy$conf_mat
  
  conf_mat = table(test_preds, x_test$pass)
  
  out = list(train_balance = train_balance, train_overall = train_overall, 
             test_balance = test_balance, test_overall = test_overall, 
             conf_mat = conf_mat)
  
  return(out)
  
} # end of getClassResults() function




# end of misc.R file
