

# report the sensitivity, specificity, accuracy
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
  return(data.frame(sens, spec, bal_acc, acc))
} # end of classAccuracy() function



# lower threshold  -> higher spec, lower sens
# higher threshold -> lower spec, higher sens
# pred_prob: output of predict(), vector of probabilities
tuneThreshold = function(pred_prob, true_class, targetAcc, maxIter = 50, 
                         minSpec = 0.6, DEBUG = TRUE) {
  
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
  
  return(data.frame(t_star = t, balAcc, iters = iter))
  
} # end of tuneThreshold() function


