## split the data into train/test using a 2/3 : 1/3 split
## take 2/3 from EACH school so that we use data from all schools

library(dplyr)

# returns train/test sets as a list
generateTrainTest = function(x, train_split = 2 / 3, seed = 1) {
  ## train_split is the proportion of data to allocate to training set
  set.seed(seed)
  ## id_inst_nms: school id's (7 x 1)
  ## id_inst    : all schools (1073 x 1)
  id_inst_nms = c("ACUP", "TAMSA", "TAMU", "TSU", "UNTB", "UNTW", "UTXW")
  N_INST    = length(id_inst_nms)           # number of schools
  inst_freq = data.frame(table(id_inst))    # freq of occurence of each school

  # sample train_split * freq of j-th school
  train = c()
  for (j in 1:N_INST) {
    n_j = inst_freq[j, 2]                   # num. of occurences of j-th school
    train_ind = rep(FALSE, n_j)                
    j_train = ceiling(n_j * train_split)
    train_j = sample(1:n_j, j_train)
    train_ind[train_j] = TRUE               # mark training set with TRUE
    train = c(train, train_ind)             # append to set of train indices
  }
  
  x_train = x[train,]
  x_test = x[!train,]
  
  return(list(x_train, x_test))
}

# check that the number of training data points are correct for each school
# training set for each school should be of size (depends on train_split):
# ACUP | TAMSA | TAMU | TSU | UNTB | UNTW | UTXW |
#  72  |  26   |  317 | 40  |  91  |  64  |  108 |----> training set size: 718

# following 3 lines are for debug -- run after for loop in function to verify
# that the frequency of each school in the training set matches above #'s
# ceiling(inst_freq[, 2] * train_split)
# check_train = data.frame(train = train, id = id_inst)
# table(check_train$id, check_train$train) # TRUE column match previous output




