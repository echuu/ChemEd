
# MUST_modeling.R

library(MASS)
library(dplyr)
library(glmnet)

# if running this file locally, set the working directory to the folder that 
# contains the pertinent dataset, 'x_model.csv'
setwd('/home/eric/Dropbox/chem_ed/part2/data')

# misc.R contains some useful modeling functions that are specific to 
# this problem
source("/home/eric/ChemEd/paper_2/misc.R")

# Use the dataset that was curated for second paper to perform MUST modeling so
# that future modeling that involves both MUST and Common Questions (CQs)
# use the same dataset, allowing some fluidity between the analyses

## load in dataset
x = read.csv("x_model.csv") # 1020 x 60

## omit the variables irrelevant to the model:
# 10 variables omitted in the following list
omit_vars = c("grand", "parent", "hs_chem", "math_comp", "curr_math", 
                "chem1", "chem2", "letter", "final_exam", "zip")

# since we're considering MUST performance for this paper, we add 
# common question related columns to the omit list
    # transform indicator variables to factors:
MUST_q = paste(rep("MQ", 20), 1:20, sep = "")          # 20 MUST questions
COMM_q = paste(rep(paste("Q", 1:6, sep = ""), 2),      # 12 Common questions
               c(rep("A", 6), rep("C", 6)), sep = "")
# comm = CQ score, alg = algorithmic score, conc = conceptual score
cq_split = c("comm", "alg", "conc") 

# 10 original vars + 12 CQ + 3 CQ-related = 25 columns to omit
omit_vars = c(omit_vars, COMM_q, cq_split) # 25 x 1


# create the dataframe used for modeling; note that depending on the task,
# either course or pass will need to be omitted from the feature space
# since course determines values of pass
x0   = x[,-which(names(x) %in% omit_vars)]   # 1020 x 50



