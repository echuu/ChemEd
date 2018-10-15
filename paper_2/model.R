
# model.R

setwd('/home/eric/Dropbox/chem_ed')

############################     variables    ##################################
#-------------------------------------------------------------------------------

# school          :  TX universities (ACUP, TAMSA, TAMU, TSU, UNTB, UNTW, UTXW)
# letter          :  letter grade in chem1 (A, B, C, D, F)
# final_exam      :  percentage grade on final exam
# course          :  course average percentage; our primary regression response
# class           :  class standing (FR, SO, JR, SR)
# gender          :  indicator for male, if not reported, then -1
# ethnic          :  ethnicity (Asian, Black, Hisp, Mixed, White, Other)
# tx_birth        :  indicator for born in Texas
# tx_hs           :  indiator for attended high school in Texas
# grand           :  indicator for grandparents college education (N, Y, Other)
# parent          :  indicator for parents college education (N, Y, Other)
# emp_on          :  indicator for employment on campus (0, 1)
# emp_off         :  indicator for employment off campus (0, 1)
# hrs             :  hours worked a week (0, 1-10, 11-19, 20-29, 30-39, 40+)
# version         :  MUST version (78, 87)
# MQ1-MQ20        :  MUST questions
# must            :  MUST score: sum of MQ1-MQ20 (0, 20)
# Q1A-Q6A         :  algorithmic common questions (0, 1)
# Q1C-Q6C         :  conceptual common questions (0, 1)
# comm            :  sum of all 12 common questions (0, 12)
# pass            :  indicator variable for course_avg >= 70
# alg             :  sum of 6 algorithmic questions, Q1A-Q6A (0, 6)
# conc            :  sum of 6 conceptual questions, Q1C-Q6C (0, 6)
# gp_edu          :  grandparents/parents education code (N/A, NN, NY, YN, YY)


## variables (some temporarily) excluded due to ambiguous/poor encoding

# curr_math       :  description of current math level, poorly encoded; excluded
# chem1           :  ambiguous encoding; exlcuded for now
# chem2           :  indicator for chem 2, poorly encoded; excluded for now
# major           :  major; needs to be re-encoded; excluded for now
# zip             :  first two numbers of zip code; poorly encoded; excluded
# hs_chem         :  description for high school chem taken; poorly encoded
# math_comp       :  description for math level; poorly encoded, excluded


#### ---------------------------------------------------------------------------


x = read.csv("part2/data/x_model.csv")



## repeat the models done for paper 1





## include the new data from the 'common' questions




# end of figures.R
