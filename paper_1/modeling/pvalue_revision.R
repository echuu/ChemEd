
library(glmnet)

# setwd("~/ChemEd/paper_1/data_cleaning")            # linux machine path
setwd("C:/Users/chuu/ChemEd/paper_1/modeling")  # windows machine path

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

# questions not included in the old MUST exam
new_qs = c("MQ6", "MQ16", "MQ17", "MQ18") 
cats   = c("mult", "div", "frac", "log_exp", "symb") # category names

# (1b) course_new: course average ~ new must + . (718 x 13 training matrix)
vars_omit = c(MUST_q, cats, "pass", "old_must")
dim(x_train[,!(names(x0) %in% vars_omit)]) # 718 x 13
course_must20 = lm(course ~ ., x_train[,!(names(x0) %in% vars_omit)])

# for revision: use QuantPsyc package for lm.beta() function to generate 
# standardized regression coefficients
library(QuantPsyc)
xtrain_full = x_train[,!(names(x0) %in% vars_omit)]
data.frame(lm.beta(course_must20))


summary(course_must20)
must20_coeffs = summary(course_must20)$coefficients
getMSE(course_must20, x_test, x_test$course) # 110.3085


## compare full models with nested models - each nested model has all but one
## of the variables of omitted

# omit school vs. full
no_school = xtrain_full[,!(names(xtrain_full) %in% c("school"))]
no_school_fit = lm(course ~ ., no_school)
anova(no_school_fit, course_must20) # p = 3.806e-06 ***

# omit class vs. full
no_class = xtrain_full[,!(names(xtrain_full) %in% c("class"))]
no_class_fit = lm(course ~ ., no_class)
anova(no_class_fit, course_must20) # p = 0.1486

# omit gender vs. full
no_gender = xtrain_full[,!(names(xtrain_full) %in% c("gender"))]
no_gender_fit = lm(course ~ ., no_gender)
anova(no_gender_fit, course_must20) # p = 0.3246

# omit ethnicity vs. full
no_ethnic = xtrain_full[,!(names(xtrain_full) %in% c("ethnic"))]
no_ethnic_fit = lm(course ~ ., no_ethnic)
anova(no_ethnic_fit, course_must20) # p = 0.006356

# omit parents vs. full
no_parent = xtrain_full[,!(names(xtrain_full) %in% c("parent"))]
no_parent_fit = lm(course ~ ., no_parent)
anova(no_parent_fit, course_must20) # p = 0.08071


# omit grandparents vs. full
no_grand = xtrain_full[,!(names(xtrain_full) %in% c("grand"))]
no_grand_fit = lm(course ~ ., no_grand)
anova(no_grand_fit, course_must20) # p = 0.4162


# omit emp_on vs. full
no_emp_on = xtrain_full[,!(names(xtrain_full) %in% c("emp_on"))]
no_emp_on_fit = lm(course ~ ., no_emp_on)
anova(no_emp_on_fit, course_must20) # p = 0.4834


# omit emp_off vs. full
no_emp_off = xtrain_full[,!(names(xtrain_full) %in% c("emp_off"))]
no_emp_off_fit = lm(course ~ ., no_emp_off)
anova(no_emp_off_fit, course_must20) # p = 0.469

# omit hrs vs. full
no_hrs = xtrain_full[,!(names(xtrain_full) %in% c("hrs"))]
no_hrs_fit = lm(course ~ ., no_hrs)
anova(no_hrs_fit, course_must20) # p = 0.0003703


# omit ver vs. full
no_ver = xtrain_full[,!(names(xtrain_full) %in% c("ver"))]
no_ver_fit = lm(course ~ ., no_ver)
anova(no_ver_fit, course_must20) # p = 0.8115


# omit must vs. full
no_must = xtrain_full[,!(names(xtrain_full) %in% c("must"))]
no_must_fit = lm(course ~ ., no_must)
anova(no_must_fit, course_must20) # p < 2.2e-16 










