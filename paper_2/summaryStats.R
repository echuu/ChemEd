

# summary statistics for demographics

library(dplyr)

setwd("C:/Users/chuu/ChemEd/paper_2")

x = read.csv("must.csv") # 1020 x 50

# gender
x %>% group_by(gender) %>% count
x %>% group_by(gender) %>% summarise(mean(course), sd(course))
x %>% group_by(gender) %>% summarise(mean(comm), sd(comm))

# t-test 
t.test(x$course[x$gender == 0], x$course[x$gender == 1], 
       alternative = 'two.sided', var.equal = TRUE)

t.test(x$course[x$gender == 0], x$course[x$gender == 1], 
       alternative = 'two.sided', var.equal = FALSE) # 0.846

t.test(x$comm[x$gender == 0], x$comm[x$gender == 1], 
       alternative = 'two.sided', var.equal = TRUE)

t.test(x$comm[x$gender == 0], x$comm[x$gender == 1], 
       alternative = 'two.sided', var.equal = FALSE) # 0.2851



# ethnic 
x %>% group_by(ethnic) %>% count
x %>% group_by(ethnic) %>% summarise(mean(course), sd(course), 
                                     mean(comm), sd (comm))

# anova
aov_ethnic_comm = aov(comm ~ ethnic, data = x)
aov_ethnic_course = aov(course ~ ethnic, data = x)

summary(aov_ethnic_comm)     # p < 2e-16
summary(aov_ethnic_course)   # p = 4.46e-11

## grand/parent education
x_sub = x %>% dplyr::select(grand, parent, course, comm) %>% 
    filter(grand != "DK", parent != "DK")
x_sub$grand = droplevels(x_sub$grand)
x_sub$parent = droplevels(x_sub$parent)
table(x_sub$grand, x_sub$parent)
x_sub = x_sub %>% mutate(gp = paste(grand, parent, sep = ''))

x_sub %>% group_by(gp) %>% count
x_sub %>% group_by(gp) %>% summarise(mean(course), sd(course), 
                                     mean(comm), sd (comm))
# anova
aov_gp_comm = aov(comm ~ gp, data = x_sub)
aov_gp_course = aov(course ~ gp, data = x_sub)

summary(aov_gp_comm)     # p = 1.5e-13
summary(aov_gp_course)   # p = 1.18e-10


## hours
x %>% group_by(hrs) %>% count
x %>% group_by(hrs) %>% summarise(mean(course), sd(course), 
                                  mean(comm), sd (comm))
# anova
aov_hrs_comm = aov(comm ~ hrs, data = x)
aov_hrs_course = aov(course ~ hrs, data = x)

summary(aov_hrs_comm)     # p = 2.47e-14
summary(aov_hrs_course)   # p = 5.81e-14
