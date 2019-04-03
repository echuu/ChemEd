

# summary statistics for demographics

library(dplyr)
library(userfriendlyscience) # for games-howell post hoc test

# setwd("C:/Users/chuu/ChemEd/paper_2")

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
x_sub = x %>% dplyr::select(grand, parent, course, comm, hrs, ethnic) %>% 
    filter(grand != "DK", parent != "DK")
x_sub$grand = droplevels(x_sub$grand)
x_sub$parent = droplevels(x_sub$parent)
table(x_sub$grand, x_sub$parent)
x_sub = x_sub %>% mutate(gp = paste(grand, parent, sep = ''))

x_sub$gp[x_sub$gp == "YN"] = "NY"

x_sub %>% group_by(gp) %>% count
x_sub %>% group_by(gp) %>% summarise(mean(course), sd(course), 
                                     mean(comm), sd (comm))
# anova
aov_gp_comm = aov(comm ~ as.factor(gp), data = x_sub)
aov_gp_course = aov(course ~ gp, data = x_sub)

summary(aov_gp_comm)     # p = 6.75e-14
summary(aov_gp_course)   # p = 1.6e-10


## hours


x$hrs = as.character(x$hrs)
x$hrs[grep(c("20-29"), x$hrs)] = "20+"
x$hrs[grep(c("30-39"), x$hrs)] = "20+"
x$hrs[grep(c("40+"), x$hrs)] = "20+"
x$hrs = as.factor(x$hrs)


x %>% group_by(hrs) %>% count
x %>% group_by(hrs) %>% summarise(mean(course), sd(course), 
                                  mean(comm), sd (comm))
# anova
aov_hrs_comm = aov(comm ~ hrs, data = x)
aov_hrs_course = aov(course ~ hrs, data = x)

summary(aov_hrs_comm)     # p = 5.66e-12
summary(aov_hrs_course)   # p = 4.34e-14


# -----------------------------------------------------------------------

# post hoc analysis -- CQ for ethnicity, grandparents/parents, hours

# games-howell post hoc find to find differences between groups


# subset out the response(s) and the 3 variables that we want to do 
# post hoc analysis on
x_ph = x_sub %>% dplyr::select(course, comm, gp, hrs, ethnic)


# (1) parents/grandparents (NN, YN, YY)

gp_test_course = oneway(as.factor(x_ph$gp), y = x_ph$course, 
                        posthoc = "games-howell")
gp_test_course

gp_test_comm = oneway(as.factor(x_ph$gp), y = x_ph$comm, 
                      posthoc = "games-howell")
gp_test_comm

# (2) hrs (0, 1-10, 11-19, 20+)

hrs_test_course = oneway(as.factor(x_ph$hrs), y = x_ph$course, 
                         posthoc = "games-howell")

hrs_test_course

hrs_test_comm = oneway(as.factor(x_ph$hrs), y = x_ph$comm, 
                       posthoc = "games-howell")

hrs_test_comm

# (3) ethnic 
ethnic_test_course = oneway(as.factor(x_ph$ethnic), y = x_ph$course, 
                            posthoc = "games-howell")

ethnic_test_course

ethnic_test_comm = oneway(as.factor(x_ph$ethnic), y = x_ph$comm, 
                          posthoc = "games-howell")

ethnic_test_comm



