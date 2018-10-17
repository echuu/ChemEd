
# figures.R 

library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)


setwd('/home/eric/Dropbox/chem_ed')
x = read.csv("part2/data/full_data.csv") # 1020 x 57

n = nrow(x) # 1020 rows

## reproduce existing tables & figures in the paper

## must: MUST Score (min: 0, max: 20)
## comm: Common Questions Score (min: 0, max: 12)

## Table 2: Means and Standard Deviations:

x %>% dplyr::select(must, course, comm, alg, conc) %>% colMeans
x %>% dplyr::select(must, course, comm, alg, conc) %>% apply(FUN = sd, 2)

## note discrepancy with the paper, algorithmic and conceptual means/SDs swapped



## Figure 1: Avg. MUST Score vs. MUST Question Number (per school)

must_df = x %>% select(school, MQ1:MQ20)
must_means = aggregate(must_df[, -1], list(must_df$school), mean)

long_must_means = melt(must_means)
names(long_must_means) = c("school", "q", "avg")

(fig1 = ggplot(long_must_means, aes(x = q, y = avg, color = school)) + 
          geom_point() + geom_line(aes(group = school)) + 
          scale_x_discrete(labels = c(1:20)) + 
          labs(x = "MUST Question Number", y = "Average Score", 
               title = "MUST Question Average per University") + theme_bw())

## Figure 2: Avg. Common Question Score vs. Common Question Number (per school)
cq_df = x %>% select(school, Q1A:Q6C)
cq_means = aggregate(cq_df[, -1], list(cq_df$school), mean)
long_cq_means = melt(cq_means)
names(long_cq_means) = c("school", "q", "avg")

(fig2 = ggplot(long_cq_means, aes(x = q, y = avg, color = school)) + 
          geom_point() + geom_line(aes(group = school)) + 
          scale_x_discrete(labels = c(1:12)) + 
          labs(x = "Common Question Number", y = "Average Score", 
               title = "Common Question Average per University") + theme_bw())


## Table 4: Conceptual & Algorithmic Correct Performance by Question Topic
##          means (sds) for both types of questions
##          p-value () -- should be a paired t-test, paper only reports 
##                        2-tailed t-test p-value

# table organization: mean/sd for each type of question (alg or conc)
##                    per question (1 - 6)

## paper says: the average number of students correctly answering each question
## out of 1020 students, it doesn't seem likely that only 84 answer answered 
## the q1_algorithmic correctly

## actual calculation: column sums / n * 100
round(colSums(cq_df[,-1]) / n * 100, 2)
cq_df[,-1] %>% apply(FUN = sd, 2) * 100

## instad: just report the proportion answered correctly, and sd
cq_means = round(colMeans(cq_df[,-1]), 2)
cq_sds   = cq_df[,-1] %>% apply(FUN = sd, 2) %>% round(digits = 2)

## paired t.test
p_vals = numeric(6) # vector to store each of the p-values (for each question)
algo = cq_df[,seq(2, 12, 2)] # algorithmic questions
conc = cq_df[,seq(3, 13, 2)] # conceptual questions

for (i in 1:6) {
  p_vals[i] = t.test(algo[,i], conc[,i], alternative = 'two.sided',
                     var.equal = TRUE, paired = TRUE)$p.value
}

# table that resembles table 4 in the paper
t(data.frame(algo = cq_means[seq(1, 12, 2)], conc = cq_means[seq(2, 12, 2)],
           p_val = p_vals))




## Figure 3: Avg Course Percent vs Algorithmic CQ Score
##           course performance by # of algorithmic q's answered correctly

# mean course average vs. algorithmic score (0 - 6)
course_alg = x %>% group_by(as.factor(alg)) %>% summarise(mean(course)) %>% 
  data.frame()
names(course_alg) = c("alg_score", "course_avg")
(fig3 = ggplot(course_alg, aes(x = alg_score, y = course_avg)) + 
  geom_bar(stat = 'identity') + scale_y_continuous(limits = c(0,100)) +
  geom_text(aes(x = alg_score, y = course_avg + 3, 
                label = round(course_avg, 1)))) + theme_bw() + 
  labs(x = "Algorithmic CQ Score", y = "Average Course Percent")




## Figure 4: Avg Course Percent vs Conceptual CQ Score
##           course performance by # of conceptual q's answered correctly

course_conc = x %>% group_by(as.factor(conc)) %>% summarise(mean(course)) %>% 
  data.frame()
names(course_conc) = c("conc_score", "course_avg")
(fig4 = ggplot(course_conc, aes(x = conc_score, y = course_avg)) + 
    geom_bar(stat = 'identity') + scale_y_continuous(limits = c(0,100)) +
    geom_text(aes(x = conc_score, y = course_avg + 3, 
                  label = round(course_avg, 1)))) + theme_bw() + 
  labs(x = "Conceptual CQ Score", y = "Average Course Percent")



## Figure 5: Avg Course Percent vs MUST Score
##           course performance by # of MUST q's answered correctly


course_must = x %>% group_by(as.factor(must)) %>% summarise(mean(course)) %>% 
  data.frame()
names(course_must) = c("must_score", "course_avg")
(fig5 = ggplot(course_must, aes(x = must_score, y = course_avg)) + 
    geom_bar(stat = 'identity') + scale_y_continuous(limits = c(0,100)) +
    geom_text(aes(x = must_score, y = course_avg + 3, 
                  label = round(course_avg, 1)))) + theme_bw() + 
  labs(x = "MUST Score", y = "Average Course Percent")


## Figure 6: Avg Course Percent vs Number of Common Questions Answered Correctly

course_cq = x %>% group_by(as.factor(comm)) %>% summarise(mean(course)) %>% 
  data.frame
names(course_cq) = c("comm_score", "course_avg")
(fig6 = ggplot(course_cq, aes(x = comm_score, y = course_avg)) + 
    geom_bar(stat = 'identity') + scale_y_continuous(limits = c(0,100)) +
    geom_text(aes(x = comm_score, y = course_avg + 3, 
                  label = round(course_avg, 1)))) + theme_bw() + 
  labs(x = "Common Questions Score", y = "Average Course Percent")



## Table 5: Predictability of success and failure of each diagnostic instrument

## not reproduced -- reasoning, model seems problematic, oversimplified

## Table 6: % of students successfully identified out of faily population and
##          % students misidentified as at risk by group

## not reproduced -- reasoning, model seems problematic, oversimplified
## uses the same model used to generate the results in table 5


## Figure 7: avg score on common qustions vs # of MUST q's answered correctly

# mean of common questions score per MUST score (0 - 20)
cq_must = x %>% group_by(as.factor(must)) %>% summarise(mean(comm)) %>% 
  data.frame()
names(cq_must) = c("must", "cq")
(fig7 = ggplot(cq_must, aes(x = must, y = cq)) + 
    geom_bar(stat = 'identity') + scale_y_continuous(limits = c(0,12)) +
    geom_text(aes(x = must, y = cq + 1, 
                  label = round(cq, 1))) + theme_bw() + 
  labs(x = "MUST Score", y = "Average Common Question Score"))


## Figure 8: average common question performance (0,1) per MUST group

# create MUST performance groups: 1, 5, 10, 15, 20
# subset out students with scores exactly equal to 1, 5, 10, 15, 20
must_group = x %>% filter(must %in% c(1, 5, 10, 15, 20)) # 224 observations

cq_must_df = x %>% dplyr::select(must, Q1A:Q6C) %>% 
  filter(must %in% c(1, 5, 10, 15, 20))

cq_by_group = aggregate(cq_must_df[, -1], list(cq_must_df$must), mean)
names(cq_by_group) = c("group", as.character(1:12))


cq_by_group_long = melt(cq_by_group, id.vars = "group")
ggplot(cq_by_group_long, aes(x = variable, y = value, 
                             group = group, color = as.factor(group))) + 
  geom_point() + geom_line() +
  labs(x = "Common Question", y = "Average Score", 
       main = "Average CQ score by Question") +
  scale_y_continuous(limits = c(0, 1))


## table 7: course percent and comment question score by gender

x %>% group_by(gender) %>% summarise(mean(course)) # mean course by gender
x %>% group_by(gender) %>% summarise(sd(course))   # sd course by gender

x %>% group_by(gender) %>% summarise(mean(comm))   # comm score by gender
x %>% group_by(gender) %>% summarise(sd(comm))     # sd score by gender


# t-test to test for differences in course average per gender, cq per gender
# sds (shown in previous calculation) are approx equal, so we use var.equal = T

# p-value = 0.8448
t.test(x$course[x$gender == 0], x$course[x$gender == 1], var.equal = T) 

# p-value = 0.2874
t.test(x$comm[x$gender == 0], x$comm[x$gender == 1], var.equal = T) 



## Table 8: Course percent and common question score by ethnicity 
eth_counts = x %>% group_by(ethnic) %>% count %>% data.frame()
course_avg = x %>% group_by(ethnic) %>% summarise(mean(course)) %>% data.frame()
course_sd  = x %>% group_by(ethnic) %>% summarise(sd(course)) %>% data.frame()
cq_avg     = x %>% group_by(ethnic) %>% summarise(mean(comm)) %>% data.frame()
cq_sd      = x %>% group_by(ethnic) %>% summarise(sd(course)) %>% data.frame()

tbl8 = data.frame(eth_counts, course_avg[,2], course_sd[,2], 
                  cq_avg[,2], cq_sd[,2])
names(tbl8) = c("Ethn", "N", "Course_Avg", "Course_SD", "CQ_Score", "CQ_SD")
tbl8[c(1:4, 6, 5),]  # course average column does not match

# anova to test difference in means between groups: course average
# first examine plots of course average for each group
ggplot(x, aes(x = ethnic, y = course)) + geom_boxplot() + ylab("Course Average")

aov_course = aov(course ~ ethnic, data = x)
summary(aov_course)

# anova to test difference in means between groups: cq score
# first examine plots of cq_score for each group
ggplot(x, aes(x = ethnic, y = comm)) + geom_boxplot() + ylab("CQ Score") 

aov_comm = aov(comm ~ ethnic, data = x)
summary(aov_comm)



## Table 9: Course percent and average common question score by 
##          college graduation of grandparents and parents 


# create the groups: nn, ny, yn, yy (Grandparents/Parents College)
x = x %>% mutate(gp_edu = "N/A") # create variable for the group described above
x$gp_edu[(x$grand == "N") & (x$parent == "N")] = "NN"  # grand no, parent no
x$gp_edu[(x$grand == "N") & (x$parent == "Y")] = "NY"  # grand no, parent yes
x$gp_edu[(x$grand == "Y") & (x$parent == "N")] = "YN"  # grand yes, parent no
x$gp_edu[(x$grand == "Y") & (x$parent == "Y")] = "YY"  # grand yes, parent yes


gp_counts  = x %>% group_by(gp_edu) %>% count %>% data.frame()
course_avg = x %>% group_by(gp_edu) %>% summarise(mean(course)) %>% data.frame()
course_sd  = x %>% group_by(gp_edu) %>% summarise(sd(course)) %>% data.frame()
cq_avg     = x %>% group_by(gp_edu) %>% summarise(mean(comm)) %>% data.frame()
cq_sd      = x %>% group_by(gp_edu) %>% summarise(sd(course)) %>% data.frame()

tbl9 = data.frame(gp_counts, course_avg[,2], course_sd[,2], 
                  cq_avg[,2], cq_sd[,2])
names(tbl9) = c("Grand/Parent", "N", "Course_Avg", "Course_SD", 
                "CQ_Score", "CQ_SD")


# first examine plots of course average for each group
ggplot(x, aes(x = gp_edu, y = course)) + geom_boxplot() + 
  labs(y = "Course Average", x = "Grand/Parents College")

# include N/A group
aov_course = aov(course ~ gp_edu, data = x)
summary(aov_course) # p-value: 1.53e-09

# if we exclude N/A group
aov_course1 = aov(course ~ gp_edu, data = x[x$gp_edu != "N/A",])
summary(aov_course1) # p-value: 1.18e-10 --- still very significant


# first examine plots of cq_score average for each group
ggplot(x, aes(x = gp_edu, y = comm)) + geom_boxplot() + 
  labs(y = "Comm Score", x = "Grand/Parents College")

# include N/A group
aov_comm = aov(comm ~ gp_edu, data = x)
summary(aov_comm) # p-value: 3.04e-13 ***

# if we exclude N/A group
aov_comm1 = aov(comm ~ gp_edu, data = x[x$gp_edu != "N/A",])
summary(aov_comm1) # p-value: 1.5e-13 *** --- still very significant



## Table 10: Course average and average common question score 
##           by hours per week of pay

hrs_counts  = x %>% group_by(hrs) %>% count %>% data.frame()
course_avg  = x %>% group_by(hrs) %>% summarise(mean(course)) %>% data.frame()
course_sd   = x %>% group_by(hrs) %>% summarise(sd(course)) %>% data.frame()
cq_avg      = x %>% group_by(hrs) %>% summarise(mean(comm)) %>% data.frame()
cq_sd       = x %>% group_by(hrs) %>% summarise(sd(course)) %>% data.frame()

tbl10 = data.frame(hrs_counts, course_avg[,2], course_sd[,2], 
                  cq_avg[,2], cq_sd[,2])
names(tbl10) = c("Hours", "N", "Course_Avg", "Course_SD", 
                "CQ_Score", "CQ_SD")

# first examine plots of course average for each group
ggplot(x, aes(x = hrs, y = course)) + geom_boxplot() + 
  labs(y = "Course Average", x = "Hours")

# include N/A group
aov_course = aov(course ~ hrs, data = x)
summary(aov_course) # p-value: 5.81e-14

# first examine plots of cq_score average for each group
ggplot(x, aes(x = hrs, y = comm)) + geom_boxplot() + 
  labs(y = "Comm Score", x = "Hours")

# include N/A group
aov_comm = aov(comm ~ hrs, data = x)
summary(aov_comm) # p-value: 2.47e-14


## Table 11: Course percent and average common question score by 
##           Texas zip code 

x0 = x %>% filter(zip %in% c("75", "76", "77", "78", "79"))
zip_counts  = x0 %>% group_by(zip) %>% count %>% data.frame()
course_avg  = x0 %>% group_by(zip) %>% summarise(mean(course)) %>% data.frame()
course_sd   = x0 %>% group_by(zip) %>% summarise(sd(course)) %>% data.frame()
cq_avg      = x0 %>% group_by(zip) %>% summarise(mean(comm)) %>% data.frame()
cq_sd       = x0 %>% group_by(zip) %>% summarise(sd(course)) %>% data.frame()

tbl11 = data.frame(zip_counts, course_avg[,2], course_sd[,2], 
                   cq_avg[,2], cq_sd[,2])
names(tbl11) = c("Zip", "N", "Course_Avg", "Course_SD", 
                 "CQ_Score", "CQ_SD")

# first examine plots of course average for each group
ggplot(x0, aes(x = zip, y = course)) + geom_boxplot() + 
  labs(y = "Course Average", x = "Zip")

# include N/A group
aov_course = aov(course ~ zip, data = x0)
summary(aov_course) # p-value: 5.81e-14

# first examine plots of cq_score average for each group
ggplot(x0, aes(x = zip, y = comm)) + geom_boxplot() + 
  labs(y = "Comm Score", x = "Zip")

# include N/A group
aov_comm = aov(comm ~ zip, data = x0)
summary(aov_comm) # p-value: 2.47e-14


write.csv(x, "part2/data/x_model.csv", row.names = FALSE)

# end of figures.R
