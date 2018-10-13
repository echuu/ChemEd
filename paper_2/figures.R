
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



## create new supporting figures






# end of figures.R
