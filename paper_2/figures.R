
# figures.R 

library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)


setwd('/home/eric/Dropbox/chem_ed')
x = read.csv("part2/data/full_data.csv")


## reproduce existing figures in the paper

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


cq_labels = c(rep(1:6, 2), )

(fig2 = ggplot(long_cq_means, aes(x = q, y = avg, color = school)) + 
          geom_point() + geom_line(aes(group = school)) + 
          scale_x_discrete(labels = c(1:12)) + 
          labs(x = "Common Question Number", y = "Average Score", 
               title = "Common Question Average per University") + theme_bw())

## Table 4: Conceptual & Algorithmic Correct Performance by Question Topic
##          means (sds) for both types of questions
##          p-value ()



## Figure 3: Avg Course Percent vs Algorithmic CQ Score
##           course performance by # of algorithmic q's answered correctly


## Figure 4: Avg Course Percent vs Conceptual CQ Score
##           course performance by # of conceptual q's answered correctly


## Figure 5: Avg Course Percent vs MUST Score
##           course performance by # of MUST q's answered correctly





## create new supporting figures






# end of figures.R
