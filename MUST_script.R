
library(ggplot2) # for prettier figures

####
#### MUST standardized test of math "automaticity" 
#### for predicting performance in CHEM I.
####

## set working directory to source file location

dta <- read.csv("data/MUST_data.csv")[1:1073, 1:43]
colnames(dta)[5] <- "Gender_Male"
n <- nrow(dta)

##
## Decompose student, school identifiers.
##

id <- as.character(dta[, 1])

## Split off last 3 characters (subject number within institution). 
## Some missing numbers. What happened to those students' data?
id_inst <- subj_inst <- character(n)
for(i in 1:n) {
  n_char <- nchar(id[i])
  id_inst[i] <- substr(id[i], 1, n_char - 3)
  subj_inst[i] <- substr(id[i], n_char - 2, n_char)
}

## Store just the 7 institution identifiers.
id_inst[grep("ACUP", id_inst)] <- "ACUP"
id_inst[grep("TAMSA", id_inst)] <- "TAMSA"
id_inst[grep("TAMU", id_inst)] <- "TAMU"
id_inst[grep("TSU", id_inst)] <- "TSU"
id_inst[grep("UNTB", id_inst)] <- "UNTB"
id_inst[grep("UNTW", id_inst)] <- "UNTW"
id_inst[grep("UTXW", id_inst)] <- "UTXW"
id_inst_nms <- c("ACUP", "TAMSA", "TAMU", "TSU", "UNTB", "UNTW", "UTXW")

## note that the number of observations from each school is different
## when we split into train/test sets, take equal proportions from each school
## so that we do not completely leave out a school
table(id_inst) 

##
## Data organization. Will treat numeric course grade in column 'Course' 
## as the response variable, with predictor variables for student 
## classification ('Class'), gender (F=0, M=1), ethnicity, some information 
## on major, born in TX, attended TX high school, something called "Zip"(?), 
## some qualitative information on high school chemistry, some kind of estimate 
## of standardized math performance, whether grandparents went to college, 
## whether parents went to college, some information on employment, indicators 
## (I think) of whether CHEM I and II have been taken, the version of the MUST 
## exam, and the total exam score. 
##

y <- as.numeric(dta$Course)
X_df <- dta[, -c(1:3)]

##
## EDA...
##

##
## Reproduce Figure 1: MUST question averages by university.
##

MUST_df <- X_df[, 20:39]    # Q1 - Q20 indicators (1073 x 20)
MUST <- matrix(as.numeric(as.matrix(MUST_df)), ncol = 20)
colnames(MUST) <- colnames(MUST_df)
MUST_sum <- as.numeric(X_df[, 40])

## One NA that should be a 0.
MUST[is.na(MUST)] <- 0

## Question averages by university.
MUST_inst <- vector("list", length = 7)
names(MUST_inst) <- id_inst_nms
for(j in 1:7) {
  ## for the j-th school, average the score of each question (b/w 0 and 1)
  ## j-th element of MUST_inst contains the avg score for each of 20 questions
  MUST_inst[[j]] <- colMeans(MUST[id_inst == id_inst_nms[j], ])
}

## Plot question averages as connected line segments, one per university.
plot(c(1, 20), c(0, 1), type = "n", xlab = "question", ylab = "average")
for(j in 1:7) {
  lines(1:20, MUST_inst[[j]], col = j + 1)
}
  
##
## Reproduce Table 1: Comparison of mean MUST score among successful 
## (course average at least 69.5) and unsuccessful.
##

x_s <- MUST_sum[y >= 69.5]; n_s <- length(x_s)
x_u <- MUST_sum[y < 69.5]; n_u <- length(x_u)
sd_s <- sd(x_s)
sd_u <- sd(x_u)
se_s <- sd_s / sqrt(n_s)
se_u <- sd_u / sqrt(n_u)
t.test(x_s, x_u)

##
## Reproduce Figure 2: Mean course average as a linear function of MUST score.
##

MUST_poss <- 0:20
y_MUST <- numeric(21)
for(i in 1:21) {
  y_MUST[i] <- mean(y[MUST_sum == (i - 1)])
}
plot(0:20, y_MUST, type = "b", ylim = c(0, 100))



