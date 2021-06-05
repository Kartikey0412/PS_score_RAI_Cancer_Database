#Propensity score calculation for preparing Matched Datasets
library(MatchIt)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(Matrix)
library(xgboost)
library(caTools)

r <- read.csv("RAI Database.csv")  #The dataset
r$treat <- as.factor(ifelse(r$raidose >110,1,0))   #Creating binary treatment variable
#summarising covariates in the study
rsub_cov <- c('sex','grossete', 'lnmets3_path', 'histologycode','posmargins', 'metsatdiagnosis')
 r%>%
  group_by(treat) %>%
  select(one_of(rsub_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))


 # T.TEST used to investigate differences between control vs treat for each covariate
 lapply(rsub_cov, function(v) {
   t.test(r[, v] ~ r[, 'treat'])
 })

 #Logistic Regression for propensity score calculation
 rsub_glm <- glm(treat~ sex + grossete+ lnmets3_path + histologycode +posmargins + metsatdiagnosis, family= binomial(), data= r)
 rpr_df <- data.frame(pr_score = predict(rsub_glm, type = "response"))

 rpr_df$treat <- r$treat
 rpr_df <- rpr_df[,-1]
 colnames(rpr_df)[1]<- "pr_score"


 r_nomiss <- r %>%
   select(treat, one_of(rsub_cov)) %>%
   na.omit()

 #using matchit algorithm with 'nearest' method for creating well matched subsets
 #propensity score from logistic regression
 pr_score <- rpr_df$pr_score
 mod_match <- matchit(treat~ sex  +grossete+ lnmets3_path + histologycode+ posmargins + metsatdiagnosis,
                      method = "nearest",  data = r_nomiss)

 dta_m_match <- match.data(mod_match)

 dta_m %>%
   group_by(treat) %>%
   select(one_of(rsub_cov)) %>%
   summarise_all(funs(mean))
 #Investigation of final matching performance
 lapply(rsub_cov, function(v) {
   chisq.test(dta_m[, v] , dta_m$treat)
 })
