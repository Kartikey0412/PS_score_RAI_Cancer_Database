#SuperLearner
library(MatchIt)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(Matrix)
library(xgboost)
library(caTools)
library(SuperLearner)


rsl <- r[,c(3,33,41,47,30,22,105)]
rsl <- na.omit(rsl)
rsl$treat <- as.numeric(rsl$treat)
rsl$treat <- rsl$treat -1

#creating different models in SL library
SL.library <- c("SL.glm", "SL.glm.interaction", "SL.mean", "SL.glmnet", "SL.randomForest")

#Fitting ensemble model with cross- validation
SL.out <- SuperLearner(Y = rsl$treat, X = rsl[,c(1:6)],
                       SL.library = SL.library,
                       family = 'binomial',
                       cvControl = list(V=10))


pscore <- SL.out$SL.predict

rsl$weights <- ifelse(rsl$treat==1, 1, pscore/(1-pscore))

#using full method matching for gourps
rslf_match <- matchit(treat~ sex  +grossete+ lnmets3_path + histologycode+ posmargins + metsatdiagnosis,  data = rsl,
                      method = "full", distance= pscore)

rslf_m <- match.data(rslf_match)

rsub_cov <- c('sex','grossete', 'lnmets3_path', 'histologycode','posmargins', 'metsatdiagnosis')

lapply(rsub_cov, function(v) {
  chisq.test(rsl_m[, v] , rsl_m$treat)
})

cov <- rsl_m[,c(1:6)]
rsl_m$rec <- r[rownames(rsl_m),]$recurrence
rsl$rec <- r[rownames(rsl),]$recurrence

#calculating effect of treatment on response variable recurrence from the matched dataset after Super Learner
rsl_m_glm <- glm(rec~ treat+ sex+ grossete + lnmets3_path + histologycode + posmargins + metsatdiagnosis, family= binomial,data=rsl_m ) 
