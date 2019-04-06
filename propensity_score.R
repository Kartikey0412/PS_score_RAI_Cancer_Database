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
rsub_cov <- c('sex','grossete', 'lnmets3_path', 'histologycode','posmargins', 'metsatdiagnosis')
 r%>%
  group_by(treat) %>%
  select(one_of(rsub_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))


 # T.TEST used to investigate matching before
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


#xgboost

rxg <- data.frame(r$sex, r$grossete, r$lnmets3_path, r$histologycode, r$posmargins, r$metsatdiagnosis, r$treat)
rxg <- na.omit(rxg)
rxgdata <- data.frame(rxg$r.sex, rxg$r.grossete, rxg$r.lnmets3_path, rxg$r.histologycode, rxg$r.posmargins, rxg$r.metsatdiagnosis)
rxgdata <- as.matrix(rxgdata)
rxgdata <- as(rxgdata, "sparseMatrix")
rxglabel <- as.numeric(as.numeric(rxg$r.treat)-1)
rxglist <- list(rxgdata, rxglabel)

# Cross validation for optimal Gradient Boosted Trees
ind <- createDataPartition(rxg$r.treat, p = 2/3, list = FALSE)
trainDF<-rxg[ind,]
testDF<-rxg[-ind,]

ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE
)

parametersGrid <-  expand.grid(eta = c(0.1,0.3),gamma=0,
                               subsample=1, colsample_bytree=1,
                               max_depth=c(3,6,9),
                               nrounds=50,
                              min_child_weight=1
)
rxg1 <- rxg
rxg1$r.treat <- as.factor(rxg$r.treat)
modelxgboost <- train(r.treat~.,
                      data = rxg1,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)

feature.names=names(rxg1)

for (f in feature.names) {
  if (class(rxg1[[f]])=="factor") {
    levels <- unique(c(rxg1[[f]]))
    rxg1[[f]] <- factor(rxg1[[f]],
                         labels=make.names(levels))
  }
}

rxg_df <- data.frame(pr_score = predict(rxgdense,rxgdata))

rxg_df$treat <- rxg$treat
rxg_df <- rxg_df[,-1]
colnames(rxg_df)[1]<- "pr_score"

pr_score <- predict(rxgdense,rxgdata)

rxg_match <- matchit(r.treat~ r.sex  +r.grossete+ r.lnmets3_path + r.histologycode+ r.posmargins + r.metsatdiagnosis,
                     method = "nearest",  data = rxg, distance = pr_score)

rxg_m <- match.data(rxg_match)

rsub_cov <- c('r.sex','r.grossete', 'r.lnmets3_path', 'r.histologycode','r.posmargins', 'r.metsatdiagnosis')

lapply(rsub_cov, function(v) {
  chisq.test(rxg_m[, v] , rxg_m$r.treat)
})


#SuperLearner

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


#treat 0.04, calculating effect of treatment on response variable recurrence from the matched dataset after Super Learner
rsl_m_glm <- glm(rec~ treat+ sex+ grossete + lnmets3_path + histologycode + posmargins + metsatdiagnosis, family= binomial,data=rsl_m ) 
