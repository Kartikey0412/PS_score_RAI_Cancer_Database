#xgboost matching 
library(MatchIt)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(Matrix)
library(xgboost)
library(caTools)


rxg <- data.frame(r$sex, r$grossete, r$lnmets3_path, r$histologycode, r$posmargins, r$metsatdiagnosis, r$treat)
rxg <- na.omit(rxg)
rxgdata <- data.frame(rxg$r.sex, rxg$r.grossete, rxg$r.lnmets3_path, rxg$r.histologycode, rxg$r.posmargins, rxg$r.metsatdiagnosis)
rxgdata <- as.matrix(rxgdata)
#Creating sparse matrix
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

#Investigating matching performance after matching  
lapply(rsub_cov, function(v) {
  chisq.test(rxg_m[, v] , rxg_m$r.treat)
})