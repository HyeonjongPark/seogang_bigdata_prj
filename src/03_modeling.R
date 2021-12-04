
library(dplyr)
library(data.table)

library(car)
library(caret)

library(forecast)

data = fread("./data/data_api/prep/paprika_prep.csv") %>% as.data.frame()

data$frmDate = NULL

# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인

data2$avg_outWs = NULL

colSums(is.na(data2))


#######################################################################
################## lm   + 다중공선성 확인   ########################### 
#######################################################################

# y : avg_outtrn  

# 결측 보정
data.imputed = rfImpute(avg_outtrn ~ ., data2[,-1])
data.imputed %>% str

dim(data.imputed)
colSums(is.na(data.imputed))

mod.lm = lm(avg_outtrn ~ ., data = data.imputed[,-c(2:12)])
summary(mod.lm)

vif(mod.lm) # 10이상이면 다중공선성 존재한다고 판단 -> 없음



pred.lm = predict(mod.lm,test.imputed[,-c(2:12)])
pred.lm = ifelse(pred.lm < 0, 0, pred.lm)

out.lm = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.lm)


forecast::accuracy(out.lm$real, out.lm$pred+1)




#######################################################################
################## split -> train, test     ########################### 
#######################################################################


set.seed(123)
proportion = 0.7
idx = sample(1:nrow(data2), size = round(proportion * nrow(data2)))
train = data2[idx, ]
test = data2[-idx, ]

id = test$index

y_train = train$avg_outtrn


# 결측이 없이 모델을 돌려야할 경우 사용
train.imputed = rfImpute(avg_outtrn ~ ., train[,-1])
test.imputed = rfImpute(avg_outtrn ~ ., test[,-1])






#######################################################################
############################ ARIMA  ################################### 
#######################################################################

#######################################################################
############################ ARIMAX ################################### 
#######################################################################

#######################################################################
############################ RF ####################################### 
#######################################################################

library(randomForest)

set.seed(123)

mod.rdf = randomForest(avg_outtrn ~ ., data = train.imputed, 
                       ntree=300,importance=T)

pred.rdf = predict(mod.rdf,test.imputed[,-1])
pred.rdf = ifelse(pred.rdf < 0, 0, pred.rdf)

out.rdf = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.rdf)

forecast::accuracy(out.rdf$real, out.rdf$pred)

#######################################################################

#######################################################################
############################ GBM ###################################### 
#######################################################################

install.packages("gbm")
library(gbm)
# GBM
# Final Model fit
mod.gbm <- gbm.fit(x = as.matrix(trainSparse), y = y_train, n.trees = 50,
                   shrinkage = 0.1 ,interaction.depth = 3, n.minobsinnode = 10,
                   distribution = "gaussian",bag.fraction = 0.5)

# Predict
pred.gbm <- predict(mod.gbm, newdata = as.matrix(testSparse), n.trees = 50)
pred.gbm = ifelse(pred.gbm < 0, 0, pred.gbm)

out.gbm = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.gbm)

forecast::accuracy(out.gbm$real, out.gbm$pred)


#######################################################################

#######################################################################
############################ XGB ###################################### 
#######################################################################

library(xgboost)


trainSparse = xgb.DMatrix(data.matrix(train[,-c(1,length(train))]), label=y_train, missing=NA)
testSparse  = xgb.DMatrix(data.matrix(test[,-c(1,length(test))]), missing = NA)
#236

foldsCV <- createFolds(y_train, k=20, list=TRUE, returnTrain=FALSE)


set.seed(123)

### 모델링
cat("modeling\n")

param.xgb <- list(subsample = 1
                  , max_depth = 5
                  , colsample_bytree = 0.5
                  , eta = 0.08
                  , eval_metric = 'rmse'#average
                  , min_child_weight = 1.2)



xgb_cv <- xgb.cv(data=trainSparse,
                 params=param.xgb,
                 nrounds=100,
                 prediction=TRUE,
                 maximize=TRUE,
                 folds=foldsCV,
                 #early_stopping_rounds = 50,
                 print_every_n = 5
)


mod.xgb = xgboost(data = trainSparse,
                  eta = 0.08,
                  nfold = 5, #5
                  max_depth = 5, # 5
                  min_child_weight = 1.2,
                  gamma = 0,
                  nround = 100, # 70
                  subsample = 1,
                  colsample_bytree = 0.5,
                  eval_metric = 'rmse',
                  verbose = 1)



# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)
xgb.plot.importance(xgb_imp, top_n = 10)



pred.xgb = predict(mod.xgb,testSparse)
pred.xgb = ifelse(pred.xgb < 0, 0, pred.xgb)

out.xgb = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.xgb)


forecast::accuracy(out.xgb$real, out.xgb$pred)






#######################################################################


#######################################################################
############################ LGB ###################################### 
#######################################################################

library(Matrix)
library(lightgbm)

train_sparse = Matrix(as.matrix(train[,-c(1,length(train))]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(test[,-c(1,length(test))]), sparse=TRUE)

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)


lgb.param = list(objective = "regression", 
              metric = "auc", 
              learning_rate= 0.1,
              num_leaves= 7,
              max_depth= 4,
              min_child_samples= 100,
              max_bin= 100,
              subsample= 0.7, 
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0,
              scale_pos_weight= 99.7)


lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}



best.iter = 300

# Train final model
mod.lgb = lgb.train(params = lgb.param, data = lgb.train, learning_rate = 0.03,
                    num_leaves = 60, num_threads = 30 , nrounds = best.iter,
                    eval_freq = 10, eval = lgb.normalizedgini)


pred.lgb = predict(mod.lgb, test_sparse)
pred.lgb = ifelse(pred.lgb < 0, 0, pred.lgb)

out.lgb = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.lgb)


forecast::accuracy(out.lgb$real, out.lgb$pred+1)





#######################################################################


#######################################################################
############################ SVR ###################################### 
#######################################################################

library(e1071)
library(Matrix)

train %>% dim
y_train %>% length

paramsvr <- list(gamma = 1e-4, cost = 1000, epsilon = 0.001)


trainSparse <- sparse.model.matrix(~. , data = train.imputed[,-c(1)])
testSparse <- sparse.model.matrix(~. , data = test.imputed[,-c(1)])




mod.svr <- svm(x = as.matrix(trainSparse) , y = y_train, type = "eps-regression",
               kernel = "radial",cost = paramsvr[2], gamma = paramsvr[1], 
               epsilon = paramsvr[3])


pred.svr <- predict(mod.svr, newdata = as.matrix(testSparse)) %>% as.vector()
pred.svr = ifelse(pred.svr < 0, 0, pred.svr)


out.svr = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.svr)

forecast::accuracy(out.svr$real, out.svr$pred+1)



#######################################################################






#############################################
######### 예측 모형 평가지표 종합 ###########
#############################################

lm_eval = cbind(model_name = "Regression", forecast::accuracy(out.lm$real, out.lm$pred+1))

rdf_eval = cbind(model_name = "RF", forecast::accuracy(out.rdf$real, out.rdf$pred))

gbm_eval = cbind(model_name = "GBM", forecast::accuracy(out.gbm$real, out.gbm$pred))

xgb_eval = cbind(model_name = "XGB", forecast::accuracy(out.xgb$real, out.xgb$pred))

lgb_eval = cbind(model_name = "LGB", forecast::accuracy(out.lgb$real, out.lgb$pred+1))

svr_eval = cbind(model_name = "SVR", forecast::accuracy(out.svr$real, out.svr$pred+1))

eval = do.call("rbind", list(lm_eval, 
                             rdf_eval,
                             gbm_eval,
                             xgb_eval,
                             lgb_eval,
                             svr_eval))

eval = eval %>% as.data.frame()
rownames(eval) = NULL
eval %>% arrange(RMSE)



