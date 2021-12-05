
source("./src/00_libs.R")

#############
## 일 단위 ##

data = fread("./data/data_api/prep/paprika_prep_days.csv") %>% as.data.frame()
data %>% head
data$frmDate = NULL

# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))


data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인

data2$avg_outWs = NULL

colSums(is.na(data2))


############



#############
## 주 단위 ## ver1

data = fread("./data/data_api/prep/paprika_prep_weeks.csv") %>% as.data.frame()
data$year_week = NULL
# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2 %>% head
data2 %>% tail

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인


colSums(is.na(data2))
data2 %>% head
data2 %>% tail

colnames(data2)[ncol(data2)] = "avg_outtrn"

data2 %>% head

############






#############
## 주 단위 + dacon data ver2

data = fread("./data/data_api/prep/paprika_prep_weeks2.csv")%>% as.data.frame()

data$year_week = NULL
# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2 %>% head
data2 %>% tail

data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인


colSums(is.na(data2))
data2 %>% head
data2 %>% tail

colnames(data2)[ncol(data2)] = "avg_outtrn"

data2 %>% head

############





#############
## 주 단위 + dacon data + lag,lead ver3

data = fread("./data/data_api/prep/paprika_prep_weeks2.csv")%>% as.data.frame()

data$year_week = NULL
# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2 %>% head
data2$avg_price_lead1 = lead(data2$avg_price, 1) 
data2$avg_price_lead2 = lead(data2$avg_price, 2) 
data2$avg_price_lead3 = lead(data2$avg_price, 3) 
data2$avg_price_lead1 = lag(data2$avg_price, 1) 
data2$avg_price_lead2 = lag(data2$avg_price, 2) 
data2$avg_price_lead3 = lag(data2$avg_price, 3) 

data2$avg_volume_lead1 = lead(data2$avg_volume, 1) 
data2$avg_volume_lead2 = lead(data2$avg_volume, 2) 
data2$avg_volume_lead3 = lead(data2$avg_volume, 3) 
data2$avg_volume_lead1 = lag(data2$avg_volume, 1) 
data2$avg_volume_lead2 = lag(data2$avg_volume, 2) 
data2$avg_volume_lead3 = lag(data2$avg_volume, 3) 

data2 = data2[-c(1:3),]

data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인


colSums(is.na(data2))
data2 %>% head
data2 %>% tail

colnames(data2)[ncol(data2)] = "avg_outtrn"

data2 %>% head



############













#######################################################################
################## lm   + 다중공선성 확인   ########################### 
#######################################################################

# y : avg_outtrn  

# 결측 보정
data.imputed = rfImpute(avg_outtrn ~ ., data2[,-1])
data.imputed %>% str

dim(data.imputed)
colSums(is.na(data.imputed))

data.imputed %>% head
#mod.lm = lm(avg_outtrn ~ ., data = data.imputed[,-c(2:12)])
mod.lm = lm(avg_outtrn ~ ., data = data.imputed[,-c(2:14)])
summary(mod.lm)

vif(mod.lm) # 10이상이면 다중공선성 존재한다고 판단 -> 없음


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





#pred.lm = predict(mod.lm,test.imputed[,-c(2:12)])
pred.lm = predict(mod.lm,test.imputed[,-c(3:14)])
pred.lm = ifelse(pred.lm < 0, 0, pred.lm)

out.lm = data.frame(id = id, 
                    real = test$avg_outtrn, 
                    pred = pred.lm)


forecast::accuracy(out.lm$real, out.lm$pred+1)






#######################################################################
############################ ARIMA  ################################### 
#######################################################################

#######################################################################
############################ ARIMAX ################################### 
#######################################################################

#######################################################################
############################ RF ####################################### 
#######################################################################


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

# GBM
trainSparse <- sparse.model.matrix(~. , data = train.imputed[,-c(1)])
testSparse <- sparse.model.matrix(~. , data = test.imputed[,-c(1)])


matrix(trainSparse)
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



#236
trainSparse = xgb.DMatrix(data.matrix(train[,-c(1,length(train))]), label=y_train, missing=NA)
testSparse  = xgb.DMatrix(data.matrix(test[,-c(1,length(test))]), missing = NA)

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
xgb.plot.importance(xgb_imp, top_n = 20)



pred.xgb = predict(mod.xgb,testSparse)
pred.xgb = ifelse(pred.xgb < 0, 0, pred.xgb)

out.xgb = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.xgb)


forecast::accuracy(out.xgb$real, out.xgb$pred+1)






#######################################################################


#######################################################################
############################ LGB ###################################### 
#######################################################################


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


train %>% dim
y_train %>% length

paramsvr <- list(gamma = 1e-4, cost = 100, epsilon = 0.001)


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
colnames(out.lm)[3] = "lm_pred"
colnames(out.rdf)[3] = "rdf_pred"
colnames(out.gbm)[3] = "gbm_pred"
colnames(out.xgb)[3] = "xgb_pred"
colnames(out.lgb)[3] = "lgb_pred"
colnames(out.svr)[3] = "svr_pred"

out = do.call("cbind", list(out.lm,
                            out.rdf,
                            out.gbm,
                            out.xgb,
                            out.lgb,
                            out.svr))
out = out[c(1,2,grep("pred",colnames(out)))]

#fwrite(out, "./out/pred_out_days.csv")
#fwrite(out, "./out/pred_out_weeks.csv")
# fwrite(out, "./out/pred_out_weeks2.csv")
fwrite(out, "./out/pred_out_weeks3.csv")

lm_eval = cbind(model_name = "Regression", forecast::accuracy(out.lm$real, out.lm$lm_pred+1))
rdf_eval = cbind(model_name = "RF", forecast::accuracy(out.rdf$real, out.rdf$rdf_pred))
gbm_eval = cbind(model_name = "GBM", forecast::accuracy(out.gbm$real, out.gbm$gbm_pred+1))
xgb_eval = cbind(model_name = "XGB", forecast::accuracy(out.xgb$real, out.xgb$xgb_pred+1))
lgb_eval = cbind(model_name = "LGB", forecast::accuracy(out.lgb$real, out.lgb$lgb_pred+1))
svr_eval = cbind(model_name = "SVR", forecast::accuracy(out.svr$real, out.svr$svr_pred+1))

eval = do.call("rbind", list(lm_eval, 
                             rdf_eval,
                             gbm_eval,
                             xgb_eval,
                             lgb_eval,
                             svr_eval))

eval = eval %>% as.data.frame()
rownames(eval) = NULL
eval = eval %>% arrange(RMSE)
eval

#fwrite(eval, "./out/eval_out_days.csv")
#fwrite(eval, "./out/eval_out_weeks.csv")
#fwrite(eval, "./out/eval_out_weeks2.csv")
fwrite(eval, "./out/eval_out_weeks3.csv")

