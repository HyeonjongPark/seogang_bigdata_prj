
# frmhsId
# 농가코드

# measDtStr
# 측정일시

# inTp
# 내부온도

# outTp
# 외부온도

# inHd
# 내부습도

# inCo2
# 내부Co2

# outWs
# 풍속

# acSlrdQy
# 누적일사량

# ec
# 급액 EC

# ph
# 급액 pH

# cunt
# 일 급액횟수

# daysuplyqy
# 일 급액량(cc/1주수)

# otmsuplyqy
# 1회 급액량(cc/1주수)

# outtrn
# 생산량

## 
library(data.table)
library(tidyr)
library(highcharter)
library(quantmod)
library(lubridate)

paprika = fread("./data/data_api/paprika.csv", encoding = "UTF-8") %>% as.data.frame() %>% arrange(measDtStr)

# 데이터 수집 기간
paprika %>% head # 2017-07-31 ~ 
paprika %>% tail # 2018-11-27 

# 농가별로 정렬
paprika = paprika %>% arrange(frmhsId)

paprika %>% dim # 107235     22
colSums(is.na(paprika)) # 결측치 X

# 0이 100000 개 이상존재하는 컬럼들 확인
colSums(paprika == 0) 
paprika[colSums(paprika == 0) > 100000] %>% colnames()  






# "otmsuplyqy" -> 급액을 안헀다면 0일 수 있음
# "acSlrdQy"   -> 누적 일사량 0 일 수 없음 -> 10만개가 넘으므로 컬럼 제외
paprika$acSlrdQy = NULL 
# "cunt"       -> 급액을 안헀다면 0일 수 있음
# "daysuplyqy" -> 급액을 안헀다면 0일 수 있음

# 결측이라 0 인 것은 NA로 대체 필요 
paprika$inTp[paprika$inTp == 0] = NA
paprika$inHd[paprika$inHd == 0] = NA
paprika$outTp[paprika$outTp == 0] = NA
paprika$outWs[paprika$outWs == 0] = NA
paprika$inCo2[paprika$inCo2 == 0] = NA
paprika$ec[paprika$ec == 0] = NA
paprika$ph[paprika$ph == 0] = NA
paprika$otmsuplyqy[paprika$otmsuplyqy == 0] = NA



# Y인자 확인
paprika$outtrn %>% table

paprika %>% filter(frmhsId == "SP10") %>% head(100)
paprika %>% filter(frmhsId == "SP10") %>% tail(100)

paprika$ph[paprika$cunt != 0] %>% table %>% hist() # ph 값분포 확인 

paprika %>% filter(ph >= 11) # ph 55는 이상치로 분류하고 제외
paprika = paprika %>% filter(ph < 50)


paprika[paprika$cunt != 0 & paprika$ph != 0 , ]
paprika[paprika$cunt == 0 & paprika$ph != 0 , ]

paprika2 = paprika %>% 
              group_by(frmhsId, frmDate, frmMonth_x, frmHours) %>% 
              summarise(max_inTp = max(inTp, na.rm = T),
                        min_inTp = min(inTp, na.rm = T),
                        avg_inTp = mean(inTp, na.rm = T),
                        max_inHd = max(inHd, na.rm = T),
                        min_inHd = min(inHd, na.rm = T),
                        avg_inHd = mean(inHd, na.rm = T),
                        max_outTp = max(outTp, na.rm = T),
                        min_outTp = min(outTp, na.rm = T),
                        avg_outTp = mean(outTp, na.rm = T),
                        max_outWs = max(outWs, na.rm = T),
                        min_outWs = min(outWs, na.rm = T),
                        avg_outWs = mean(outWs, na.rm = T),
                        max_inCo2 = max(inCo2, na.rm = T),
                        min_inCo2 = min(inCo2, na.rm = T),
                        avg_inCo2 = mean(inCo2, na.rm = T),
                        sum_cunt = sum(cunt, na.rm = T), # 급액 횟수 총합
                        sum_daysuplyqy = sum(daysuplyqy, na.rm = T), # 급액 량 총합
                        avg_otmsuplyqy = mean(otmsuplyqy, na.rm = T),
                        max_ph = max(ph, na.rm = T),
                        min_ph = min(ph, na.rm = T),
                        avg_ph = mean(ph, na.rm = T),
                        max_ec = max(ec, na.rm = T),
                        min_ec = min(ec, na.rm = T),
                        avg_ec = mean(ec, na.rm = T),
                        avg_outtrn = mean(outtrn, na.rm = T)) %>% as.data.frame() # 급액 ph 값




# inf, nan -> NA
for(colname in colnames(paprika2)) {
  try({
    paprika2[is.infinite(paprika2[,colname]),][,colname] = NA
  })
  try({
    paprika2[is.nan(paprika2[,colname]),][,colname] = NA
  })
}

# 농가 별 측정일시
paprika2 %>% 
  group_by(frmhsId, frmHours) %>% 
  summarise(n = n()) %>% 
  as.data.frame() 




library(dplyr)
library(data.table)
library(plyr)
library(xgboost)
library(car)
library(caret)

data = paprika2
data$frmDate = NULL

# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]

set.seed(123)
proportion = 0.7
idx = sample(1:nrow(data2), size = round(proportion * nrow(data2)))
train = data2[idx, ]
test = data2[-idx, ]

id = test$index

y_train = train$avg_outtrn

#features <- colnames(train)[2:ncol(train)]

set.seed(123)

#######################################################################
############################ XGB ###################################### 
#######################################################################



trainSparse = xgb.DMatrix(data.matrix(train[,-c(1,length(train))]), label=ylabels, missing=NA)
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
                 nrounds=300,
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
                  nround = 300, # 70
                  subsample = 1,
                  colsample_bytree = 0.5,
                  eval_metric = 'rmse',
                  verbose = 1)

# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)

# 도식화 -> top 10
xgb.plot.importance(xgb_imp, top_n = 10)

xgb_prob = data.frame(id = id, real = test$avg_outtrn, pred = predict(mod.xgb,testSparse))






#######################################################################
############################ LGB ###################################### 
#######################################################################

library(Matrix)
library(lightgbm)

train_sparse = Matrix(as.matrix(train[,-c(1,length(train))]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(test[,-c(1,length(test))]), sparse=TRUE)

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)
lgb.param = list(boosting_type = "gbdt",
                 metric = "rmse",
                 sub_feature = 0.9,
                 bagging_fraction = 0.8,
                 bagging_freq = 1, 
                 min_split_gain = 0.01,
                 # min_data = 50,
                 reg_alpha = 0.1,
                 reg_lambda = 0.1)
#min_hessian = 0.001)

lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

# lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 100,
#                       num_threads = 15 , nrounds = 1000, early_stopping_rounds = 50,
#                       eval_freq = 20, eval = lgb.normalizedgini, nfold = 20, stratified = TRUE)
# 
# best.iter = lgb.model.cv$best_iter
# best.iter



################

best.iter = 300

# Train final model
mod.lgb = lgb.train(params = lgb.param, data = lgb.train, learning_rate = 0.03,
                    num_leaves = 60, num_threads = 30 , nrounds = best.iter,
                    eval_freq = 10, eval = lgb.normalizedgini)

################



lgb_prob = data.frame(id = id, real = test$avg_outtrn, pred = predict(mod.lgb,testSparse))






#######################################################################
############################ RF ####################################### 
#######################################################################


#######################################################################
############################ GBM ###################################### 
#######################################################################




