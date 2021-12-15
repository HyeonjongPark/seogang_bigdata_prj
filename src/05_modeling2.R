rm(list = ls())
########################
###  주 단위 + smartfarm 홈페이지 + farmNew ver7
########################

data2 = fread("./data/prep/train_new.csv") %>% as.data.frame()
data2 %>% head
data2

data2 = data2 %>% mutate(countDate = NA)
data2_1 = data.frame()
for(farm in unique(data2$id)) {
  temp = data2 %>% filter(id == farm)
  temp$countDate = 1:nrow(temp)
  
  data2_1 = rbind(data2_1, temp)
}

data2_1 %>%tail
library(data.table)
data2_1 = as.data.table(data2_1)
# group 별 lag => 횟수 i

data2_1$target = lead(data2_1$target, 2)

# for(i in 1:3) {
#   data2_1[, target:=c(NA, target[-.N]), by=id]
# }



data2_1 = data2_1 %>% filter(!is.na(target))
data2_1 = as.data.frame(data2_1)
# 25 countdate 이후에 0인 것 NA 처리
data2_1$target[data2_1$countDate > 30][data2_1$target[data2_1$countDate > 30] == 0] = NA

data2_1 = data2_1 %>% filter(!is.na(target))

data2_1[data2_1 == 0] = NA
data2_1$target[is.na(data2_1$target)] = 0

data2_1 = data2_1[,!(colnames(data2_1) %in% c('TP_daytime1','TP_daytime2','HD_daytime1','HD_daytime2','co2_daytime1','co2_daytime2','n_group', 'length', 'width', 'growth', 'n_leaf', 'new_f_height', 'stem', 'f_group', 'fr_group', 'h_group', 'n_fruit'))]
# 'length', 'width', 'growth', 'n_leaf', 'new_f_height', 'stem', 'f_group', 'fr_group', 'h_group', 'n_fruit'
colnames(data2_1)

fwrite(data2_1, "./data/prep/train_new2.csv")




#############
## 주 단위 + smartfarm 홈페이지 + farmNew ver7

data2 = fread("./data/prep/train_new2.csv") %>% as.data.frame()
data2 %>% head
colSums(is.na(data2))

data2$id = as.character(data2$id)
data2 %>% str

## 더미 변수화
# dmy <- dummyVars(~., data = data2)
# data2 <- data.frame(predict(dmy, newdata = data2))

data2$index = 1:nrow(data2)

# 컬럼 위치 변경
avg_outtrn = data2$target
index = data2$index

data2$target = NULL
data2$index = NULL

data2 = cbind(index, data2, avg_outtrn)

data2$id = NULL

data2 %>% dim
colSums(is.na(data2))
############

data2 = rfImpute(avg_outtrn ~ ., data2[,-1])
data2 = data2[c(2:ncol(data2),1)]


mod.lm1 = lm(avg_outtrn ~., data = data2[,-1])
summary(mod.lm1)
data2 %>% dim # 60개


# am : 6 ~ 12
# pm : 12 ~ 17
# sun : 17 ~ 19
# eve : 19 ~ 22
# nig : 22 ~ 02
# daw : 02 ~ 06

cor(data2)


#data2 = na.omit(data2)

mod.lm2 = step(mod.lm1, direction = "both")
summary(mod.lm2)

#fwrite(data2, "./data/prep/final_train_new2.csv")

#c("index", "growth", "width", "new_f_height", "f_group", "fr_group", "n_fruit", "tp_all", "TP_am", "TP_pm", "TP_sunset", "TP_evening", "TP_night", "HD_all", "HD_sunset", "HD_night", "HD_dawn", "co2_all", "co2_am", "co2_pm", "co2_evening", "co2_night", "sol", "d_num", "p_water", "p_water_day", "volume_sum", "price_std", "avg_outtrn")

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
train$avg_outtrn = NULL
train$avg_outtrn = y_train

y_test = test$avg_outtrn
test$avg_outtrn = NULL
test$avg_outtrn = y_test





#236
train = as.data.frame(train)
test = as.data.frame(test)

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



# xgb_cv <- xgb.cv(data=trainSparse,
#                  params=param.xgb,
#                  nrounds=100,
#                  prediction=TRUE,
#                  maximize=TRUE,
#                  folds=foldsCV,
#                  #early_stopping_rounds = 50,
#                  print_every_n = 5
# )

mod.xgb = xgboost(data = trainSparse, nrounds = 300)
# mod.xgb = xgboost(data = trainSparse,
#                   eta = 0.09,
#                   nfold = 5, 
#                   max_depth = 10, 
#                   min_child_weight = 1.2,
#                   gamma = 0,
#                   nround = 300, 
#                   subsample = 1,
#                   colsample_bytree = 0.5,
#                   eval_metric = 'rmse',
#                   verbose = 1)


# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)
xgb.ggplot.importance(importance_matrix = xgb_imp[1:20])


pred.xgb = predict(mod.xgb,testSparse)
pred.xgb = ifelse(pred.xgb < 0, 0, pred.xgb)

out.xgb = data.frame(id = id, 
                     real = test$avg_outtrn, 
                     pred = pred.xgb)


forecast::accuracy(out.xgb$real, out.xgb$pred+1)







# data2 = na.omit(data2)
# mod.lm1 = lm(avg_outtrn ~., data = data2[,-1])
# summary(mod.lm1)
# mod.lm2 = step(mod.lm1, direction = "both")
# summary(mod.lm2)
