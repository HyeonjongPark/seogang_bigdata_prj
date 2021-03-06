rm(list = ls())
# frmhsId : 농가코드
# measDtStr : 측정일시
# inTp :내부온도
# outTp : 외부온도
# inHd : 내부습도
# inCo2 : 내부Co2
# outWs : 풍속
# acSlrdQy : 누적일사량
# ec : 급액 EC
# ph : 급액 pH
# cunt : 일 급액횟수
# daysuplyqy : 일 급액량(cc/1주수)
# otmsuplyqy : 1회 급액량(cc/1주수)
# outtrn : 생산량

source("./src/00_libs.R")


#######################################################
########### 일 단위 처리
#######################################################

paprika = fread("./data/raw/openapi/paprika.csv", encoding = "UTF-8") %>% as.data.frame() %>% arrange(measDtStr)
paprika %>% dim
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
  summarise(#max_inTp = max(inTp, na.rm = T),
    #min_inTp = min(inTp, na.rm = T),
    avg_inTp = mean(inTp, na.rm = T),
    #max_inHd = max(inHd, na.rm = T),
    #min_inHd = min(inHd, na.rm = T),
    avg_inHd = mean(inHd, na.rm = T),
    #max_outTp = max(outTp, na.rm = T),
    #min_outTp = min(outTp, na.rm = T),
    avg_outTp = mean(outTp, na.rm = T),
    #max_outWs = max(outWs, na.rm = T),
    #min_outWs = min(outWs, na.rm = T),
    avg_outWs = mean(outWs, na.rm = T),
    #max_inCo2 = max(inCo2, na.rm = T),
    #min_inCo2 = min(inCo2, na.rm = T),
    avg_inCo2 = mean(inCo2, na.rm = T),
    sum_cunt = sum(cunt, na.rm = T), # 급액 횟수 총합
    sum_daysuplyqy = sum(daysuplyqy, na.rm = T), # 급액 량 총합
    avg_otmsuplyqy = mean(otmsuplyqy, na.rm = T),
    #max_ph = max(ph, na.rm = T),
    #min_ph = min(ph, na.rm = T),
    avg_ph = mean(ph, na.rm = T),
    #max_ec = max(ec, na.rm = T),
    #min_ec = min(ec, na.rm = T),
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


fwrite(paprika2, "./data/prep/paprika_prep_days.csv", bom = TRUE)















#######################################################
########### 주 단위 처리 ver1
#######################################################

paprika = fread("./data/raw/openapi/paprika.csv", encoding = "UTF-8") %>% as.data.frame() %>% arrange(measDtStr)

# 데이터 수집 기간
paprika %>% head # 2017-07-31 ~ 
paprika %>% tail # 2018-11-27 
paprika$frmhsId %>% unique %>% length

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


paprika = paprika %>% filter(ph < 50)

# 결측이라 0 인 것은 NA로 대체 필요 
paprika$inTp[paprika$inTp == 0] = NA
paprika$inHd[paprika$inHd == 0] = NA
paprika$outTp[paprika$outTp == 0] = NA
paprika$outWs[paprika$outWs == 0] = NA
paprika$inCo2[paprika$inCo2 == 0] = NA
paprika$ec[paprika$ec == 0] = NA
paprika$ph[paprika$ph == 0] = NA
paprika$otmsuplyqy[paprika$otmsuplyqy == 0] = NA

paprika$frmhsId %>% unique %>% length

# Y인자 확인
paprika$outtrn %>% table

paprika %>% filter(frmhsId == "SP10") %>% head(100)
paprika %>% filter(frmhsId == "SP10") %>% tail(100)

paprika$ph[paprika$cunt != 0] %>% table %>% hist() # ph 값분포 확인 

paprika %>% filter(ph >= 11) # ph 55는 이상치로 분류하고 제외



paprika[paprika$cunt != 0 & paprika$ph != 0 , ]
paprika[paprika$cunt == 0 & paprika$ph != 0 , ]

paprika$fulldate = ymd_hms(paprika$measDtStr)

paprika$week = ifelse(str_length(week(paprika$fulldate)) == 1, 
                      paste0("0",week(paprika$fulldate)),
                      week(paprika$fulldate))

paprika2 = paprika %>% 
  mutate(year_week = paste(frmYear, week, sep = "_")) %>% 
  arrange(year_week)

paprika2 %>% head
#paprika2$outtrn = paprika2$outtrn / paprika2$frmAr # 단위 면적당 생산량으로 변경

paprika2 = paprika2 %>% 
  group_by(frmhsId, year_week) %>% 
  summarise(avg_inTp = mean(inTp, na.rm = T),
    avg_inHd = mean(inHd, na.rm = T),
    avg_outTp = mean(outTp, na.rm = T),
    avg_outWs = mean(outWs, na.rm = T),
    avg_inCo2 = mean(inCo2, na.rm = T),
    sum_cunt = sum(cunt, na.rm = T), # 급액 횟수 총합
    sum_daysuplyqy = sum(daysuplyqy, na.rm = T), # 급액 량 총합
    avg_otmsuplyqy = mean(otmsuplyqy, na.rm = T),
    avg_ph = mean(ph, na.rm = T),
    avg_ec = mean(ec, na.rm = T),
    avg_outtrn = mean(outtrn, na.rm = T)) %>% as.data.frame() # 급액 ph 값

colSums(is.na(paprika2))
paprika2$frmhsId %>% unique %>% length

# inf, nan -> NA
for(colname in colnames(paprika2)) {
  try({
    paprika2[is.infinite(paprika2[,colname]),][,colname] = NA
  }, silent = TRUE)
  try({
    paprika2[is.nan(paprika2[,colname]),][,colname] = NA
  }, silent = TRUE)
}


paprika2 %>% head
# 농가별 index
paprika2 = paprika2 %>% mutate(countDate = NA)
paprika3 = data.frame()
for(farm in unique(paprika2$frmhsId)) {
  temp = paprika2 %>% filter(frmhsId == farm)
  temp$countDate = 1:nrow(temp)
  
  paprika3 = rbind(paprika3, temp)
}

paprika3 %>% head
paprika3 = paprika3[,c(ncol(paprika3),1:(ncol(paprika3)-1))]

paprika3 %>% head

fwrite(paprika3, "./data/prep/paprika_prep_weeks1.csv")


con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks1", paprika3, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)











########################
### dacon data 결합 #### ver2
########################

dacon_data4 = fread("./data/prep/dacon_prep.csv")

paprika4 = left_join(paprika3, dacon_data4)

fwrite(paprika4, "./data/prep/paprika_prep_weeks2.csv", bom = TRUE)

con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks2", paprika4, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)





########################
### dacon data 결합 + lead, lag  #### ver3
########################


data = fread("./data/prep/paprika_prep_weeks2.csv")%>% as.data.frame()

data$year_week = NULL
# 더미 변수를 이용해 원 핫 인코딩
dmy <- dummyVars(~., data = data)
data2 <- data.frame(predict(dmy, newdata = data))

data2 %>% head
data2$avg_price_lead1 = lead(data2$avg_price, 1) 
data2$avg_price_lead2 = lead(data2$avg_price, 2) 
data2$avg_price_lead3 = lead(data2$avg_price, 3) 
data2$avg_price_lag1 = lag(data2$avg_price, 1) 
data2$avg_price_lag2 = lag(data2$avg_price, 2) 
data2$avg_price_lag3 = lag(data2$avg_price, 3) 

data2$avg_volume_lead1 = lead(data2$avg_volume, 1) 
data2$avg_volume_lead2 = lead(data2$avg_volume, 2) 
data2$avg_volume_lead3 = lead(data2$avg_volume, 3) 
data2$avg_volume_lag1 = lag(data2$avg_volume, 1) 
data2$avg_volume_lag2 = lag(data2$avg_volume, 2) 
data2$avg_volume_lag3 = lag(data2$avg_volume, 3) 

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

fwrite(data2, "./data/prep/paprika_prep_weeks3.csv", bom = TRUE)


con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks3", data2, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)




########################
### dacon data 결합 + lead, lag  + 농업 기후데이터  #### ver4
########################

data = fread("./data/prep/paprika_prep_weeks2.csv")%>% as.data.frame()
data$site_code = NA
data$site_code = ifelse(data$frmhsId %in% c("WP18","WP25","WP26","WP27","WP28","WP29","WP30","WP33","WP36"),  702, 970)

data %>% str
weatherfarm4 = fread("./data/prep/weatherfarm4.csv")
weatherfarm4 %>% str
weatherfarm4 %>% colnames()
data = left_join(data, weatherfarm4, by = c("year_week" = "year_week",
                                            "site_code" = "site_code"))

colSums(is.na(data))

data$year_week = NULL
data$site_code = NULL
data$frmhsId = NULL
data %>% head


# 더미 변수를 이용해 원 핫 인코딩 - > 제외
# dmy <- dummyVars(~., data = data)
# data2 <- data.frame(predict(dmy, newdata = data))
data2 = data
data2 %>% head
data2$avg_price_lead1 = lead(data2$avg_price, 1) 
data2$avg_price_lead2 = lead(data2$avg_price, 2) 
data2$avg_price_lead3 = lead(data2$avg_price, 3) 
data2$avg_price_lag1 = lag(data2$avg_price, 1) 
data2$avg_price_lag2 = lag(data2$avg_price, 2) 
data2$avg_price_lag3 = lag(data2$avg_price, 3) 

data2$avg_volume_lead1 = lead(data2$avg_volume, 1) 
data2$avg_volume_lead2 = lead(data2$avg_volume, 2) 
data2$avg_volume_lead3 = lead(data2$avg_volume, 3) 
data2$avg_volume_lag1 = lag(data2$avg_volume, 1) 
data2$avg_volume_lag2 = lag(data2$avg_volume, 2) 
data2$avg_volume_lag3 = lag(data2$avg_volume, 3) 

data2 = data2[-c(1:3),]


y_value = data2$avg_outtrn
data2$avg_outtrn = NULL

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2$avg_outtrn = y_value

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인


colSums(is.na(data2))
data2 %>% head
data2 %>% tail

fwrite(data2, "./data/prep/paprika_prep_weeks4.csv", bom = TRUE)

con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks4", data2, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)





########################
### dacon data 결합 + farmNew  #### ver5
########################

data = fread("./data/prep/paprika_prep_weeks2.csv") %>% as.data.frame()
farmNew2 = fread("./data/prep/farmNew.csv")

data %>% arrange(year_week) %>% head
farmNew2 %>% head

data %>% arrange(year_week) %>% tail
farmNew2 %>% tail


data2 = left_join(data, farmNew2)

data2$frmhsId = NULL

y_value = data2$avg_outtrn
data2$avg_outtrn = NULL

data2$index = 1:nrow(data2)
data2 = data2[,c(ncol(data2),1:(ncol(data2)-1))]
data2$avg_outtrn = y_value



fwrite(data2, "./data/prep/paprika_prep_weeks5.csv", bom = TRUE)

con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks5", data2, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)






########################
### dacon data 결합 + lead, lag  + 농업 기후데이터 + farmNew  #### ver6
########################
farmNew2 = fread("./data/prep/farmNew.csv")


data = fread("./data/prep/paprika_prep_weeks2.csv")%>% as.data.frame()
data$site_code = NA
data$site_code = ifelse(data$frmhsId %in% c("WP18","WP25","WP26","WP27","WP28","WP29","WP30","WP33","WP36"),  702, 970)

data %>% str
weatherfarm4 = fread("./data/prep/weatherfarm4.csv")
weatherfarm4 %>% str
weatherfarm4 %>% colnames()
data = left_join(data, weatherfarm4, by = c("year_week" = "year_week",
                                            "site_code" = "site_code"))

colSums(is.na(data))


farmNew2 = as.data.frame(farmNew2)

data2 = left_join(data, farmNew2)

data2$frmhsId = NULL

y_value = data2$avg_outtrn
data2$avg_outtrn = NULL

data2$index = 1:nrow(data2)
data2$avg_outtrn = y_value
data2 %>% head




data2$year_week = NULL
data2$site_code = NULL
data2$frmhsId = NULL
data2 %>% head


# 더미 변수를 이용해 원 핫 인코딩 - > 제외
# dmy <- dummyVars(~., data = data)
# data2 <- data.frame(predict(dmy, newdata = data))
data2 %>% head
data2$avg_price_lead1 = lead(data2$avg_price, 1) 
data2$avg_price_lead2 = lead(data2$avg_price, 2) 
data2$avg_price_lead3 = lead(data2$avg_price, 3) 
data2$avg_price_lag1 = lag(data2$avg_price, 1) 
data2$avg_price_lag2 = lag(data2$avg_price, 2) 
data2$avg_price_lag3 = lag(data2$avg_price, 3) 

data2$avg_volume_lead1 = lead(data2$avg_volume, 1) 
data2$avg_volume_lead2 = lead(data2$avg_volume, 2) 
data2$avg_volume_lead3 = lead(data2$avg_volume, 3) 
data2$avg_volume_lag1 = lag(data2$avg_volume, 1) 
data2$avg_volume_lag2 = lag(data2$avg_volume, 2) 
data2$avg_volume_lag3 = lag(data2$avg_volume, 3) 

data2 = data2[-c(1:3),]







y_value = data2$avg_outtrn
data2$avg_outtrn = NULL

data2$index = 1:nrow(data2)
data2$avg_outtrn = y_value

data2 %>% head
dim(data2)
colnames(data2[colSums(is.na(data2))/nrow(data2) * 100 >= 50]) # 결측이 50% 이상 컬럼 확인


colSums(is.na(data2))
data2 %>% head
data2 %>% tail
fwrite(data2, "./data/prep/paprika_prep_weeks6.csv", bom = TRUE)

con <- dbConnect(RMariaDB::MariaDB(), username="root", password = as.character(pwd[1,]), dbname ="sinto", host="localhost")
dbWriteTable(con, "paprika_prep_weeks6", data2, overwrite = TRUE, fileEncoding="UTF-8") # db 내에 데이터 넣기
dbDisconnect(con)







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

data2_1 = data2_1[,!(colnames(data2_1) %in% c('TP_daytime1','TP_daytime2','HD_daytime1','HD_daytime2','co2_daytime1','co2_daytime2','n_group'))]
colnames(data2_1)

fwrite(data2_1, "./data/prep/train_new2.csv")
