
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


fwrite(paprika2, "./data/data_api/prep/paprika_prep_days.csv", bom = TRUE)















#######################################################
########### 주 단위 처리
#######################################################

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

paprika$fulldate = ymd_hms(paprika$measDtStr)

paprika$week = ifelse(str_length(week(paprika$fulldate)) == 1, 
                      paste0("0",week(paprika$fulldate)),
                      week(paprika$fulldate))


paprika2 = paprika %>% 
  mutate(year_week = paste(frmYear, week, sep = "_")) %>% 
  arrange(year_week)

paprika2 %>% head
paprika2$outtrn = paprika2$outtrn / paprika2$frmAr # 단위 면적당 생산량으로 변경

paprika2 = paprika2 %>% 
  group_by(frmhsId, year_week, frmMonth_x,) %>% 
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
    sum_outtrn = sum(outtrn, na.rm = T)) %>% as.data.frame() # 급액 ph 값

colSums(is.na(paprika2))


# inf, nan -> NA
for(colname in colnames(paprika2)) {
  try({
    paprika2[is.infinite(paprika2[,colname]),][,colname] = NA
  }, silent = TRUE)
  try({
    paprika2[is.nan(paprika2[,colname]),][,colname] = NA
  }, silent = TRUE)
}



# 농가별 index


paprika2 = paprika2 %>% mutate(countDate = NA)
paprika3 = data.frame()
for(farm in unique(paprika2$frmhsId)) {
  temp = paprika2 %>% filter(frmhsId == farm)
  temp$countDate = 1:nrow(temp)
  
  paprika3 = rbind(paprika3, temp)
}

paprika3
paprika3 = paprika3[,c(ncol(paprika3),1:(ncol(paprika3)-1))]


fwrite(paprika3, "./data/data_api/prep/paprika_prep_weeks.csv", bom = TRUE)
