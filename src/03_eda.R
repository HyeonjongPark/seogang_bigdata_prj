# 
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

data = fread("./data/data_api/prep/paprika_prep.csv") %>% as.data.frame()


data %>% 
  group_by(frmhsId) 





