


# 1. public_data : public leaderboard용 데이터
# 
# 1-1. train.csv : 베이스라인 코드용으로 가공된 학습 데이터
# 
# date: 일자
# 요일: 요일
# 품목_거래량(kg): 해당 품목의 거래량
# 품목_가격(원/kg): 해당 품목의 kg당 가격
# 품목_가격 산출 방식 : 품목 또는 품종의 총 거래금액/총 거래량 (※취소된 거래내역 제외)
# 1-2. test_files : 베이스라인 코드용으로 가공된 테스트 데이터(추론일자별 분리, ex.test_2020-08-29.csv => 2020년 8월 29일 추론에 사용 가능 데이터)
# 
# 1-3. train_AT_TSALET_ALL : 학습용 전국 도매시장 거래정보 데이터(train.csv 생성에 사용)
# 
# SALEDATE: 경락 일자
# WHSAL_NM: 도매시장
# CMP_NM: 법인
# PUM_NM: 품목
# KIND_NM: 품종
# DAN_NM: 단위
# POJ_NM: 포장
# SIZE_NM: 크기
# LV_NM: 등급
# SAN_NM: 산지
# DANQ: 단위중량
# QTY: 물량
# COST: 단가
# TOT_QTY: 총물량 (음수로 집계된 값은 거래 취소 내역)
# TOT_AMT: 총금액

# 1-4. test_AT_TSALET_ALL : 추론용 전국 도매시장 거래정보 데이터(test_files 생성에 사용)

rm(list = ls())

## 
library(data.table)
library(tidyr)
library(highcharter)
library(quantmod)
library(lubridate)

public_train = fread("./data/datazip/public_data/train.csv", encoding = "UTF-8") %>% as.data.frame()

public_train2 = gather(data = public_train, 
                       key = "qty_price", value = "value",
                       colnames(public_train)[3:length(colnames(public_train))])

public_train3 = separate(data = public_train2,
                         col = qty_price,
                         sep = "_",
                         into = c("vegetable", "flag"))

public_train4 = spread(public_train3, 
                       key = "flag",
                       value = "value") %>% arrange(vegetable, date)


colnames(public_train4) = c("date", "day", "vegetable", "price", "qty")


# value 가 0인 값 -> 결측치로 대체
public_train4 %>% filter(qty == 0, price != 0)
public_train4 %>% filter(price == 0, qty != 0)

public_train4$qty[public_train4$qty == 0] = NA
public_train4$price[public_train4$price == 0] = NA


# 농산물 unique
public_train4$vegetable %>% unique

# 전체 농산물 월 단위 집계
hc_df_month = public_train4 %>% mutate(yearmon = format(as.Date(as.character(date),format="%Y-%m-%d"),"%Y-%m")) %>% 
                                group_by(yearmon) %>% 
                                summarise(avg_price = mean(price, na.rm = TRUE),
                                          sum_qty = sum(qty, na.rm = TRUE))

hc_df_month2 <- xts(x = hc_df_month[,c(2,3)], order.by = sort(as.Date(paste(hc_df_month$yearmon,"-01",sep=""))))
hc_df_month2
trend = highchart(type = "stock") %>%
  hc_add_series(hc_df_month2[, "avg_price"], yAxis = 0, showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
  hc_add_series(hc_df_month2[, "sum_qty"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 2L, title = list(text = "qty"), relative = 1) %>% 
  hc_title(
    text = "Vegetable's Price and qty trend - <b> All </b>",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )

trend



# 전체 농산물 요일 단위 집계
hc_df_day = public_train4 %>%  
  group_by(day) %>% 
  summarise(avg_price = mean(price, na.rm = TRUE),
            sum_qty = sum(qty, na.rm = TRUE))

hc_df_day$day = factor(hc_df_day$day, levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
hc_df_day = hc_df_day %>% arrange(day)

highchart() %>% 
  hc_xAxis(categories = hc_df_day$day) %>% 
  hc_add_series(name = "avg_price", data = hc_df_day$avg_price) %>% 
  hc_add_series(name = "sum_qty", data = hc_df_day$sum_qty/1000000) %>% 
  hc_title(
    text = paste0("Vegetable's Price and qty trend / 요일별 집계(Qty단위 : M) - <b>All</b>"),
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )







library(htmlwidgets)
library(webshot)
#webshot::install_phantomjs()
setwd("C:/Users/guswh/Desktop/data-analysis/seogang_bigdata_prj/eda/hchart/")


# all
htmlwidgets::saveWidget(widget = trend, file = "all.html")
# webshot("all.html", "all.png", delay = 5)


# html 파일 추출
count = 1
for(veg in unique(public_train4$vegetable)) {
  hc_df_month = public_train4 %>% 
    filter(vegetable == veg) %>% 
    mutate(yearmon = format(as.Date(as.character(date),format="%Y-%m-%d"),"%Y-%m")) %>% 
    group_by(yearmon) %>% 
    summarise(avg_price = mean(price, na.rm = TRUE),
              sum_qty = sum(qty, na.rm = TRUE))
  
  hc_df_month2 <- xts(x = hc_df_month[,c(2,3)], order.by = sort(as.Date(paste(hc_df_month$yearmon,"-01",sep=""))))
  trend = highchart(type = "stock") %>%
    hc_add_series(hc_df_month2[, "avg_price"], yAxis = 0, showInLegend = FALSE) %>%
    hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
    hc_add_series(hc_df_month2[, "sum_qty"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
    hc_add_yAxis(nid = 2L, title = list(text = "qty"), relative = 1) %>% 
    hc_title(
      text = paste0("Vegetable's Price and qty trend - <b>",veg,"</b>"),
      margin = 20,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    )
  
  htmlwidgets::saveWidget(widget = trend, file = paste0("vegetable_",count,".html"))
  Sys.sleep(1)
  count = count + 1
  #webshot("all.html", "all.png")
  
}


# html -> png
html_files = dir(getwd())
for(file in html_files) {
  png_file = gsub(".html",".png",file)
  webshot(file, png_file, delay = 3)
}





# 일 단위 집계
hc_df_day = public_train4 %>% 
  group_by(date) %>% 
  summarise(avg_price = mean(price, na.rm = TRUE),
            sum_qty = sum(qty, na.rm = TRUE))

hc_df_day2 <- xts(x = hc_df_day[,c(2,3)], order.by = as.Date(hc_df_day$date))
highchart(type = "chart") %>%
  hc_add_series(hc_df_day2, yAxis = 0, showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
  hc_add_series(hc_df_day2[, "sum_qty"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 1)





# all dataset

# QTY: 물량
# COST: 단가
# TOT_QTY: 총물량 (음수로 집계된 값은 거래 취소 내역)
# TOT_AMT: 총금액

setwd("C:/Users/guswh/Desktop/data-analysis/seogang_bigdata_prj/")
AT_TSALET_ALL = fread("./data/datazip/public_data/train_AT_TSALET_ALL/AT_TSALET_ALL_201601.csv", encoding = "UTF-8") %>% as.data.frame()
AT_TSALET_ALL = AT_TSALET_ALL %>% mutate(raw_cost = QTY/DANQ) 
AT_TSALET_ALL2 = separate(AT_TSALET_ALL, col = "SAN_NM", into = c("do", "si"), sep = " ")

AT_TSALET_ALL2 %>% head

AT_TSALET_ALL %>% 
  group_by(SAN_NM, PUM_NM) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

AT_TSALET_ALL2 %>% 
  group_by(do) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

AT_TSALET_ALL %>% 
  group_by(PUM_NM) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))




tt = AT_TSALET_ALL %>% 
  group_by(SALEDATE, 
           WHSAL_NM, 
           CMP_NM, 
           PUM_NM, 
           KIND_NM, 
           DAN_NM,
           POJ_NM,
           SIZE_NM,
           LV_NM,
           SAN_NM) %>% 
  summarise(TOT_QTY = sum(TOT_QTY, na.rm = TRUE),
            TOT_AMT = sum(TOT_AMT, na.rm = TRUE),
            raw_cost = mean(raw_cost, na.rm = TRUE))

tt









usethis::edit_r_environ()

myKey = Sys.getenv('DATAGOKR_TOKEN')

library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(XML)
library(RCurl)
#install.packages("RCurl")

# http://openapi.customs.go.kr/openapi/service/newTradestatistics/gettradeList?searchBgnDe=201601&searchEndDe=201601&pageNo=1&numOfRows=10&serviceKey=서비스키
# http://apis.data.go.kr/1390000/SmartFarmdata/
# envdatarqst?serviceKey=%ED%95%9C%EA%B8%80&searchFrmhsCode=TestFarm01&searchMeasDt=2019010100&returnType=xml


# http://apis.data.go.kr/1390000/SmartFarmdata/envdatarqst?serviceKey=%ED%95%9C%EA%B8%80&searchFrmhsCode=TestFarm01&searchMeasDt=2019010100&returnType=xml

# 상세 기능 1
URL = "http://apis.data.go.kr/1390000/SmartFarmdata/envdatarqst"
searchFrmhsCode = c("S17", "S21", "S23", "S26", "S29", "S31", "S32", "S33")
pageSize = 10000



urllist <- list()
cnt <-0



for(i in 1:length(searchFrmhsCode)){
  cnt = cnt + 1
  urllist[cnt] = paste0(URL,"?serviceKey=",myKey,"&searchFrmhsCode=",searchFrmhsCode[i],"&pageSize=",pageSize,"&returnType=xml")
}


total<-list()

for(i in 1:length(urllist)){
  
  item <- list()
  item_temp_dt<-data.table()
  
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
  
  size <- xmlSize(items)
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    item_temp_dt = item_temp  %>% t() %>% as.data.frame()
    
    item[[j]]<-item_temp_dt
  }
  
  total[[i]] <- rbindlist(item)
  print(i)
}


total2 <- rbindlist(total)

fwrite(total2, "./data/data_api/envdatarqst_strawberry.csv")






# 상세 기능 2
URL = "http://apis.data.go.kr/1390000/SmartFarmdata/prddatarqst"
searchFrmhsCode = c("S17", "S21", "S23", "S26", "S29", "S31", "S32", "S33")
pageSize = 10000



urllist <- list()
cnt <-0



for(i in 1:length(searchFrmhsCode)){
  cnt = cnt + 1
  urllist[cnt] = paste0(URL,"?serviceKey=",myKey,"&searchFrmhsCode=",searchFrmhsCode[i],"&pageSize=",pageSize,"&returnType=xml")
}


total<-list()
i=1
for(i in 1:length(urllist)){
  
  item <- list()
  item_temp_dt<-data.table()
  
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
  
  size <- xmlSize(items)
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    item_temp_dt = item_temp  %>% t() %>% as.data.frame()
    
    item[[j]]<-item_temp_dt
  }
  
  total[[i]] <- rbindlist(item)
  print(i)
}


total2 <- rbindlist(total)

fwrite(total2, "./data/data_api/grwdatarqst_strawberry.csv")





# urllist = paste0(URL,"?serviceKey=",myKey,"&searchFrmhsCode=",searchFrmhsCode,"&pageSize=",pageSize,"&returnType=xml")
# # xml file 
# raw.data <- xmlTreeParse(urllist, useInternalNodes = TRUE, encoding = "utf-8")
# 
# rootNode <- xmlRoot(raw.data)
# items <- rootNode[[2]][['items']]
# 
# item <- list()
# item_temp_dt<-data.table()
# 
# 
# 
# 
# item <- list()
# total<-list()
# size <- xmlSize(items)
# 
# for(j in 1:size){
#   item_temp <- xmlSApply(items[[j]],xmlValue)
#   item_temp_dt = item_temp  %>% t() %>% as.data.frame()
# 
#   item[[j]]<-item_temp_dt
# }
# total <- rbindlist(item)







