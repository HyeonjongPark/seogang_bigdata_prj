
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
public_train4


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



