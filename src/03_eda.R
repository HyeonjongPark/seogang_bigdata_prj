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
source("./src/99_function.R")

data = fread("./data/data_api/prep/paprika_prep.csv") %>% as.data.frame()


###########
# hchart
###########
setwd("C:/Users/guswh/Desktop/data-analysis/seogang_bigdata_prj/eda/hchart")

h1 = data %>% 
  group_by(frmMonth_x) %>% 
  summarise(avg_outtrn = mean(avg_outtrn, na.rm = TRUE)) %>% 
  hchart("column", hcaes(x = frmMonth_x, y = avg_outtrn)) %>% 
  hc_title(
    text = paste0("<b>월 별 생산량 집계</b>"),
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>% 
  hc_add_theme(hc_theme_darkunica())

htmlwidgets::saveWidget(widget = h1, file = "prod_agg1.html")



h2 = data %>% 
  group_by(frmhsId, frmMonth_x) %>% 
  summarise(avg_outtrn = mean(avg_outtrn, na.rm = TRUE)) %>% 
  hchart("bar", hcaes(x = frmMonth_x, y = avg_outtrn, group = frmhsId)) %>% 
  hc_title(
    text = paste0("<b>월, 농가 별 생산량 집계</b>"),
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>% 
  hc_add_theme(hc_theme_darkunica())
# -> # 8~ 10 월에 저조한 생산

htmlwidgets::saveWidget(widget = h2, file = "prod_agg2.html")


h3 = data %>% 
  group_by(frmhsId, frmHours) %>% 
  summarise(avg_outtrn = mean(avg_outtrn, na.rm = TRUE)) %>% 
  hchart("bar", hcaes(x = frmHours, y = avg_outtrn, group = frmhsId)) %>% 
  hc_title(
    text = paste0("<b>시간, 농가 별 생산량 집계</b>"),
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>% 
  hc_add_theme(hc_theme_darkunica())
# -> 농가 별 관측 시간에 따라 다를 수 있음.

htmlwidgets::saveWidget(widget = h3, file = "prod_agg3.html")




###########
# 상관분석
###########

colSums(is.na(data)) # 결측이 많은 avg_outWs, avg_otmsuplyqy 컬럼 누락, 결측 로우 제거후 상관 분석 진행

cor_data = data

cor_data$frmMonth_x = NULL
cor_data$frmHours = NULL
cor_data$frmhsId = NULL
cor_data$avg_outWs = NULL
cor_data$avg_otmsuplyqy = NULL

cor_data %>% dim  # 3086
na.omit(cor_data) %>% dim # 2509

cor_data2 = na.omit(cor_data)

# 산점도 행렬
plot(cor_data2)

# 상관계수 행렬
cor_x <- cor(cor_data2)
corrplot(cor_x, method="number")
corrplot(cor_x, method="ellipse")








## 생산량이 0 인 것을 제외해보고
data %>% filter(avg_outtrn != 0) %>% dim # 2193

na_omit_data = data %>% filter(avg_outtrn != 0)


cor_data = na_omit_data

cor_data$frmMonth_x = NULL
cor_data$frmHours = NULL
cor_data$frmhsId = NULL
cor_data$avg_outWs = NULL
cor_data$avg_otmsuplyqy = NULL

cor_data %>% dim  # 3086
na.omit(cor_data) %>% dim # 2509

cor_data2 = na.omit(cor_data)

# 산점도 행렬
plot(cor_data2)

# 상관계수 행렬
cor_x <- cor(cor_data2)
corrplot(cor_x, method="number")
corrplot(cor_x, method="ellipse")



###########
# DataExplorer - auto eda
###########
data %>% str


data$frmMonth_x = factor(data$frmMonth_x, levels = unique(data$frmMonth_x))
data$frmHours = factor(data$frmHours, levels = sort(unique(data$frmHours)))


plot_histogram(data)
ggsave("./eda/DataExplorer/histogram.png")

plot_bar(data, with = "avg_outtrn")
ggsave("./eda/DataExplorer/plotbar_by_avg_outtrn.png")

plot_bar(data, by = "frmhsId")
ggsave("./eda/DataExplorer/plotbar_by_frmhsId.png")

plot_bar(data, by = "frmMonth_x")
ggsave("./eda/DataExplorer/plotbar_by_frmMonth_x.png")

plot_bar(data, by = "frmHours")
ggsave("./eda/DataExplorer/plotbar_by_frmHours.png")

plot_density(data)
ggsave("./eda/DataExplorer/plotdensity.png")

plot_boxplot(data, by = "frmhsId")
ggsave("./eda/DataExplorer/plotboxplot_by_frmhsId.png")

plot_boxplot(data, by = "frmMonth_x")
ggsave("./eda/DataExplorer/plotboxplot_by_frmMonth_x.png")

plot_boxplot(data, by = "frmHours")
ggsave("./eda/DataExplorer/plotboxplot_by_frmHours.png")

plot_qq(data, by = "frmhsId")
ggsave("./eda/DataExplorer/plotqq_by_frmhsId.png")

plot_qq(data, by = "frmMonth_x")
ggsave("./eda/DataExplorer/plotqq_by_frmMonth_x.png")

plot_qq(data, by = "frmHours")
ggsave("./eda/DataExplorer/plotqq_by_frmHours.png")


plot_prcomp(data, maxcat = 5L)




create_report(data) # report1
create_report(data, y = "avg_outtrn") # report2










