
library(data.table)
library(tidyr)
library(highcharter)
library(quantmod)
library(lubridate)

URL = "http://apis.data.go.kr/1390000/SmartFarmdata/envdatarqst"
searchFrmhsCode = c("S17", "S21", "S23", "S26", "S29", "S31", "S32", "S33")
pageSize = 10000 # 유동적으로



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

