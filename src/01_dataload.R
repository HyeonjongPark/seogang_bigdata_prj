
source("./src/00_libs.R")

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






## dacon data - 노지 데이터 가격, 거래량

dacon_data = fread("./data/datazip/public_data/train.csv", encoding = "UTF-8") %>% as.data.frame()
dacon_data$date = ymd(dacon_data$date)
dacon_data %>% colnames
dacon_data = dacon_data[,c(1,2,27,28)]
colnames(dacon_data) = c("date", "days", "volume", "price")
# 2017-07-31 ~ 
# 2018-11-27 
dacon_data2 = dacon_data %>% filter(date >= "2017-07-31", date <= "2018-11-27")


dacon_data2$week = ifelse(str_length(week(dacon_data2$date)) == 1, 
                      paste0("0",week(dacon_data2$date)),
                      week(dacon_data2$date))

dacon_data2$year = year(dacon_data2$date)

dacon_data3 = dacon_data2 %>% 
  mutate(year_week = paste(year, week, sep = "_")) %>% 
  arrange(year_week)

dacon_data3$price = ifelse(dacon_data3$price == 0, NA, dacon_data3$price)

dacon_data4 = dacon_data3 %>% 
  group_by(year_week) %>% 
  summarise(avg_price = mean(price, na.rm = TRUE),
            avg_volume = mean(volume, na.rm = TRUE)) %>% 
  arrange(year_week)


fwrite(dacon_data4, "./data/data_api/prep/dacon_prep.csv")




