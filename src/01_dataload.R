
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








######################################################
############# weather data 
######################################################

getwd()
weather_list = dir("./data/weather/")

weather = data.frame()
for(weath in weather_list) {
  temp = fread(paste0("./data/weather/", weath))
  weather = rbind(weather , temp)
  print(weath)
}

weather %>% str
weather %>% colnames()
weather %>% head

weather$일시 = ymd_hm(weather$일시)
weather %>% head


weather$year = year(weather$일시)
weather$week = week(weather$일시)

weather$week = ifelse(str_length(week(weather$일시)) == 1, 
                          paste0("0",week(weather$일시)),
                          week(weather$일시))

weather2 = weather %>% 
  mutate(year_week = paste(year, week, sep = "_")) %>% 
  arrange(year_week)


weather2 %>% head
weather2 %>% colnames
weather3 = weather2[,c("지점", "지점명" ,"year", "year_week", "기온(°C)", "강수량(mm)", "풍속(m/s)", "습도(%)",
                       "이슬점온도(°C)", "현지기압(hPa)", "해면기압(hPa)",
                       "적설(cm)", "5cm 지중온도(°C)", "10cm 지중온도(°C)", 
                       "20cm 지중온도(°C)", "30cm 지중온도(°C)")]

colnames(weather3) = c("site_code","site", "year", "year_week", "temp", "rain", "wind", "humi", 
                       "dew_temp","atmo_local_pres", "atmo_sealevel_pres",
                       "snow", "temp_5cm", "temp_10cm", "temp_20cm", "temp_30cm")

weather3%>% head
#weather3 = weather3 %>% as.data.frame()
weather4 = weather3 %>% 
  group_by(site_code, site, year, year_week) %>% 
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rain = mean(rain, na.rm = TRUE),
            avg_wind = mean(wind, na.rm = TRUE),
            avg_humi = mean(humi, na.rm = TRUE),
            avg_dew_temp = mean(dew_temp, na.rm = TRUE),
            avg_atmo_local_pres = mean(atmo_local_pres, na.rm = TRUE),
            avg_atmo_sealevel_pres = mean(atmo_sealevel_pres, na.rm = TRUE),
            avg_temp_5cm = mean(temp_5cm, na.rm = TRUE),
            avg_temp_10cm = mean(temp_10cm, na.rm = TRUE),
            avg_temp_20cm = mean(temp_20cm, na.rm = TRUE),
            avg_temp_30cm = mean(temp_30cm, na.rm = TRUE))
weather4 %>% as.data.frame() %>% head
weather4 %>% dim
weather4$site %>% unique
weather4 %>% head
weather4$site_code %>% unique

fwrite(weather4, "./data/prep/weather_1521.csv")





######################################################
############# weather data  farm
######################################################


getwd()
weatherfarm_list = dir("./data/weatherfarm/")

weatherfarm = data.frame()
for(weath in weatherfarm_list) {
  temp = fread(paste0("./data/weatherfarm/", weath))
  weatherfarm = rbind(weatherfarm , temp)
  print(weath)
}

weatherfarm %>% str
weatherfarm %>% colnames()
weatherfarm %>% head
weatherfarm %>% dim

weatherfarm$일시 = ymd_hm(weatherfarm$일시)
weatherfarm %>% head


weatherfarm$year = year(weatherfarm$일시)
weatherfarm$week = week(weatherfarm$일시)
weatherfarm$week = ifelse(str_length(week(weatherfarm$일시)) == 1, 
                      paste0("0",week(weatherfarm$일시)),
                      week(weatherfarm$일시))

weatherfarm2 = weatherfarm %>% 
  mutate(year_week = paste(year, week, sep = "_")) %>% 
  arrange(year_week)


colnames(weatherfarm2)[1:2] = c("site_code","site")
colnames(weatherfarm2)[4:30] = gsub(" ","_",colnames(weatherfarm2)[4:30])

weatherfarm2$week = NULL
weatherfarm2$일시 = NULL


weatherfarm3 = weatherfarm2 %>% group_by(site_code, site, year, year_week) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 


weatherfarm3 %>% dim
weatherfarm3 %>% as.data.frame() %>% head
weatherfarm3$site %>% unique
# [1] "수원"       "서산"       "익산"       "철원장흥"   "안동옥동"   "화순능주"   "대곡"       "오창가곡"   "춘천신북"  
# [10] "강정"       "보성군(농)"

# 가정
# 철원장흥 -> 강원도(970)
# 익산 -> 전남 (702)


weatherfarm4 = weatherfarm3 %>% filter(site %in% c("철원장흥", "익산"))
weatherfarm4 %>% tail
weatherfarm4$site = NULL

fwrite(weatherfarm4, "./data/weatherfarm/prep/weatherfarm4.csv")


