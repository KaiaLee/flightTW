# 載入所需的套件
library(dplyr)
library(ggplot2)

# 合併日期檔案
file1 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240812.CSV", stringsAsFactors = FALSE)
file2 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240813.CSV", stringsAsFactors = FALSE)
file3 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240814.CSV", stringsAsFactors = FALSE)
file4 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240815.CSV", stringsAsFactors = FALSE)
file5 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240816.CSV", stringsAsFactors = FALSE)
file6 <- read.csv("C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\20240817.CSV", stringsAsFactors = FALSE)

data <- rbind(file1, file2, file3, file4, file5, file6) #3204792



# 確認資料集內容
head(data, 5)
summary(data)

airlineID指的是航空公司
flightnumber指的是航班編號

#### 刪除不要的欄位
data <- data %>%
  select(-AirRouteType, -EstimatedDepartureTime, -DepartureRemarkEn, -Terminal, -Gate, -CodeShare, -AcType, -BaggageClaim, -DepartureRemark, -CheckCounter, -UpdateTime)

# 刪除ActualDepartureTime空值欄位
data <- subset(data, ActualDepartureTime != "") # 2195855

# 根據 DepartureAirportID ArrivalAirpotID DepartureAirportID  FlightDeparture來刪除重複行
# 因為在觀察資料時發現聯合航班不同 FightNumber+AirlineID 可能為同一班飛機(DepartureAirportID ArrivalAirpotID DepartureAirportID  FlightDeparture相同)
data <- data %>%
  distinct(DepartureAirportID, ArrivalAirportID, ActualDepartureTime, .keep_all = TRUE) #5092


# 將字串轉換為時間序列格式
data$FlightDate <- as.POSIXct(data$FlightDate, format="%Y-%m-%d")
data$ScheduleDepartureTime <- as.POSIXct(data$ScheduleDepartureTime, format="%Y-%m-%d %H:%M:%S")
data$ActualDepartureTime <- as.POSIXct(data$ActualDepartureTime, format="%Y-%m-%d %H:%M:%S")

# 新增延遲時間欄位
data$DelayMin <- as.numeric(data$ActualDepartureTime - data$ScheduleDepartureTime) / 60
data$DelayHour <- as.numeric(data$ActualDepartureTime - data$ScheduleDepartureTime) / 3600

write.csv(filtered_data, file = "C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\5092.csv", row.names = FALSE)


#### 新增DepartureRemark欄位

#data$DepartureRemark <- NA

#for (i in 1:nrow(data)) {
#  if (data$DelayMin[i] < 0) {
#    data$DepartureRemark[i] <- "提前"
#  } else if (data$DelayMin[i] == 0) {
#    data$DepartureRemark[i] <- "準時"
#  } else {
#    data$DepartureRemark[i] <- "延遲"
#  }
#}



# 計算每個類別的頻數
freq_table <- table(data$DepartureRemark)

# 創建數據框
df <- as.data.frame(freq_table)
names(df) <- c('Status', 'Count')  # 重命名列名稱

# 繪製長條圖
ggplot(df, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Flight Departure Remarks Distribution",
       x = "Departure Remark",
       y = "Number of Flights") +
  theme_minimal()


#### 敘述統計：畫Q4觀察delay的數據
delaydata <- data[data$DelayMin>0,]
barplot(table(delaydata$DelayMin)) #延遲時間集中在50分鐘以內
BP <- boxplot(delaydata$DelayMin, main = "Boxplot of DelayData", ylim = c(0,70))
text(y = BP$stats, labels = BP$stats, x=1, col = "red")



# 计算四分位数
Q1 <- quantile(data$DelayMin, 0.25)
Q3 <- quantile(data$DelayMin, 0.75)

IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# 改成只看延遲部分的绘制四分图(圖太右偏很醜可刪)
delay_median <- median(delaydata$DelayMin, na.rm = TRUE)

ggplot(delaydata, aes(x = DelayMin)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = Q1, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = Q3, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = delay_median, color = "red", linetype = "dashed") +
  annotate("text", x = delay_median, y = Inf, label = "Median", vjust = 1.5, color = "red") +
  labs(title = "Histogram of DelayMin with Quartiles",
       x = "DelayMin",
       y = "Frequency") +
  theme_minimal()

ggplot(delaydata, aes(x = DelayMin)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = Q1, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = Q3, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = delay_median, color = "red", linetype = "dashed") +
  annotate("text", x = Q1, y = Inf, label = "Q1", vjust = 1.5, color = "blue") +
  annotate("text", x = Q3, y = Inf, label = "Q3", vjust = 1.5, color = "blue") +
  annotate("text", x = delay_median, y = Inf, label = "Median", vjust = 1.5, color = "red") +
  labs(title = "Histogram of DelayMin with Quartiles",
       x = "DelayMin",
       y = "Frequency") +
  xlim(0, upper_bound) +  # 設置 X 軸範圍
  theme_minimal()

# 定义每个区间的范围，并将 0 分到 "ontime" 类别
delaydata$DelaySpace <- cut(
  delaydata$DelayHour, 
  breaks = c(0, 1, 4, 8, 12, 100),  # 定义区间边界
  labels = c("<1", "1-4", "4-8", "8-12", ">12"), 
  ordered_result = TRUE  # 设置为有序因子
)


# 計算每個類別的頻數
freq_table2 <- table(delaydata$DelaySpace) #延誤超過4小時可獲得保險公司理賠，僅25筆

# 創建數據框
df2 <- as.data.frame(freq_table2)
names(df2) <- c('Status', 'Count')  # 重命名列名稱

# 繪製長條圖
ggplot(df2, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Flight delay space",
       x = "Delay Hour",
       y = "Number of Flights") +
  theme_minimal()






####取出8/12-8/16班機
# 定義開始和結束日期
start_date <- as.POSIXct("2024-08-12 00:00:00", format="%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2024-08-17 23:59:59", format="%Y-%m-%d %H:%M:%S")

# 過濾指定日期範圍內的數據
filtered_data <- data %>%
  filter(ScheduleDepartureTime >= start_date & ScheduleDepartureTime <= end_date)


write.csv(filtered_data, file = "C:\\Users\\a0937\\OneDrive\\桌面\\flightTW\\row_data\\4399.csv", row.names = FALSE)



# 抓特定日期
start_date <- as.POSIXct("2024-08-12 00:00:00", format="%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2024-08-17 23:59:59", format="%Y-%m-%d %H:%M:%S")

# 過濾指定日期範圍內的數據
filtered_data <- data %>%
  filter(ScheduleDepartureTime >= start_date & ScheduleDepartureTime <= end_date)

用信用卡支付8成以上的團費或公共運輸工具票款的話，就會贈送旅遊不便險。當遇到班機延誤、行李損失及旅行文件損失等情況時，都會給予賠償，不過，不便險的賠付保險金額依各家保險公司、投保金額、班機延誤時數而有不同，相關合約細節及理賠範圍還是和投保公司確認比較保險喔。
當國內外航班延誤超過5小時時，旅客若不接受航空公司安排，可選擇全額退票且航空公司不能收取手續費。如果你搭乘的是歐盟成員國的航空公司或是歐盟境內出發的航班，根據歐盟條例（EC） No261/2004，只要你符合其中一個條件，從任何地方出發的航班延誤超過3小時，你就有權向航空公司索取賠償。
https://www.fubon.com/insurance/blog/lifestyle/cancel-flights.html
如果最晚延誤到 8 小時，依照「每 4 小時定額給付班機延誤保險金」的規定，就會給付到 2 倍的班機延誤保險金。且單趟班機（不論去程或是回程）一次最高理賠金額可到 2 倍。
https://blog.finfo.tw/insurance-travel/%E7%8F%AD%E6%A9%9F%E5%BB%B6%E8%AA%A4%E6%80%8E%E9%BA%BC%E8%BE%A6%EF%BC%9F%E8%99%95%E7%90%86%E5%AE%88%E5%89%87%E3%80%81%E7%94%B3%E8%AB%8B%E6%96%87%E4%BB%B6%E7%9C%8B%E9%80%99%E4%BA%9B
託運行李沒有同時到達機場，根據條款規定，要是人在抵達目的地 6 小時後仍無法取得託運行李，保險公司即理賠。每發生一次給的會是固定的金額。




# 桃園機場最熱門的出國地點
airport <- read.csv("C:/Users/YU/Desktop/project/CSV/airport.csv")
TPE_data <- data[data$DepartureAirportID == "TPE",]
airportTPE <- merge(TPE_data, airport, by.x = "ArrivalAirportID", by.y = "AirportID")
count_TPEflights <- sort(table(airportTPE$AirportNationality), decreasing = TRUE)
TPEhotspot <- as.data.frame(count_TPEflights)
colnames(TPEhotspot) <- c("Nation", "Flightcount")
# 畫長條圖
ggplot(TPEhotspot, aes(x = Nation, y = Flightcount)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  
  geom_text(aes(label = Flightcount), vjust = -0.5, color = "black", size = 3) +  
  scale_y_continuous(limits = c(0, 1200)) +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(title = "Number of Flights from TPE to Each National", 
       x = "Arrival Nation", 
       y = "Number of Flights")

# 桃園機場單一航空公司延遲航班的時間序列
TPE_BR <- delaydata[delaydata$DepartureAirportID == "TPE" & delaydata$AirlineID == "BR",]

ts()

airline <- read.csv("C:/Users/YU/Desktop/project/CSV/airline.csv")





