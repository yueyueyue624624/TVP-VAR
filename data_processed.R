# 设置工作目录
rm(list=ls())
setwd("E:/00毕业毕业论文")

# 加载必要的R包----
library(tidyverse)
library(readxl)
library(zoo)
library(purrr)
library(tidyverse)
library(readxl)
library(lubridate)
library(moments)
library(tseries)
# 加载 CSV 数据
brent_data <- read_csv("Brent Oil.csv")
usd_data <- read_csv("US Dollar Index.csv")
copper_data <- read_csv("Copper.csv")
silver_data <- read_csv("Silver.csv")
gas_data <- read_csv("Natural Gas.csv")
wti_data <- read_csv("Crude Oil WTI.csv")
gold_data <- read_csv("Gold.csv")

# 加载 Excel 数据
gpr_data <- read_excel("GPR.xlsx")
wheat_data <- read_excel("wheat.xlsx")
maize_data <- read_excel("maize.xlsx")
soyabeans_data <- read_excel("soyabeans.xlsx")


# 检查数据框的列名和前几行数据
head(silver_data)
head(copper_data)
head(wti_data)
head(brent_data)
head(gold_data)
head(usd_data)
head(gas_data)
head(wheat_data)
head(maize_data)
head(soyabeans_data)
head(gpr_data)

#CHANGE 为对数收益率
#有缺失值、
# 修改所有数据框，统一保留 Date 和 Change 列

silver_data <- silver_data %>% select(Date, Change)
copper_data <- copper_data %>% select(Date,  Change)
wti_data <- wti_data %>% select(Date, Change)
brent_data <- brent_data %>% select(Date, Change)
gold_data <- gold_data %>% select(Date,  Change)
usd_data <- usd_data %>% select(Date,  Change)
gas_data <- gas_data %>% select(Date,  Change)

# 确保日期格式正确
clean_date <- function(df) {
  df <- df %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  return(df)
}


silver_data <- clean_date(silver_data)
copper_data <- clean_date(copper_data)
wti_data <- clean_date(wti_data)
brent_data <- clean_date(brent_data)
gold_data <- clean_date(gold_data)
usd_data <-  clean_date(usd_data) 
gas_data <- clean_date(gas_data)

# 计算变化率并保留日期和变化率列
calculate_change <- function(df, value_col) {
  df <- df %>%
    arrange(Date) %>%
    mutate(Change = log(get(value_col) / lag(get(value_col)))) %>%
    select(Date, Change)
  return(df)
}

wheat_data <- calculate_change(wheat_data, "wheat")
maize_data <- calculate_change(maize_data, "maize")
soyabeans_data <- calculate_change(soyabeans_data, "soyabeans")
gpr_data <- calculate_change(gpr_data, "GPRD")

# 检查处理后的数据
head(wti_data)
head(brent_data)
head(gold_data)
head(usd_data)
head(gas_data)
head(wheat_data)
head(maize_data)
head(soyabeans_data)
head(gpr_data)
head(silver_data)
head(copper_data)
# 过滤日期范围
filter_date_range <- function(df) {
  df <- df %>%
    filter(Date >= as.Date("2000-01-01") & Date <= as.Date("2024-08-31"))
  return(df)
}
silver_data<- filter_date_range(silver_data)
copper_data<-filter_date_range(copper_data)
wti_data <- filter_date_range(wti_data)
brent_data <- filter_date_range(brent_data)
gold_data <- filter_date_range(gold_data)
usd_data <- filter_date_range(usd_data)
gas_data <- filter_date_range(gas_data)
wheat_data <- filter_date_range(wheat_data)
maize_data <- filter_date_range(maize_data)
soyabeans_data <- filter_date_range(soyabeans_data)
gpr_data <- filter_date_range(gpr_data)

# 检查过滤后的数据
head(silver_data)
head(copper_data)
head(wti_data)
head(brent_data)
head(gold_data)
head(usd_data)
head(gas_data)
head(wheat_data)
head(maize_data)
head(soyabeans_data)
head(gpr_data)



# 为每个数据框添加前缀
add_prefix <- function(df, prefix) {
  names(df)[-1] <- paste0(prefix, "_", names(df)[-1])
  return(df)
}

silver_data <- add_prefix(silver_data, "Silver")
copper_data <- add_prefix(copper_data, "Copper")
wti_data <- add_prefix(wti_data, "WTI")
brent_data <- add_prefix(brent_data, "Brent")
gold_data <- add_prefix(gold_data, "Gold")
usd_data <- add_prefix(usd_data, "USD")
gas_data <- add_prefix(gas_data, "Gas")
wheat_data <- add_prefix(wheat_data, "Wheat")
maize_data <- add_prefix(maize_data, "Maize")
soyabeans_data <- add_prefix(soyabeans_data, "Soyabeans")
gpr_data <- add_prefix(gpr_data, "GPR")



# 合并所有数据集
data_list <- list(silver_data, copper_data, wti_data, brent_data, gold_data, usd_data, gas_data, wheat_data, maize_data, soyabeans_data, gpr_data)

# 按照 Date 合并所有数据集，并删除 NA
merged_data <- purrr::reduce(data_list, left_join, by = "Date")

# 删除包含 NA 的行
cleaned_data <- na.omit(merged_data)




# 将百分比格式的字符列转换为数值
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), ~ as.numeric(gsub("%", "", .)) / 100)) %>%
  # 将所有数值列格式化为两位小数
  mutate(across(where(is.numeric), round, 4))

# 查看合并后的数据
View(cleaned_data)
#write.csv(cleaned_data,file="cleaned_data.csv")
