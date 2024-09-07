
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

library(tseries)
library(dplyr)
library(tseries)    # Jarque-Bera 检验
library(forecast)   # 单位根检验（ERS）
library(moments)    # 偏度、峰度
library(FinTS)      # Q检验
library(urca)       # ERS 单位根检验

library(parallel)
library(ConnectednessApproach)
library(vars)
library(ggplot2)






data <- read_csv("cleaned_data.csv")
View(data)
#初步分析----
# 计算大宗商品价格变化率的描述性统计量
# 转换为日期格式
data$Date <- as.Date(data$Date)

# 查看数据结构
str(data)

# 对需要的列进行描述性统计分析
# 提取要分析的列
variables <- data %>%
  select(Silver_Change, Copper_Change, WTI_Change, Brent_Change, Gold_Change, 
         USD_Change, Gas_Change, Wheat_Change, Maize_Change, Soyabeans_Change, GPR_Change)


## 1. 描述性统计分析（均值、方差、偏度、峰度）----
summary_stats <- summary(variables)
print(summary_stats)

detailed_stats <- describe(variables)
print(detailed_stats)




## 2.创建一个函数来计算所有统计量及其显著性----
compute_stats_with_significance <- function(variable) {
  mean_value <- mean(variable, na.rm = TRUE)
  variance_value <- var(variable, na.rm = TRUE)
  skewness_value <- skewness(variable, na.rm = TRUE)
  kurtosis_value <- kurtosis(variable, na.rm = TRUE)
  
  # 均值显著性检验 (t检验)
  mean_t_test <- t.test(variable, mu = 0)
  
  # 偏度和峰度显著性检验 (标准误差)
  n <- length(variable[!is.na(variable)])
  skewness_se <- sqrt(6/n)
  kurtosis_se <- sqrt(24/n)
  
  skewness_significance <- skewness_value / skewness_se
  kurtosis_significance <- kurtosis_value / kurtosis_se
  
  # Jarque-Bera 正态性检验
  #Jarque-Bera 检验用于检验数据是否符合正态分布。p 值小于显著水平（通常为 0.05）意味着拒绝正态性假设。
  jb_test <- jarque.bera.test(variable)
  
  # ERS 单位根检验
  #ERS 单位根检验用于检测数据是否存在单位根（即是否平稳）。负的统计量和临界值（ERS_stat < ERS_critical_value）表明拒绝单位根假设，说明该序列是平稳的。
  ers_test <- ur.ers(variable)
  
  # Q(20) Ljung-Box 自相关检验
  #Q(20) 检验用于检查自相关性，特别是延迟 20 的自相关性。p 值小于 0.05 表示存在显著的自相关性。
  #大多数变量的 Q(20) 检验 p 值接近 0，说明这些变量存在显著的自相关性，即历史的价格变化与未来的变化存在一定的关联性。
  q_test <- Box.test(variable, lag = 20, type = "Ljung-Box")
  
  # Q²(20) 对残差平方的 Ljung-Box 检验
  residual_squared <- variable^2
  q2_test <- Box.test(residual_squared, lag = 20, type = "Ljung-Box")
  
  # 返回所有统计量和显著性作为一个列表
  return(c(mean_value, mean_t_test$p.value, variance_value, skewness_value, skewness_significance, 
           kurtosis_value, kurtosis_significance, jb_test$statistic, jb_test$p.value, 
           ers_test@teststat, ers_test@cval[1], q_test$statistic, q_test$p.value, 
           q2_test$statistic, q2_test$p.value))
}

# 对所有变量进行统计量及显著性检验计算，并汇总到表格
stats_results <- apply(variables, 2, compute_stats_with_significance)

# 转置为数据框并添加列名
stats_results <- as.data.frame(t(stats_results))
colnames(stats_results) <- c("Mean", "Mean_pvalue", "Variance", "Skewness", "Skewness_Significance", 
                             "Kurtosis", "Kurtosis_Significance", "JB_stat", "JB_pvalue", 
                             "ERS_stat", "ERS_critical_value", "Q_stat", "Q_pvalue", 
                             "Q2_stat", "Q2_pvalue")

# 添加变量名称作为行名
rownames(stats_results) <- colnames(variables)

# 打印汇总后的表格
View(stats_results)

## 3.相关系数矩阵----
# 计算相关系数矩阵
correlation_matrix <- cor(variables, use = "complete.obs")

# 打印相关系数矩阵
correlation_matrix

#要更漂亮的表格，可以使用 corrplot 包来可视化

library(corrplot)

# 可视化相关系数矩阵
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8)


#进一步分析----
## 4.TVP-VAR----

# 导入数据（确保是日度数据）

DATA <- data[,-1]
str(DATA[, 1])
head(DATA[, 1])
DATE <- DATA[, 1] 
Y <- DATA[, -1]
data1 <- data.frame(Date = DATE, Y)
data2 <- read.zoo(data1, format = "%Y-%m-%d")


# 设置变量名称
NAMES <- c("WTI", "Brent", "Gold", "USD", "Gas", "Wheat", "Maize", "Soyabeans", "GPR")
colnames(Y) <- NAMES

#
# 描述性统计分析----
desc_stats <- data.frame(
  Variable = NAMES,
  Mean = sapply(Y, mean, na.rm = TRUE),
  Median = sapply(Y, median, na.rm = TRUE),
  SD = sapply(Y, sd, na.rm = TRUE),
  Min = sapply(Y, min, na.rm = TRUE),
  Max = sapply(Y, max, na.rm = TRUE)
)

print(desc_stats)

### 绘制每个变量的图表----
plot_list <- lapply(variables, function(var_name) {
  ggplot(data, aes_string(x = "Date", y = var_name)) +
    geom_line() +
    ggtitle(paste("Time Series Plot of", var_name)) +
    xlab("Date") +
    ylab(var_name) +
    theme_minimal()
})

for (plot in plot_list) {
  print(plot)
}
dev.off()
print(plot_list[[1]])

# 选择滞后期数（尝试不同滞后期数以找到最佳设置）----
lag_selection <- VARselect(Y, lag.max = 20, type = "const")
print(lag_selection$selection)
print(lag_selection$criteria)
## TVP-VAR模型----

# 定义频率区间
partition = c(pi + 0.00001, pi / 5, 0)

# 执行 TVP-VAR 连接性分析
dca <- ConnectednessApproach(
  x = data2, 
  nlag = 5,
  nfore = 100,
  model = "TVP-VAR",
  connectedness = "Frequency",
  VAR_config = list(
    TVPVAR = list(
      kappa1 = 0.99, 
      kappa2 = 0.99, 
      prior = "BayesPrior"
    )
  ),
  Connectedness_config = list(
    FrequencyConnectedness = list(
      partition = partition, 
      generalized = TRUE, 
      scenario = "ABS"
    )
  )
)

# 打印连接性表格
print(dca$TABLE)

# 绘制图形
PlotTCI(dca)
PlotNET(dca)
PlotTO(dca)
PlotFROM(dca)
PlotNPDC(dca)




