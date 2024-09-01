
library(xlsx)
library(plotrix)
library(RColorBrewer)
library(ggcorrplot)

# 读取CPI增长率数据，并以1994年12月为基进行转化
CPI_growth <- read.xlsx('./data/CPI环比增长.xlsx', sheetIndex = 1)
CPI <- c()
CPI[1] <- 100*(1 + CPI_growth[1,2]/100)
for(i in 2:300){
  CPI[i] <- CPI[i-1]*(1 + CPI_growth[i,2]/100)
}
CPI_data <- data.frame(CPI, CPI_growth)

# 读取利率数据，并转化为月利率，将单位设为1，并用CPI修正
Int_rate <- read.xlsx('./data/利率.xlsx', sheetIndex = 1)
Risk_free <- Int_rate[,2]/1200
Int_data <- data.frame(Risk_free, Int_rate)
Risk_free_real <- (rep(1,300) + Risk_free)/(rep(1,300) + CPI_data[,3]/100) - rep(1,300)

# 读取人口数据，并根据几何平均求月增速，转化为每月人口
Popu <- read.xlsx('./data/总人口.xlsx', sheetIndex = 1)
Popu_year <- Popu[2:26, 2]
Popu_growth_year <- (Popu[2:26, 2] - Popu[1:25, 2])/Popu[1:25, 2]
Popu_growth_month <- (rep(1,25) + Popu_growth_year)^(1/12) - rep(1,25)
Popu_month <- c()
Popu_month[1] <- Popu[1,2]*(1+Popu_growth_month[1])
for(i in 2:300){
  m <- ceiling(i/12)
  Popu_month[i] <- Popu_month[i-1]*(1+Popu_growth_month[m])
}

# 读取销售总额数据，对部分年份1、2月进行缺失值填补
Retail_raw <- read.csv('./data/零售总额.csv', header = FALSE)
Retail_raw <- t(Retail_raw[,2:305])
Retail_raw <- data.frame(Retail_raw)
names(Retail_raw) <- c('Month', 'Current', 'Cumulative')
Retail_raw$Current <- as.numeric(Retail_raw$Current)
Retail_raw$Cumulative <- as.numeric(Retail_raw$Cumulative)
Retail_current <- Retail_raw[1:292, 2]
Retail_current_na <- is.na(Retail_current)
Retail_Cumulative <- Retail_raw[Retail_current_na, 3][1:8]
Retail_current_corr <- c()
k <- 0
for(i in 1:292){
  if(Retail_current_na[i]==TRUE){
    Retail_current_corr[i+k] <- Retail_Cumulative[k+1]/2
    Retail_current_corr[i+k+1] <- Retail_Cumulative[k+1]/2
    k <- k + 1
  }
  else{
    Retail_current_corr[i+k] <- Retail_current[i]
  }
}
Retail_current_corr <- rev(Retail_current_corr)
Retail_current_corr <- Retail_current_corr / CPI * 100
Retail_current_avg <- Retail_current_corr / Popu_month # 根据人口进行调整
Retail_growth <- Retail_current_avg[2:300] / Retail_current_avg[1:299] # 消费增长率

# 读取上证综指和深证成指收盘价数据，并用CPI修正，计算收益率
IDX_raw <- read.xlsx('./data/IDX.xlsx', sheetIndex = 1)
sh_clo <- IDX_raw[1:301, 8] # 上证综合指数收盘价
sz_clo <- IDX_raw[302:602, 8] # 深证成分指数收盘价
sh_clo <- sh_clo / c(1, CPI/100) # 上证修正
sz_clo <- sz_clo / c(1, CPI/100) # 深证修正
sh_re <- (sh_clo[2:301] - sh_clo[1:300]) / sh_clo[1:300]
sz_re <- (sz_clo[2:301] - sz_clo[1:300]) / sz_clo[1:300]
# 读取上证50和沪深300
IDX_raw_2 <- read.xlsx('./data/IDX_2.xlsx', sheetIndex = 1)
sz50 <- IDX_raw[1:180, 8]
hs300 <- IDX_raw[181:360, 8]
sz50 <- sz50 / (CPI[121:300]/100) # 上证修正
hs300 <- hs300 / (CPI[121:300]/100) # 深证修正
sz50_re <- (sz50[2:180] - sz50[1:179]) / sz50[1:179]
hs300_re <- (hs300[2:180] - hs300[1:179]) / hs300[1:179]

# 描述性统计
summary(Retail_current_avg)
summary(Retail_growth)
summary(Risk_free_real)
summary(sh_re)
summary(sz_re)
summary(sz50_re)
summary(hs300_re)
sd(Retail_current_avg)
sd(Retail_growth)
sd(Risk_free_real)
sd(sh_re)
sd(sz_re)
sd(sz50_re)
sd(hs300_re)

# 探索性数据分析
# 零售趋势图
Retail_current_avg_ts <- ts(Retail_current_avg, frequency=12, start=c(1995,1))
plot(Retail_current_avg_ts, type = 'l', lty = 1,, col = 4, lwd = 2,
     ylab = 'Consumption of representative consumer', xlab = 'Time')
components <- decompose(Retail_current_avg_ts)
plot(components, col = 4, lwd = 2)
# 人口趋势图
twoord.plot(lx=c(1995:2019), ly=Popu_year,rx=c(1995:2019),ry=Popu_growth_year,
            xlab='Year', ylab='Population', rylab='Growth rate', type=c('bar','l'),
            lcol='steelblue3', lwd = 3)
# CPI趋势图
twoord.plot(lx=c(1:300), CPI, rx = c(1:300), ry=CPI_growth[,2]/100,
            xlab='Time', ylab='CPI', rylab='Growth rate', type=c('l','l'),
            lcol='#2166AC', rcol = '#D6604D', lwd = 3)
# 利率趋势图
Int_rate_ts <- ts(Int_rate[,2]/100, frequency=12, start=c(1995,1))
plot(Int_rate_ts, type = 's', ylab = 'Interest rate', col=4, lwd = 2)
# 指数趋势图
sh_cl_ts <- ts(IDX_raw[2:301, 8], frequency=12, start=c(1995,1))
sz_cl_ts <- ts(IDX_raw[303:602, 8], frequency=12, start=c(1995,1))
sh_op_ts <- ts(IDX_raw[2:301, 5], frequency=12, start=c(1995,1))
sz_op_ts <- ts(IDX_raw[303:602, 5], frequency=12, start=c(1995,1))
sz50_cl_ts <- ts(IDX_raw_2[1:180, 8], frequency=12, start=c(2005,1))
sz50_op_ts <- ts(IDX_raw_2[1:180, 5], frequency=12, start=c(2005,1))
hs300_cl_ts <- ts(IDX_raw_2[181:360, 8], frequency=12, start=c(2005,1))
hs300_op_ts <- ts(IDX_raw_2[181:360, 5], frequency=12, start=c(2005,1))
par(mfrow=c(2,2))
plot(sh_cl_ts, type = 'l', col = 2, lwd = 2, ylab = 'Shanghai Composite Index')
lines(sh_op_ts, lty = 2, col = 4, lwd = 2)
legend('bottomright', col = c(2,4), lwd = 2, lty = c(1,2),
       legend = c('Open', 'Close'))
plot(sz_cl_ts, type = 'l', col = 2, lwd = 2, ylab = 'Shenzhen Component Index')
lines(sz_op_ts, lty = 2, col = 4, lwd = 2)
legend('bottomright', col = c(2,4), lwd = 2, lty = c(1,2),
       legend = c('Open', 'Close'))
plot(sz50_cl_ts, type = 'l', col = 2, lwd = 2, ylab = 'SSE 50 Index')
lines(sz50_op_ts, lty = 2, col = 4, lwd = 2)
legend('bottomright', col = c(2,4), lwd = 2, lty = c(1,2),
       legend = c('Open', 'Close'))
plot(hs300_cl_ts, type = 'l', col = 2, lwd = 2, ylab = 'CSI 300 Index')
lines(hs300_op_ts, lty = 2, col = 4, lwd = 2)
legend('bottomright', col = c(2,4), lwd = 2, lty = c(1,2),
       legend = c('Open', 'Close'))
dev.off()

# 相关热力图
mydata <- data.frame(CPI[122:300], CPI_growth[122:300,2],
                     Popu_month[122:300], Retail_current_avg[122:300],
                     Risk_free_real[122:300], sh_re[122:300], 
                     sz_re[122:300],sz50_re, hs300_re)
names(mydata) <- c('CPI', 'CPI_growth', 'Population', 'Consumption', 'Rate', 'SH', 'SZ', 'SSE', 'CSI')
res <- cor(mydata)
p.mat <- cor_pmat(mydata)
ggcorrplot(res, hc.order = FALSE, type = "lower", p.mat = p.mat)

# 效用函数
Ut <- function(C, alpha){
  y <- (C^(1-alpha)-1) / (1-alpha)
  return(y)
}

dUt <- function(C, alpha){
  y <- C^(-alpha)
  return(y)
}

# 风险延误系数
alpha <- c(1:100)/10

# 计算HJ下界
omega <- var(data.frame(sh_re, sz_re))
varM_hj <- function(M){
  x <- rep(1,2)-M*c(mean(sh_re)+1, mean(sz_re)+1)
  y <- t(x)%*%solve(omega)%*%x
  return(y)
}

EM_hj <- c(9000:11000)/10000
sd_hj <- c()
for(i in 1:2001){
  sd_hj[i] <- sqrt(varM_hj(EM_hj[i]))
}

# 改变折扣因子
get_result <- function(beta){
  EM <- c()
  sdM <- c()
  for(i in 1:100){
    EM[i] <- beta*mean(Retail_growth^(-alpha[i]))
    sdM[i] <- beta*sd(Retail_growth^(-alpha[i]))
  }
  return(list(EM, sdM))
}

EM_93 <- get_result(0.93)[[1]]
sdM_93 <- get_result(0.93)[[2]]
EM_94 <- get_result(0.94)[[1]]
sdM_94 <- get_result(0.94)[[2]]
EM_95 <- get_result(0.95)[[1]]
sdM_95 <- get_result(0.95)[[2]]
EM_96 <- get_result(0.96)[[1]]
sdM_96 <- get_result(0.96)[[2]]
EM_97 <- get_result(0.97)[[1]]
sdM_97 <- get_result(0.97)[[2]]
EM_98 <- get_result(0.98)[[1]]
sdM_98 <- get_result(0.98)[[2]]
EM_99 <- get_result(0.99)[[1]]
sdM_99 <- get_result(0.99)[[2]]
EM_100 <- get_result(1)[[1]]
sdM_100 <- get_result(1)[[2]]

plot(EM_hj, sd_hj, type = 'l', xlab = 'E(M)', ylab = 'sd(M)', lwd = 2)
lines(EM_93, sdM_93, type = 'p', pch = 6, col = 2)
lines(EM_94, sdM_94, type = 'p', pch = 7, col = 3)
lines(EM_95, sdM_95, type = 'p', pch = 8, col = 4)
lines(EM_96, sdM_96, type = 'p', pch = 9, col = 5)
lines(EM_97, sdM_97, type = 'p', pch = 10, col = 6)
lines(EM_98, sdM_98, type = 'p', pch = 11, col = 7)
lines(EM_99, sdM_99, type = 'p', pch = 12, col = 8)
lines(EM_100, sdM_100, type = 'p', pch = 13, col = 9)
legend('top', pch = c(6:13), col = c(2:9), text.width=0.02, ncol = 2,
       legend = c(expression(paste(beta, " = 0.93")),expression(paste(beta, " = 0.94")), 
                  expression(paste(beta, " = 0.95")),expression(paste(beta, " = 0.96")), 
                  expression(paste(beta, " = 0.97")), expression(paste(beta, " = 0.98")),
                  expression(paste(beta, " = 0.99")), expression(paste(beta, " = 1.00"))))

# 考虑从2005年1月开始，加入更多指数
omega_2 <- var(data.frame(sh_re[122:300], sz_re[122:300], sz50_re, hs300_re))
varM_hj_2 <- function(M){
  x <- rep(1,4)-M*c(mean(sh_re[122:300])+1, mean(sz_re[122:300])+1, 
                    mean(sz50_re)+1, mean(hs300_re)+1)
  y <- t(x)%*%solve(omega_2)%*%x
  return(y)
}
EM_hj_2 <- c(9000:11000)/10000
sd_hj_2 <- c()
for(i in 1:2001){
  sd_hj_2[i] <- sqrt(varM_hj_2(EM_hj_2[i]))
}

get_result_2 <- function(beta){
  EM <- c()
  sdM <- c()
  for(i in 1:100){
    EM[i] <- beta*mean(Retail_growth[121:299]^(-alpha[i]))
    sdM[i] <- beta*sd(Retail_growth[121:299]^(-alpha[i]))
  }
  return(list(EM, sdM))
}

EM_93_2 <- get_result_2(0.93)[[1]]
sdM_93_2 <- get_result_2(0.93)[[2]]
EM_94_2 <- get_result_2(0.94)[[1]]
sdM_94_2 <- get_result_2(0.94)[[2]]
EM_95_2 <- get_result_2(0.95)[[1]]
sdM_95_2 <- get_result_2(0.95)[[2]]
EM_96_2 <- get_result_2(0.96)[[1]]
sdM_96_2 <- get_result_2(0.96)[[2]]
EM_97_2 <- get_result_2(0.97)[[1]]
sdM_97_2 <- get_result_2(0.97)[[2]]
EM_98_2 <- get_result_2(0.98)[[1]]
sdM_98_2 <- get_result_2(0.98)[[2]]
EM_99_2 <- get_result_2(0.99)[[1]]
sdM_99_2 <- get_result_2(0.99)[[2]]
EM_100_2 <- get_result_2(1)[[1]]
sdM_100_2 <- get_result_2(1)[[2]]

plot(EM_hj_2, sd_hj_2, type = 'l', xlab = 'E(M)', ylab = 'sd(M)', lwd = 2)
lines(EM_93_2, sdM_93_2, type = 'p', pch = 6, col = 2)
lines(EM_94_2, sdM_94_2, type = 'p', pch = 7, col = 3)
lines(EM_95_2, sdM_95_2, type = 'p', pch = 8, col = 4)
lines(EM_96_2, sdM_96_2, type = 'p', pch = 9, col = 5)
lines(EM_97_2, sdM_97_2, type = 'p', pch = 10, col = 6)
lines(EM_98_2, sdM_98_2, type = 'p', pch = 11, col = 7)
lines(EM_99_2, sdM_99_2, type = 'p', pch = 12, col = 8)
lines(EM_100_2, sdM_100_2, type = 'p', pch = 13, col = 9)
legend('top', pch = c(6:13), col = c(2:9), text.width=0.02, ncol = 2,
       legend = c(expression(paste(beta, " = 0.93")),expression(paste(beta, " = 0.94")), 
                  expression(paste(beta, " = 0.95")),expression(paste(beta, " = 0.96")), 
                  expression(paste(beta, " = 0.97")), expression(paste(beta, " = 0.98")),
                  expression(paste(beta, " = 0.99")), expression(paste(beta, " = 1.00"))))

# 求取曲线交点
get_point <- function(EM1, sdM1, EM2, sdM2){
  n1 <- length(EM1)
  n2 <- length(EM2)
  for(i in 1:n1){
    for( j in 1:n2){
      if(EM2[j]>=EM1[i]){
        break
      }
    }
    if(sdM1[i]>=sdM2[j]){
      break
    }
  }
  return(alpha[i])
}

get_point(EM_93, sdM_93, EM_hj, sd_hj)
get_point(EM_94, sdM_94, EM_hj, sd_hj)
get_point(EM_95, sdM_95, EM_hj, sd_hj)
get_point(EM_96, sdM_96, EM_hj, sd_hj)
get_point(EM_97, sdM_97, EM_hj, sd_hj)
get_point(EM_98, sdM_98, EM_hj, sd_hj)
get_point(EM_99, sdM_99, EM_hj, sd_hj)
get_point(EM_100, sdM_100, EM_hj, sd_hj)

get_point(EM_93_2, sdM_93_2, EM_hj_2, sd_hj_2)
get_point(EM_94_2, sdM_94_2, EM_hj_2, sd_hj_2)
get_point(EM_95_2, sdM_95_2, EM_hj_2, sd_hj_2)
get_point(EM_96_2, sdM_96_2, EM_hj_2, sd_hj_2)
get_point(EM_97_2, sdM_97_2, EM_hj_2, sd_hj_2)
get_point(EM_98_2, sdM_98_2, EM_hj_2, sd_hj_2)
get_point(EM_99_2, sdM_99_2, EM_hj_2, sd_hj_2)
get_point(EM_100_2, sdM_100_2, EM_hj_2, sd_hj_2)

# 检验无风险收益率之谜
-log(0.93) + 7.1*mean(log(Retail_growth)) - 7.1^2/2*var(log(Retail_growth))
-log(0.94) + 6.7*mean(log(Retail_growth)) - 6.7^2/2*var(log(Retail_growth))
-log(0.95) + 6.2*mean(log(Retail_growth)) - 6.2^2/2*var(log(Retail_growth))
-log(0.96) + 5.7*mean(log(Retail_growth)) - 5.7^2/2*var(log(Retail_growth))
-log(0.97) + 5.1*mean(log(Retail_growth)) - 5.1^2/2*var(log(Retail_growth))
-log(0.98) + 4.3*mean(log(Retail_growth)) - 4.3^2/2*var(log(Retail_growth))
-log(0.99) + 3.4*mean(log(Retail_growth)) - 3.4^2/2*var(log(Retail_growth))
-log(1.00) + 1.5*mean(log(Retail_growth)) - 1.5^2/2*var(log(Retail_growth))
max(Risk_free_real)

-log(0.93) + 10*mean(log(Retail_growth[121:299])) - 
  10^2/2*var(log(Retail_growth[121:299]))
-log(0.94) + 9.8*mean(log(Retail_growth[121:299])) - 
  9.8^2/2*var(log(Retail_growth[121:299]))
-log(0.95) + 9.3*mean(log(Retail_growth[121:299])) - 
  9.3^2/2*var(log(Retail_growth[121:299]))
-log(0.96) + 8.7*mean(log(Retail_growth[121:299])) - 
  8.7^2/2*var(log(Retail_growth[121:299]))
-log(0.97) + 8*mean(log(Retail_growth[121:299])) - 
  8^2/2*var(log(Retail_growth[121:299]))
-log(0.98) + 7.2*mean(log(Retail_growth[121:299])) - 
  7.2^2/2*var(log(Retail_growth[121:299]))
-log(0.99) + 6.3*mean(log(Retail_growth[121:299])) - 
  6.3^2/2*var(log(Retail_growth[121:299]))
-log(1.00) + 5.1*mean(log(Retail_growth[121:299])) - 
  5.1^2/2*var(log(Retail_growth[121:299]))
max(Risk_free_real[121:300])
